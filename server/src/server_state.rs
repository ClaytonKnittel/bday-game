use std::{collections::HashMap, iter::once, sync::Arc};

use common::{
  crossword::Crossword,
  msg::{write_message_to_wire, ClientMessage, ServerMessage},
  util::AsyncWriteT,
};
use tokio::sync::Mutex;
use util::{
  error::{TermgameError, TermgameResult},
  pos::Pos,
};

use crate::client_context::{AuthenticatedLiveClient, ClientContext, LiveClient};

enum Action {
  Respond(ServerMessage),
  Broadcast(ServerMessage),
  Ignore,
}

pub struct ServerState<W> {
  crossword: Crossword,
  clients: HashMap<u64, ClientContext<W>>,
  next_uid: u64,
}

impl<W> ServerState<W>
where
  W: AsyncWriteT,
{
  pub fn with_crossword(crossword: Crossword) -> Self {
    Self {
      crossword,
      clients: HashMap::new(),
      next_uid: 0,
    }
  }

  fn assign_new_uid(&mut self) -> u64 {
    let new_uid = self.next_uid;
    self.next_uid += 1;
    new_uid
  }

  fn all_connections_mut(&mut self) -> impl Iterator<Item = (&u64, &mut ClientContext<W>)> {
    self.clients.iter_mut()
  }

  fn live_connections_mut(&mut self) -> impl Iterator<Item = (u64, &mut LiveClient<W>)> {
    self
      .clients
      .iter_mut()
      .filter_map(|(&id, context)| context.as_live_mut().map(|client| (id, client)))
  }

  async fn execute_actions(
    &mut self,
    stream: Arc<Mutex<W>>,
    actions: impl Iterator<Item = Action>,
  ) -> TermgameResult {
    for action in actions {
      match action {
        Action::Respond(message) => {
          write_message_to_wire(&mut *stream.lock().await, message).await?
        }
        Action::Broadcast(message) => {
          for (_, context) in self.live_connections_mut() {
            context.write_message(message.clone()).await?;
          }
        }
        Action::Ignore => {}
      }
    }

    Ok(())
  }

  async fn to_authenticated_mut(
    &mut self,
    stream: &Arc<Mutex<W>>,
    uid: u64,
  ) -> TermgameResult<AuthenticatedLiveClient<'_, W>> {
    if let Some(state) = self.clients.get_mut(&uid) {
      if let Some(live_state) = state.as_live_mut() {
        if let Some(auth_client) = live_state.to_authenticated_mut(&stream).await {
          Ok(auth_client)
        } else {
          Err(TermgameError::Internal(format!("Cannot authenticate as client {uid}")).into())
        }
      } else {
        Err(TermgameError::Internal(format!("Client {uid} is not live")).into())
      }
    } else {
      Err(TermgameError::Internal(format!("No such client with uid {uid}")).into())
    }
  }

  async fn to_authenticated_context_mut(
    &mut self,
    stream: &Arc<Mutex<W>>,
    uid: u64,
  ) -> TermgameResult<&mut ClientContext<W>> {
    self.to_authenticated_mut(stream, uid).await?;
    self
      .clients
      .get_mut(&uid)
      .ok_or_else(|| TermgameError::Internal(format!("No such client with uid {uid}")).into())
  }

  async fn new_connection(
    &mut self,
    stream: Arc<Mutex<W>>,
  ) -> TermgameResult<impl Iterator<Item = Action>> {
    let new_uid = self.assign_new_uid();

    let client_ctx = ClientContext::new(stream);
    let old_val = self.clients.insert(new_uid, client_ctx);
    debug_assert!(old_val.is_none());

    Ok(once(Action::Respond(ServerMessage::NewConnection {
      uid: new_uid,
    })))
  }

  async fn connect_to_existing(
    &mut self,
    stream: Arc<Mutex<W>>,
    uid: u64,
  ) -> TermgameResult<impl Iterator<Item = Action>> {
    let success = if let Some(state) = self.clients.get_mut(&uid) {
      state.make_live(stream)
    } else {
      return Err(TermgameError::Internal(format!("No such client with uid {uid}")).into());
    };

    Ok(once(Action::Respond(ServerMessage::ConnectToExisting {
      success,
    })))
  }

  async fn position_update(
    &mut self,
    stream: Arc<Mutex<W>>,
    uid: u64,
    pos: Pos,
  ) -> TermgameResult<impl Iterator<Item = Action>> {
    let context = self.to_authenticated_context_mut(&stream, uid).await?;
    context.player_info_mut().pos = pos;
    Ok(once(Action::Broadcast(
      ServerMessage::PlayerPositionUpdate { uid, pos },
    )))
  }

  pub async fn respond_to_message(
    &mut self,
    stream: Arc<Mutex<W>>,
    message: ClientMessage,
  ) -> TermgameResult {
    println!("Message: {message:?}");

    macro_rules! execute {
      ($handler:expr) => {{
        let actions = $handler;
        self.execute_actions(stream, actions).await
      }};
    }

    match message {
      ClientMessage::NewConnection => {
        execute!(self.new_connection(stream.clone()).await?)
      }
      ClientMessage::ConnectToExisting { uid } => {
        execute!(self.connect_to_existing(stream.clone(), uid).await?)
      }
      ClientMessage::PositionUpdate { uid, pos } => {
        execute!(self.position_update(stream.clone(), uid, pos).await?)
      }
    }
  }

  pub async fn cleanup_dead_clients(&mut self) {
    for (uid, client) in self.all_connections_mut() {
      if let Some(live_client) = client.as_live_mut() {
        if !live_client.tcp_writeable().await {
          println!("Client {uid} is dead now");
          client.make_dead();
        }
      }
    }
  }
}
