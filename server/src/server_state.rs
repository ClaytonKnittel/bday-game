use std::{collections::HashMap, iter::once, sync::Arc};

use common::msg::{write_message_to_wire, ClientMessage, ServerMessage};
use tokio::{io::AsyncWriteExt, sync::Mutex};
use util::error::TermgameResult;

use crate::client_context::{ClientContext, LiveClient};

enum Action {
  Respond(ServerMessage),
  Broadcast(ServerMessage),
  Ignore,
}

pub struct ServerState<W> {
  clients: HashMap<u64, ClientContext<W>>,
  next_uid: u64,
}

impl<W> ServerState<W>
where
  W: AsyncWriteExt + Unpin,
{
  pub fn new() -> Self {
    Self { clients: HashMap::new(), next_uid: 0 }
  }

  fn assign_new_uid(&mut self) -> u64 {
    let new_uid = self.next_uid;
    self.next_uid += 1;
    new_uid
  }

  fn live_connections_mut(&mut self) -> impl Iterator<Item = (u64, &mut LiveClient<W>)> {
    self
      .clients
      .iter_mut()
      .filter_map(|(&id, context)| context.to_live_mut().map(|client| (id, client)))
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
    uid: u64,
  ) -> TermgameResult<impl Iterator<Item = Action>> {
    Ok(once(Action::Respond(ServerMessage::NewConnection { uid })))
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
        execute!(self.connect_to_existing(uid).await?)
      }
    }
  }
}
