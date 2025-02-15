use std::{
  collections::HashMap,
  iter::{empty, once},
  sync::Arc,
};

use common::{
  crossword::{Crossword, XWordTile},
  msg::{write_message_to_wire, ClientMessage, ServerMessage},
  util::AsyncWriteT,
};
use itertools::Itertools;
use tokio::sync::Mutex;
use util::{
  error::{TermgameError, TermgameResult},
  pos::Pos,
  variant::Variant2,
};

use crate::client_context::{AuthenticatedLiveClient, ClientContext, LiveClient};

enum Action {
  Respond(ServerMessage),
  Broadcast(ServerMessage),
}

enum State {
  Prompt,
  Crossword {
    crossword_answers: Crossword,
    scratch: Crossword,
  },
}

pub struct ServerState<W> {
  state: State,
  clients: HashMap<u64, ClientContext<W>>,
  next_uid: u64,
}

impl<W> ServerState<W>
where
  W: AsyncWriteT,
{
  pub fn new() -> Self {
    Self {
      state: State::Prompt,
      clients: HashMap::new(),
      next_uid: 0,
    }
  }

  pub fn make_crossword(&mut self) -> TermgameResult {
    todo!();
    Ok(())
  }

  pub fn with_crossword(crossword_answers: Crossword, scratch: Crossword) -> Self {
    Self {
      state: State::Crossword { crossword_answers, scratch },
      clients: HashMap::new(),
      next_uid: 0,
    }
  }

  pub fn scratch(&self) -> Option<&Crossword> {
    if let State::Crossword { scratch, .. } = &self.state {
      Some(scratch)
    } else {
      None
    }
  }

  pub fn scratch_mut(&mut self) -> Option<&mut Crossword> {
    if let State::Crossword { scratch, .. } = &mut self.state {
      Some(scratch)
    } else {
      None
    }
  }

  pub fn crossword_answers(&self) -> Option<&Crossword> {
    if let State::Crossword { crossword_answers, .. } = &self.state {
      Some(crossword_answers)
    } else {
      None
    }
  }

  fn assign_new_uid(&mut self) -> u64 {
    let new_uid = self.next_uid;
    self.next_uid += 1;
    new_uid
  }

  fn all_connections(&self) -> impl Iterator<Item = (&u64, &ClientContext<W>)> {
    self.clients.iter()
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
      }
    }

    Ok(())
  }

  async fn to_authenticated_mut(
    &mut self,
    uid: u64,
  ) -> TermgameResult<AuthenticatedLiveClient<'_, W>> {
    if let Some(state) = self.clients.get_mut(&uid) {
      if let Some(live_state) = state.as_live_mut() {
        if let Some(auth_client) = live_state.to_authenticated_mut().await {
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
    uid: u64,
  ) -> TermgameResult<&mut ClientContext<W>> {
    self.to_authenticated_mut(uid).await?;
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
      state.make_live(stream).ok_or_else(|| {
        TermgameError::Internal(format!("Trying to connect to live connection on {uid}"))
      })?;
      true
    } else {
      false
    };

    Ok(once(Action::Respond(ServerMessage::ConnectToExisting {
      success,
    })))
  }

  async fn position_update(
    &mut self,
    uid: u64,
    pos: Pos,
  ) -> TermgameResult<impl Iterator<Item = Action>> {
    let context = self.to_authenticated_context_mut(uid).await?;
    context.player_info_mut().pos = pos;
    Ok(once(Action::Broadcast(
      ServerMessage::PlayerPositionUpdate { uid, pos },
    )))
  }

  async fn tile_update(
    &mut self,
    pos: Pos,
    tile: XWordTile,
  ) -> TermgameResult<impl Iterator<Item = Action>> {
    if matches!(tile, XWordTile::Wall) {
      return Err(TermgameError::Internal("Cannot place wall tiles".to_owned()).into());
    }

    let xword_tile = self
      .scratch_mut()
      .ok_or_else(|| TermgameError::Internal("Not in crossword state".to_owned()))?
      .tile_mut(pos)?;
    match xword_tile {
      XWordTile::Empty | XWordTile::Letter(_) => {
        *xword_tile = tile.clone();
        Ok(once(Action::Broadcast(ServerMessage::TileUpdate {
          pos,
          tile,
        })))
      }
      XWordTile::Wall => {
        Err(TermgameError::Internal(format!("Cannot modify wall tile at {pos}")).into())
      }
    }
  }

  async fn check_tile(&mut self, pos: Pos) -> TermgameResult<impl Iterator<Item = Action>> {
    Ok(
      if let Ok(tile) = self
        .crossword_answers()
        .ok_or_else(|| TermgameError::Internal("Not in crossword state".to_owned()))?
        .tile(pos)
      {
        let response = ServerMessage::CheckTile { pos, tile: tile.clone() };
        Variant2::Opt1(
          [
            Action::Respond(response.clone()),
            Action::Broadcast(response),
          ]
          .into_iter(),
        )
      } else {
        Variant2::Opt2(empty())
      },
    )
  }

  async fn cycle_clue(
    &mut self,
    pos: Pos,
    is_row: bool,
  ) -> TermgameResult<impl Iterator<Item = Action>> {
    if let Some(clue) = self
      .scratch_mut()
      .ok_or_else(|| TermgameError::Internal("Not in crossword state".to_owned()))?
      .clue_for_pos_mut(pos, is_row)
    {
      let clue_entries = clue
        .clue_entries
        .iter()
        .skip(1)
        .chain(clue.clue_entries.first().into_iter())
        .cloned()
        .collect_vec();
      clue.clue_entries = clue_entries;

      Ok(Variant2::Opt1(self.full_refresh().await?))
    } else {
      Ok(Variant2::Opt2(empty()))
    }
  }

  async fn full_refresh(&self) -> TermgameResult<impl Iterator<Item = Action>> {
    if let Some(scratch) = self.scratch() {
      Ok(Variant2::Opt1(once(Action::Respond(
        ServerMessage::FullRefresh {
          crossword: scratch.clone().into(),
          player_info: self
            .all_connections()
            .filter(|(_, connection)| connection.is_live())
            .map(|(&uid, connection)| (uid, connection.player_info().clone()))
            .collect(),
        },
      ))))
    } else {
      Ok(Variant2::Opt2(once(Action::Respond(ServerMessage::OnClue))))
    }
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
        execute!(self.position_update(uid, pos).await?)
      }
      ClientMessage::TileUpdate { pos, tile } => {
        execute!(self.tile_update(pos, tile).await?)
      }
      ClientMessage::CheckTile { pos } => {
        execute!(self.check_tile(pos).await?)
      }
      ClientMessage::CycleClue { pos, is_row } => {
        execute!(self.cycle_clue(pos, is_row).await?)
      }
      ClientMessage::FullRefresh => {
        execute!(self.full_refresh().await?)
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
