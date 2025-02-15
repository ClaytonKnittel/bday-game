use std::{
  ops::{Deref, DerefMut},
  sync::Arc,
};

use common::{
  msg::{write_message_to_wire, ServerMessage},
  player_info::PlayerInfo,
  util::AsyncWriteT,
};
use termgame::color;
use tokio::sync::Mutex;
use util::{error::TermgameResult, pos::Pos};

enum ClientState<W> {
  Dead,
  Live(LiveClient<W>),
}

pub struct ClientContext<W> {
  state: ClientState<W>,
  player_info: PlayerInfo,
}

impl<W> ClientContext<W>
where
  W: AsyncWriteT,
{
  pub fn new(stream: Arc<Mutex<W>>) -> Self {
    Self {
      state: ClientState::Live(LiveClient::new(stream)),
      player_info: PlayerInfo {
        pos: Pos::zero(),
        color: color::AnsiValue::rgb(2, 3, 4).into(),
      },
    }
  }

  pub fn is_live(&self) -> bool {
    matches!(self.state, ClientState::Live { .. })
  }

  pub fn as_live(&self) -> Option<&LiveClient<W>> {
    match &self.state {
      ClientState::Live(state) => Some(state),
      ClientState::Dead => None,
    }
  }

  pub fn as_live_mut(&mut self) -> Option<&mut LiveClient<W>> {
    match &mut self.state {
      ClientState::Live(state) => Some(state),
      ClientState::Dead => None,
    }
  }

  pub fn make_live(&mut self, stream: Arc<Mutex<W>>) -> bool {
    match self.state {
      ClientState::Live(_) => false,
      ClientState::Dead => {
        self.state = ClientState::Live(LiveClient::new(stream));
        true
      }
    }
  }

  pub fn make_dead(&mut self) {
    self.state = ClientState::Dead;
  }

  pub fn player_info_mut(&mut self) -> &mut PlayerInfo {
    &mut self.player_info
  }
}

pub struct LiveClient<W> {
  stream: Arc<Mutex<W>>,
}

impl<W> LiveClient<W>
where
  W: AsyncWriteT,
{
  fn new(stream: Arc<Mutex<W>>) -> Self {
    Self { stream }
  }

  pub async fn write_message(&self, message: ServerMessage) -> TermgameResult {
    write_message_to_wire(&mut *self.stream.lock().await, message).await
  }

  pub async fn tcp_writeable(&self) -> bool {
    self.write_message(ServerMessage::Ping).await.is_ok()
  }

  pub async fn to_authenticated_mut(
    &mut self,
    stream: &Arc<Mutex<W>>,
  ) -> Option<AuthenticatedLiveClient<'_, W>> {
    let matches_expected = {
      let p1: *const _ = &(*self.stream.lock().await);
      let p2: *const _ = &(*stream.lock().await);
      p1 == p2
    };
    if matches_expected {
      Some(AuthenticatedLiveClient::new(self))
    } else {
      None
    }
  }
}

pub struct AuthenticatedLiveClient<'a, W> {
  live_client: &'a mut LiveClient<W>,
}

impl<'a, W> AuthenticatedLiveClient<'a, W>
where
  W: AsyncWriteT,
{
  fn new(live_client: &'a mut LiveClient<W>) -> Self {
    Self { live_client }
  }
}

impl<'a, W> Deref for AuthenticatedLiveClient<'a, W> {
  type Target = LiveClient<W>;

  fn deref(&self) -> &Self::Target {
    &self.live_client
  }
}

impl<'a, W> DerefMut for AuthenticatedLiveClient<'a, W> {
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.live_client
  }
}
