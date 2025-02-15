use std::sync::Arc;

use common::msg::{write_message_to_wire, ServerMessage};
use tokio::{io::AsyncWriteExt, sync::Mutex};
use util::error::TermgameResult;

enum ClientState<W> {
  Dead,
  Live(LiveClient<W>),
}

pub struct ClientContext<W> {
  state: ClientState<W>,
}

impl<W> ClientContext<W>
where
  W: AsyncWriteExt + Unpin,
{
  pub fn new(stream: Arc<Mutex<W>>) -> Self {
    Self {
      state: ClientState::Live(LiveClient::new(stream)),
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
}

pub struct LiveClient<W> {
  stream: Arc<Mutex<W>>,
}

impl<W> LiveClient<W>
where
  W: AsyncWriteExt + Unpin,
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
}
