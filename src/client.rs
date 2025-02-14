use std::sync::Arc;

use common::{
  config::PORT,
  msg::{write_message_to_wire, ClientMessage, ServerMessage},
};
use tokio::{
  io::AsyncWriteExt,
  net::TcpStream,
  sync::{
    mpsc::{self, UnboundedReceiver, UnboundedSender},
    Mutex,
  },
  task::JoinHandle,
};
use util::{
  bitcode::{self, Decode},
  error::TermgameResult,
};

pub struct Client {
  stream: Arc<Mutex<TcpStream>>,
  rx: UnboundedReceiver<ServerMessage>,
}

impl Client {
  async fn listen_for_messages<T>(
    stream: Arc<Mutex<TcpStream>>,
    tx: UnboundedSender<T>,
  ) -> TermgameResult
  where
    T: for<'a> Decode<'a> + 'static,
  {
    Ok(())
  }

  fn start_listening_thread<T>(
    stream: Arc<Mutex<TcpStream>>,
    tx: UnboundedSender<T>,
  ) -> TermgameResult<JoinHandle<TermgameResult>>
  where
    T: for<'a> Decode<'a> + Send + 'static,
  {
    Ok(tokio::spawn(async move {
      Self::listen_for_messages(stream, tx).await
    }))
  }

  pub async fn new() -> TermgameResult<Self> {
    let (tx, rx) = mpsc::unbounded_channel();

    let stream = Arc::new(Mutex::new(
      TcpStream::connect(format!("127.0.0.1:{PORT}")).await?,
    ));

    Self::start_listening_thread(stream.clone(), tx)?;
    Ok(Self { stream, rx })
  }

  pub async fn write_test(&mut self) -> TermgameResult {
    let msg = ClientMessage::TestMessage("Hello guyz!".to_owned());
    let mut stream = self.stream.lock().await;
    write_message_to_wire(&mut stream, msg).await?;
    Ok(())
  }
}
