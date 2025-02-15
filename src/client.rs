use std::iter;

use common::{
  config::PORT,
  msg::{
    read_message_from_wire, write_message_to_wire, ClientMessage, DecodeMessageResult,
    ServerMessage,
  },
};
use tokio::{
  net::{
    tcp::{OwnedReadHalf, OwnedWriteHalf},
    TcpStream,
  },
  sync::mpsc::{self, error::TryRecvError, UnboundedReceiver, UnboundedSender},
  task::JoinHandle,
};
use util::{
  bitcode::Decode,
  error::{TermgameError, TermgameResult},
};

pub struct Client {
  stream: OwnedWriteHalf,
  rx: UnboundedReceiver<ServerMessage>,
}

impl Client {
  async fn listen_for_messages<T>(
    mut stream: OwnedReadHalf,
    tx: UnboundedSender<T>,
  ) -> TermgameResult
  where
    T: for<'a> Decode<'a> + 'static,
  {
    loop {
      let message = {
        match read_message_from_wire(&mut stream).await? {
          DecodeMessageResult::Message(message) => message,
          DecodeMessageResult::EndOfStream => return Ok(()),
        }
      };

      if let Err(err) = tx.send(message) {
        return Err(TermgameError::Internal(format!("Unbounded send error: {err}")).into());
      }
    }
  }

  fn start_listening_thread<T>(
    stream: OwnedReadHalf,
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

    let stream = TcpStream::connect(format!("127.0.0.1:{PORT}")).await?;
    let (rstream, mut wstream) = stream.into_split();

    Self::start_listening_thread(rstream, tx)?;

    // TODO: connect to existing if possible.
    write_message_to_wire(&mut wstream, ClientMessage::NewConnection).await?;

    Ok(Self { stream: wstream, rx })
  }

  pub fn pending_server_messages(
    &mut self,
  ) -> impl Iterator<Item = TermgameResult<ServerMessage>> + '_ {
    iter::once(())
      .cycle()
      .map_while(|_| match self.rx.try_recv() {
        Ok(event) => Some(Ok(event)),
        Err(TryRecvError::Empty) => None,
        Err(TryRecvError::Disconnected) => Some(Err(
          TermgameError::Internal("mpsc stream disconnected".to_owned()).into(),
        )),
      })
  }

  pub async fn send_message(&mut self, message: ClientMessage) -> TermgameResult {
    write_message_to_wire(&mut self.stream, message).await
  }
}
