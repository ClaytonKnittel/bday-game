use common::{config::PORT, msg::ClientMessage};
use tokio::{io::AsyncWriteExt, net::TcpStream, sync::Mutex};
use util::{bitcode, error::TermgameResult};

pub struct Client {
  stream: Mutex<TcpStream>,
}

impl Client {
  pub async fn new() -> TermgameResult<Self> {
    Ok(Self {
      stream: Mutex::new(TcpStream::connect(format!("127.0.0.1:{PORT}")).await?),
    })
  }

  pub async fn write_test(&mut self) -> TermgameResult {
    let msg = ClientMessage::TestMessage("Hello guyz!".to_owned());
    let encoded = bitcode::encode(&msg);
    let len = encoded.len();

    {
      let mut stream = self.stream.lock().await;
      stream.write_u64(len as u64).await?;
      stream.write_all(&encoded).await?;
    }

    Ok(())
  }
}
