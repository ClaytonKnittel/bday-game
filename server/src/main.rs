use std::process::ExitCode;

use common::{config::PORT, msg::ClientMessage};
use tokio::{
  io::AsyncReadExt,
  net::{TcpListener, TcpStream},
};
use util::{bitcode, error::TermgameResult};

async fn respond_to_message(message: ClientMessage) -> TermgameResult {
  println!("Message: {message:?}");
  Ok(())
}

async fn handle_connection(mut stream: TcpStream) -> TermgameResult {
  loop {
    let len = stream.read_u64().await?;

    let mut buf = vec![0u8; len as usize];
    stream.read_exact(buf.as_mut_slice()).await?;
    if buf.is_empty() {
      break;
    }

    let message = bitcode::decode(buf.as_slice())?;
    respond_to_message(message).await?;
  }

  Ok(())
}

async fn run_server() -> TermgameResult {
  let addr = format!("127.0.0.1:{PORT}");

  // let socket = TcpSocket::new_v4()?;
  // let stream = socket.connect(addr).await?;
  let listener = TcpListener::bind(addr).await?;

  loop {
    let (stream, _) = listener.accept().await?;

    tokio::spawn(async move {
      let result = handle_connection(stream);
      if let Err(err) = result.await {
        println!("Connection error: {err}");
      }
    });
  }
}

#[tokio::main(flavor = "multi_thread", worker_threads = 10)]
async fn main() -> ExitCode {
  if let Err(err) = run_server().await {
    println!("Error: {err}");
    ExitCode::FAILURE
  } else {
    ExitCode::SUCCESS
  }
}
