mod client_context;
mod server_state;

use std::{process::ExitCode, sync::Arc};

use common::{
  config::PORT,
  msg::{read_message_from_wire, DecodeMessageResult},
};
use server_state::ServerState;
use tokio::{
  net::{tcp::OwnedWriteHalf, TcpListener, TcpStream},
  sync::Mutex,
};
use util::error::TermgameResult;

async fn handle_connection(
  stream: TcpStream,
  state: Arc<Mutex<ServerState<OwnedWriteHalf>>>,
) -> TermgameResult {
  let (mut rstream, wstream) = stream.into_split();
  let wstream = Arc::new(Mutex::new(wstream));
  while let DecodeMessageResult::Message(message) = read_message_from_wire(&mut rstream).await? {
    let mut state = state.lock().await;
    state.respond_to_message(wstream.clone(), message).await?;
  }
  Ok(())
}

async fn run_server() -> TermgameResult {
  let addr = format!("127.0.0.1:{PORT}");
  let listener = TcpListener::bind(addr).await?;

  let server_state = Arc::new(Mutex::new(ServerState::new()));

  loop {
    let (stream, _) = listener.accept().await?;
    let server_state = server_state.clone();

    tokio::spawn(async move {
      let result = handle_connection(stream, server_state);
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
