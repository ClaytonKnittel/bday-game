mod client_context;
mod server_state;

use std::{process::ExitCode, sync::Arc, time::Duration};

use common::{
  config::PORT,
  crossword::{Crossword, XWordTile},
  msg::{read_message_from_wire, DecodeMessageResult},
  util::AsyncWriteT,
};
use server_state::ServerState;
use tokio::{
  fs::{self, File},
  io::AsyncWriteExt,
  net::{tcp::OwnedWriteHalf, TcpListener, TcpStream},
  sync::Mutex,
  time::sleep,
};
use util::{bitcode, error::TermgameResult};
use xword_dict::XWordDict;

const XWORD_PATH: &str = "../xword_gen/crossword.bin";
const XWORD_SCRATCH_PATH: &str = "./crossword_scratch.bin";
const DICT_PATH: &str = "../xword_gen/dict.bin";

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

async fn cleanup_loop<W>(server_state: Arc<Mutex<ServerState<W>>>) -> TermgameResult
where
  W: AsyncWriteT,
{
  loop {
    sleep(Duration::from_secs(5)).await;
    let mut server_state = server_state.lock().await;
    server_state.cleanup_dead_clients().await;
    save_scratch(server_state.scratch()).await?;
  }
}

async fn read_dict() -> TermgameResult<XWordDict> {
  Ok(bitcode::decode(&fs::read(DICT_PATH).await?)?)
}

async fn load_crossword() -> TermgameResult<(Crossword, Crossword)> {
  let dict = read_dict().await?;
  let crossword = Crossword::make_clues(bitcode::decode(&fs::read(XWORD_PATH).await?)?, &dict)?;

  let scratch = fs::read(XWORD_SCRATCH_PATH)
    .await
    .map(|encoded| -> TermgameResult<_> {
      Ok(Crossword::from_grid(
        bitcode::decode(&encoded)?,
        crossword.clue_map().clone(),
      ))
    })
    .unwrap_or_else(|_| Ok(crossword.clone_clearing_tiles()))?;

  Ok((crossword, scratch))
}

async fn save_scratch(scratch: &Crossword) -> TermgameResult {
  let result = bitcode::encode(scratch.grid());
  let mut file = File::create(XWORD_SCRATCH_PATH).await?;
  file.write_all(&result).await?;
  Ok(())
}

async fn run_server() -> TermgameResult {
  let addr = format!("127.0.0.1:{PORT}");
  let listener = TcpListener::bind(addr).await?;

  let (crossword, scratch) = load_crossword().await?;
  let server_state = Arc::new(Mutex::new(ServerState::with_crossword(crossword, scratch)));

  {
    let server_state = server_state.clone();
    tokio::spawn(async move { cleanup_loop(server_state.clone()).await });
  }

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
