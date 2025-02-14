use std::process::ExitCode;

use util::error::TermgameResult;

fn run() -> TermgameResult {
  Ok(())
}

fn main() -> ExitCode {
  if let Err(err) = run() {
    println!("Error: {err}");
    ExitCode::FAILURE
  } else {
    ExitCode::SUCCESS
  }
}
