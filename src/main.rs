#![deny(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

mod crossword;

use crossword::Crossword;
use termgame::{error::TermgameResult, event_loop::EventLoop};

fn main() -> TermgameResult {
  let mut ev = EventLoop::new()?;
  ev.scene().add_entity(Box::new(Crossword::new(153, 45)));
  ev.run_event_loop(|_scene, window, t| {
    if t % 30 == 0 {
      window.camera_pos_mut().x += 1;
    }

    Ok(())
  })
}
