#![deny(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

mod crossword;
mod pc;

use crossword::Crossword;
use pc::Pc;
use termgame::{color::AnsiValue, error::TermgameResult, event_loop::EventLoop, pos::Pos};

fn main() -> TermgameResult {
  let mut ev = EventLoop::new()?;
  ev.scene().add_entity(Box::new(Crossword::new(153, 45)));
  let pc_uid = ev
    .scene()
    .add_entity(Box::new(Pc::new(Pos::zero(), AnsiValue::rgb(5, 0, 5))));
  ev.run_event_loop(|scene, window, _t| {
    let width = window.width() as i32;
    let height = window.height() as i32;

    let pc: &mut Pc = scene.entity_mut(pc_uid)?;
    let camera_pos = window.camera_pos_mut();

    camera_pos.x = (pc.pos().x - width / 2).max(0);
    camera_pos.y = (pc.pos().y - height / 2).max(0);

    Ok(())
  })
}
