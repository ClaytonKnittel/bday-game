#![deny(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

mod crossword;
mod pc;

use std::fs;

use crossword::Crossword;
use pc::Pc;
use termgame::{color::AnsiValue, event_loop::EventLoop};
use util::{bitcode, error::TermgameResult, grid::Grid, pos::Pos};

fn main() -> TermgameResult {
  let mut ev = EventLoop::new()?;
  let grid: Grid<_> = bitcode::decode(&fs::read("xword_gen/crossword.bin")?)?;

  let xword_uid = ev.scene().add_entity(Box::new(Crossword::from_grid(grid)));
  let pc_uid = ev
    .scene()
    .add_entity(Box::new(Pc::new(Pos::zero(), AnsiValue::rgb(5, 0, 5))));
  ev.run_event_loop(|scene, window, _t| {
    let width = window.width() as i32;
    let height = window.height() as i32;

    let pc: &Pc = scene.entity(pc_uid)?;
    let xword: &Crossword = scene.entity(xword_uid)?;
    let camera_pos = window.camera_pos_mut();

    camera_pos.x = (pc.pos().x - width / 2)
      .max(0)
      .min((xword.screen_width()).saturating_sub(width as u32) as i32);
    camera_pos.y = (pc.pos().y - height / 2)
      .max(0)
      .min((xword.screen_height()).saturating_sub(height as u32) as i32);

    Ok(())
  })
}
