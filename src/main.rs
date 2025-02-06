#![deny(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

mod crossword;
mod pc;

use crossword::Crossword;
use pc::Pc;
use termgame::{color::AnsiValue, event_loop::EventLoop};
use util::{
  error::TermgameResult,
  grid::{Grid, MutGridlike},
  pos::Pos,
};

fn main() -> TermgameResult {
  let mut ev = EventLoop::new()?;
  let mut grid = Grid::new(30, 20);
  if let Some(slot) = grid.get_mut(Pos { x: 4, y: 3 }) {
    *slot = Some('a');
  }
  if let Some(slot) = grid.get_mut(Pos { x: 5, y: 3 }) {
    *slot = Some(' ');
  }
  if let Some(slot) = grid.get_mut(Pos { x: 6, y: 3 }) {
    *slot = Some('b');
  }
  if let Some(slot) = grid.get_mut(Pos { x: 5, y: 4 }) {
    *slot = Some(' ');
  }
  if let Some(slot) = grid.get_mut(Pos { x: 6, y: 4 }) {
    *slot = Some(' ');
  }

  ev.scene().add_entity(Box::new(Crossword::from_grid(grid)));
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
