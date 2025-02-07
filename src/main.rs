#![deny(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

mod crossword;
mod interactive_grid;
mod pc;

use std::{
  fs::{self, File},
  io::Write,
};

use crossword::Crossword;
use interactive_grid::InteractiveGrid;
use pc::Pc;
use termgame::{color::AnsiValue, event_loop::EventLoop};
use util::{bitcode, error::TermgameResult, grid::Grid, pos::Pos};

const GRID_PATH: &str = "./grid.bin";

fn read_grid(path: &str) -> TermgameResult<InteractiveGrid> {
  Ok(bitcode::decode(&fs::read(path)?)?)
}

fn main() -> TermgameResult {
  let mut ev = EventLoop::new()?;
  // let grid: Grid<_> = bitcode::decode(&fs::read("xword_gen/crossword.bin")?)?;
  // let xword_uid = ev.scene().add_entity(Box::new(Crossword::from_grid(grid)));

  let grid = read_grid(GRID_PATH).unwrap_or_else(|_| InteractiveGrid::new(60, 60));
  let grid_uid = ev.scene().add_entity(Box::new(grid));

  let pc_uid = ev
    .scene()
    .add_entity(Box::new(Pc::new(Pos::zero(), AnsiValue::rgb(5, 0, 5))));
  ev.run_event_loop(|scene, window, _t| {
    let width = window.width() as i32;
    let height = window.height() as i32;

    let pc: &Pc = scene.entity(pc_uid)?;
    // let xword: &Crossword = scene.entity(xword_uid)?;
    let grid: &InteractiveGrid = scene.entity(grid_uid)?;
    let camera_pos = window.camera_pos_mut();

    camera_pos.x = (pc.pos().x - width / 2)
      .max(0)
      .min((grid.screen_width()).saturating_sub(width as u32) as i32);
    camera_pos.y = (pc.pos().y - height / 2)
      .max(0)
      .min((grid.screen_height()).saturating_sub(height as u32) as i32);

    Ok(())
  })?;

  {
    let grid: &InteractiveGrid = ev.scene().entity(grid_uid)?;
    let grid_serialized = bitcode::encode(grid);
    let mut file = File::create(GRID_PATH)?;
    file.write_all(&grid_serialized)?;
  }

  Ok(())
}
