use std::iter;

use termgame::{color, draw::Draw, entity::Entity};
use util::{
  grid::{Grid, Gridlike},
  pos::{Diff, Pos},
};

const Z_IDX: i32 = 5;

pub struct Crossword {
  grid: Grid<Option<char>>,
}

impl Crossword {
  pub fn new(w: u32, h: u32) -> Self {
    Self {
      grid: Grid::new(w, h),
    }
  }

  pub fn width(&self) -> u32 {
    self.grid.width()
  }

  pub fn height(&self) -> u32 {
    self.grid.height()
  }
}

impl Entity for Crossword {
  fn iterate_tiles(&self) -> Box<dyn Iterator<Item = (Draw, Pos)> + '_> {
    const XSCALE: i32 = 4;
    const YSCALE: i32 = 2;
    let col = color::AnsiValue::grayscale(20);

    Box::new(
      (0..self.height() as i32)
        .flat_map(move |y| {
          (0..self.width() as i32)
            .flat_map(move |x| {
              let pos = Pos {
                x: x * XSCALE,
                y: y * YSCALE,
              };

              (0..YSCALE).flat_map(move |dy| {
                (0..XSCALE).map(move |dx| {
                  let pos = pos + Diff { x: dx, y: dy };
                  let tile = if dx == 0 && dy == 0 {
                    '+'
                  } else if dx == 0 {
                    '|'
                  } else if dy == 0 {
                    '-'
                  } else {
                    ' '
                  };
                  (Draw::new(tile).with_fg(col).with_z(Z_IDX), pos)
                })
              })
            })
            .chain((0..YSCALE).map(move |dy| {
              let tile = if dy == 0 { '+' } else { '|' };
              (
                Draw::new(tile).with_fg(col).with_z(Z_IDX),
                Pos {
                  x: self.width() as i32 * XSCALE,
                  y: y * YSCALE + dy,
                },
              )
            }))
        })
        .chain((0..self.width() as i32).flat_map(move |x| {
          (0..XSCALE).map(move |dx| {
            let tile = if dx == 0 { '+' } else { '-' };
            (
              Draw::new(tile).with_fg(col).with_z(Z_IDX),
              Pos {
                x: x * XSCALE + dx,
                y: self.height() as i32 * YSCALE,
              },
            )
          })
        }))
        .chain(iter::once((
          Draw::new('+').with_fg(col).with_z(Z_IDX),
          Pos {
            x: self.width() as i32 * XSCALE,
            y: self.height() as i32 * YSCALE,
          },
        ))),
    )
  }

  fn as_any(&self) -> &dyn std::any::Any {
    self
  }
  fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
    self
  }
}
