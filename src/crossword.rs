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

  pub fn from_grid(grid: Grid<Option<char>>) -> Self {
    Self { grid }
  }

  pub fn width(&self) -> u32 {
    self.grid.width()
  }

  pub fn height(&self) -> u32 {
    self.grid.height()
  }

  fn is_wall(&self, pos: Pos) -> bool {
    self.grid.get(pos).is_none_or(|tile| tile.is_none())
  }

  fn cross_at(&self, pos: Pos) -> char {
    const CROSSES: [char; 16] = [
      // ┼           ╀           ┾           ╄
      '\u{253C}', '\u{2540}', '\u{253E}', '\u{2544}',
      // ╁           ╂           ╆           ╊
      '\u{2541}', '\u{2542}', '\u{2546}', '\u{254A}',
      // ┽           ╃           ┿           ╇
      '\u{253D}', '\u{2543}', '\u{253F}', '\u{2547}',
      // ╅           ╉           ╈           ╋
      '\u{2545}', '\u{2549}', '\u{2548}', '\u{254B}',
    ];

    let u_in_bounds = self.grid.in_bounds(pos + Diff { x: 0, y: -1 });
    let self_in_bounds = self.grid.in_bounds(pos + Diff { x: 0, y: 0 });
    let l_in_bounds = self.grid.in_bounds(pos + Diff { x: -1, y: 0 });

    let ul = self.is_wall(pos + Diff { x: -1, y: -1 });
    let ur = self.is_wall(pos + Diff { x: 0, y: -1 });
    let dl = self.is_wall(pos + Diff { x: -1, y: 0 });
    let dr = self.is_wall(pos + Diff { x: 0, y: 0 });

    let u_solid = ul || ur;
    let r_solid = ur || dr;
    let d_solid = dl || dr;
    let l_solid = ul || dl;

    if !l_in_bounds && !u_in_bounds {
      // ┏
      '\u{250F}'
    } else if !self_in_bounds {
      if !l_in_bounds && !u_in_bounds {
        // ┛
        '\u{251B}'
      } else if !l_in_bounds {
        if u_solid {
          // ┻
          '\u{253B}'
        } else {
          // ┷
          '\u{2537}'
        }
      } else if !u_in_bounds {
        if l_solid {
          // ┫
          '\u{252B}'
        } else {
          // ┨
          '\u{2528}'
        }
      } else {
        unreachable!();
      }
    } else if !l_in_bounds {
      if r_solid {
        // ┣
        '\u{2523}'
      } else {
        // ┠
        '\u{2520}'
      }
    } else if !u_in_bounds {
      if d_solid {
        // ┳
        '\u{2533}'
      } else {
        // ┯
        '\u{252F}'
      }
    } else {
      let idx = u_solid as usize
        + ((r_solid as usize) << 1)
        + ((d_solid as usize) << 2)
        + ((l_solid as usize) << 3);
      CROSSES[idx]
    }
  }

  fn v_bar_at(&self, pos: Pos) -> char {
    let l = self.is_wall(pos + Diff { x: -1, y: 0 });
    let r = self.is_wall(pos + Diff { x: 0, y: 0 });

    if l || r {
      // ┃
      '\u{2503}'
    } else {
      // │
      '\u{2502}'
    }
  }

  fn h_bar_at(&self, pos: Pos) -> char {
    let u = self.is_wall(pos + Diff { x: 0, y: -1 });
    let d = self.is_wall(pos + Diff { x: 0, y: 0 });

    if u || d {
      // ━
      '\u{2501}'
    } else {
      // ─
      '\u{2500}'
    }
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
                (0..XSCALE).flat_map(move |dx| {
                  let pos = pos + Diff { x: dx, y: dy };
                  let grid_pos = Pos { x, y };
                  let letter = *self.grid.get(grid_pos)?;

                  let tile = if dx == 0 && dy == 0 {
                    self.cross_at(grid_pos)
                  } else if dx == 0 {
                    self.v_bar_at(grid_pos)
                  } else if dy == 0 {
                    self.h_bar_at(grid_pos)
                  } else {
                    letter.unwrap_or(if dx == XSCALE / 2 && dy == YSCALE / 2 {
                      '\u{2573}'
                    } else {
                      ' '
                    })
                  };

                  let mut draw = Draw::new(tile).with_fg(col).with_z(Z_IDX);
                  if letter.is_none() && dx != 0 && dy != 0 {
                    draw = draw.with_bold().with_fg(color::AnsiValue::grayscale(0));
                  }

                  Some((draw, pos))
                })
              })
            })
            .chain((0..YSCALE).map(move |dy| {
              let grid_pos = Pos {
                x: self.width() as i32,
                y,
              };
              let tile = if y == 0 && dy == 0 {
                // ┓
                '\u{2513}'
              } else if dy == 0 {
                self.cross_at(grid_pos)
              } else {
                self.v_bar_at(grid_pos)
              };
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
            let grid_pos = Pos {
              x,
              y: self.height() as i32,
            };
            let tile = if x == 0 && dx == 0 {
              // ┗
              '\u{2517}'
            } else if dx == 0 {
              self.cross_at(grid_pos)
            } else {
              self.h_bar_at(grid_pos)
            };
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
          Draw::new(
            // ┛
            '\u{251B}',
          )
          .with_fg(col)
          .with_z(Z_IDX),
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
