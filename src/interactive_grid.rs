use std::collections::HashMap;

use termgame::{color, draw::Draw, entity::Entity, Key};
use util::{
  error::TermgameResult,
  grid::{Grid, Gridlike, MutGridlike},
  pos::{Diff, Pos},
};
use xword_gen::xword::XWordTile;

#[derive(PartialEq, Eq, PartialOrd, Ord)]
enum Satisfaction {
  Good,
  Ok,
  Bad,
  Forbidden,
}

pub struct InteractiveGrid {
  grid: Grid<XWordTile>,
  cursor_pos: Pos,
  to_right: bool,
}

impl InteractiveGrid {
  pub fn new(width: u32, height: u32) -> TermgameResult<Self> {
    Ok(Self {
      grid: Grid::from_vec(
        vec![XWordTile::Empty; (width * height) as usize],
        width,
        height,
      )?,
      cursor_pos: Pos::zero(),
      to_right: true,
    })
  }

  pub fn from_grid(grid: Grid<XWordTile>) -> Self {
    Self {
      grid,
      cursor_pos: Pos::zero(),
      to_right: true,
    }
  }

  pub fn grid(&self) -> &Grid<XWordTile> {
    &self.grid
  }

  pub fn cursor_screen_pos(&self) -> Pos {
    Pos {
      x: self.cursor_pos.x * 2,
      ..self.cursor_pos
    }
  }

  pub fn screen_width(&self) -> u32 {
    self.grid.width() * 2 - 1
  }

  pub fn screen_height(&self) -> u32 {
    self.grid.height()
  }

  fn is_free(&self, pos: Pos) -> bool {
    self.grid.get(pos).is_some_and(|tile| tile.available())
  }

  fn length_sat(length: u32) -> Satisfaction {
    if length < 3 {
      Satisfaction::Forbidden
    } else if length < 5 {
      Satisfaction::Ok
    } else if length < 9 {
      Satisfaction::Good
    } else if length < 12 {
      Satisfaction::Ok
    } else if length <= 15 {
      Satisfaction::Bad
    } else {
      Satisfaction::Forbidden
    }
  }

  fn tile_sat(&self, pos: Pos) -> Satisfaction {
    // row sat:
    let row_length = 1
      + (1..)
        .map_while(|dx| self.is_free(pos + Diff { x: dx, y: 0 }).then_some(()))
        .count()
      + (1..)
        .map_while(|dx| self.is_free(pos + Diff { x: -dx, y: 0 }).then_some(()))
        .count();
    // col sat:
    let col_length = 1
      + (1..)
        .map_while(|dy| self.is_free(pos + Diff { x: 0, y: dy }).then_some(()))
        .count()
      + (1..)
        .map_while(|dy| self.is_free(pos + Diff { x: 0, y: -dy }).then_some(()))
        .count();

    Self::length_sat(row_length as u32).max(Self::length_sat(col_length as u32))
  }

  fn clue_num_map(&self) -> HashMap<Pos, u32> {
    (0..self.grid.height() as i32)
      .flat_map(|y| {
        (0..self.grid.width() as i32).filter_map(move |x| {
          let pos = Pos { x, y };
          (self.is_free(pos)
            && (!self.is_free(pos + Diff { x: -1, y: 0 })
              || !self.is_free(pos + Diff { x: 0, y: -1 })))
          .then_some(pos)
        })
      })
      .enumerate()
      .map(|(idx, pos)| (pos, (idx + 1) as u32))
      .collect()
  }
}

impl Entity for InteractiveGrid {
  fn iterate_tiles(&self) -> Box<dyn Iterator<Item = (Draw, Pos)> + '_> {
    let sat_color = |sat: Satisfaction| match sat {
      Satisfaction::Good => color::AnsiValue::rgb(0, 4, 0),
      Satisfaction::Ok => color::AnsiValue::rgb(5, 3, 0),
      Satisfaction::Bad => color::AnsiValue::rgb(5, 0, 0),
      Satisfaction::Forbidden => color::AnsiValue::rgb(1, 0, 0),
    };

    Box::new((0..self.grid.height() as i32).flat_map(move |y| {
      let clue_num_map = self.clue_num_map();
      (0..self.grid.width() as i32).map(move |x| {
        let pos = Pos { x, y };
        let mut draw = match self.grid.get(pos) {
          Some(XWordTile::Wall) => Draw::new('*'),
          Some(&XWordTile::Letter(letter)) => Draw::new(letter),
          Some(XWordTile::Empty) => {
            let tile = if let Some(clue_num) = clue_num_map.get(&pos) {
              char::from_u32(((clue_num % 10) as u8 + b'0') as u32).unwrap_or('?')
            } else {
              '_'
            };
            Draw::new(tile).with_fg(sat_color(self.tile_sat(pos)))
          }
          _ => unreachable!(),
        };

        if pos == self.cursor_pos {
          draw = draw
            .with_italic()
            .with_bold()
            .with_fg(color::AnsiValue::grayscale(23));
        }

        (draw, Pos { x: 2 * x, y })
      })
    }))
  }

  fn as_any(&self) -> &dyn std::any::Any {
    self
  }
  fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
    self
  }

  fn click(&mut self, pos: Pos) -> util::error::TermgameResult {
    let grid_pos = Pos { x: pos.x / 2, ..pos };
    self.cursor_pos = grid_pos;
    if let Some(tile) = self.grid.get_mut(grid_pos) {
      *tile = match tile {
        XWordTile::Wall => XWordTile::Empty,
        XWordTile::Empty => XWordTile::Wall,
        XWordTile::Letter(_) => XWordTile::Empty,
      };
    }
    Ok(())
  }

  fn drag(&mut self, pos: Pos) -> util::error::TermgameResult {
    if pos.x % 2 == 0 {
      self.click(pos)
    } else {
      Ok(())
    }
  }

  fn keypress(&mut self, key: Key) -> TermgameResult {
    match key {
      Key::Char(letter) => {
        if letter.is_ascii_lowercase() {
          if let Some(tile) = self.grid.get_mut(self.cursor_pos) {
            if !matches!(tile, XWordTile::Wall) {
              *tile = XWordTile::Letter(letter);
              if self.to_right {
                self.cursor_pos.x += 1;
              } else {
                self.cursor_pos.y += 1;
              }
            }
          }
        } else if letter == ' ' {
          if self.to_right {
            self.cursor_pos.x += 1;
          } else {
            self.cursor_pos.y += 1;
          }
        } else if letter == '\t' {
          self.to_right = !self.to_right;
        }
      }
      Key::Left => self.cursor_pos.x -= 1,
      Key::Right => self.cursor_pos.x += 1,
      Key::Up => self.cursor_pos.y -= 1,
      Key::Down => self.cursor_pos.y += 1,
      _ => {}
    }

    self.cursor_pos.x = 0.max((self.grid.width() as i32 - 1).min(self.cursor_pos.x));
    self.cursor_pos.y = 0.max((self.grid.height() as i32 - 1).min(self.cursor_pos.y));

    Ok(())
  }
}
