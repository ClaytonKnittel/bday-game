use std::{
  cell::{Ref, RefCell},
  collections::HashMap,
  iter,
};

use common::crossword::XWordTile;
use itertools::Itertools;
use termgame::{color, draw::Draw, entity::Entity, Key};
use util::{
  error::TermgameResult,
  grid::{Grid, Gridlike, MutGridlike},
  pos::{Diff, Pos},
  union_find::UnionFind,
};
use xword_gen::xword::XWord;

#[derive(PartialEq, Eq, PartialOrd, Ord)]
enum Satisfaction {
  Good,
  Ok,
  Bad,
  Forbidden,
}

#[derive(Clone, Copy, Debug)]
pub enum InteractiveGridMode {
  WordLengthSat,
  DisjointRegions,
}

trait TileColorer {
  fn tile_color(&self, grid: &InteractiveGrid, pos: Pos) -> color::AnsiValue;

  fn refresh(&mut self, grid: &Grid<XWordTile>);
}

struct LengthSatColorer {}

impl LengthSatColorer {
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

  fn tile_sat(&self, grid: &InteractiveGrid, pos: Pos) -> Satisfaction {
    // row sat:
    let row_length = 1
      + (1..)
        .map_while(|dx| grid.is_free(pos + Diff { x: dx, y: 0 }).then_some(()))
        .count()
      + (1..)
        .map_while(|dx| grid.is_free(pos + Diff { x: -dx, y: 0 }).then_some(()))
        .count();
    // col sat:
    let col_length = 1
      + (1..)
        .map_while(|dy| grid.is_free(pos + Diff { x: 0, y: dy }).then_some(()))
        .count()
      + (1..)
        .map_while(|dy| grid.is_free(pos + Diff { x: 0, y: -dy }).then_some(()))
        .count();

    Self::length_sat(row_length as u32).max(Self::length_sat(col_length as u32))
  }
}

impl TileColorer for LengthSatColorer {
  fn tile_color(&self, grid: &InteractiveGrid, pos: Pos) -> color::AnsiValue {
    match self.tile_sat(grid, pos) {
      Satisfaction::Good => color::AnsiValue::rgb(0, 4, 0),
      Satisfaction::Ok => color::AnsiValue::rgb(5, 3, 0),
      Satisfaction::Bad => color::AnsiValue::rgb(5, 0, 0),
      Satisfaction::Forbidden => color::AnsiValue::rgb(1, 0, 0),
    }
  }

  fn refresh(&mut self, _grid: &Grid<XWordTile>) {}
}

struct DisjointRegionColorer {
  uf: Option<UnionFind<Pos>>,
  color_map: HashMap<Pos, color::AnsiValue>,
}

impl DisjointRegionColorer {
  fn new() -> Self {
    Self { uf: None, color_map: HashMap::new() }
  }

  fn color_at_idx(idx: usize) -> color::AnsiValue {
    let r = (3 + 3 * idx + (idx / 2)) % 6;
    let g = idx % 6;
    let b = (2 + idx / 6) % 6;
    color::AnsiValue::rgb(r as u8, g as u8, b as u8)
  }
}

impl TileColorer for DisjointRegionColorer {
  fn tile_color(&self, _grid: &InteractiveGrid, pos: Pos) -> color::AnsiValue {
    self
      .uf
      .as_ref()
      .and_then(|uf| self.color_map.get(&uf.find_immut(pos)))
      .cloned()
      .unwrap_or(color::AnsiValue::grayscale(23))
  }

  fn refresh(&mut self, grid: &Grid<XWordTile>) {
    self.uf = XWord::from_grid(grid.clone(), iter::empty())
      .map(|xword| xword.build_partition_uf())
      .ok();

    if let Some(ref uf) = self.uf {
      let mut root_level_keys = uf.root_level_keys().into_iter().collect_vec();
      root_level_keys.sort_by_key(|pos| (pos.x as u64 + pos.y as u64 * (u32::MAX as u64 + 1)));
      self.color_map = root_level_keys
        .into_iter()
        .enumerate()
        .map(|(idx, key)| (key, Self::color_at_idx(idx)))
        .collect()
    }
  }
}

pub struct InteractiveGrid {
  grid: Grid<XWordTile>,
  tile_colorer: Box<RefCell<dyn TileColorer>>,
  cursor_pos: Pos,
  to_right: bool,
}

impl InteractiveGrid {
  fn build_tile_colorer(mode: InteractiveGridMode) -> Box<RefCell<dyn TileColorer>> {
    match mode {
      InteractiveGridMode::WordLengthSat => Box::new(RefCell::new(LengthSatColorer {})),
      InteractiveGridMode::DisjointRegions => Box::new(RefCell::new(DisjointRegionColorer::new())),
    }
  }

  pub fn new(mode: InteractiveGridMode, width: u32, height: u32) -> TermgameResult<Self> {
    Ok(Self::from_grid(
      Grid::from_vec(
        vec![XWordTile::Empty; (width * height) as usize],
        width,
        height,
      )?,
      mode,
    ))
  }

  pub fn from_grid(grid: Grid<XWordTile>, mode: InteractiveGridMode) -> Self {
    Self {
      grid,
      tile_colorer: Self::build_tile_colorer(mode),
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

  fn tile_colorer(&self) -> Ref<'_, dyn TileColorer> {
    self.tile_colorer.as_ref().borrow()
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

  fn cursor_move_delta(&self) -> Diff {
    if self.to_right {
      Diff { x: 1, y: 0 }
    } else {
      Diff { x: 0, y: 1 }
    }
  }
}

impl Entity for InteractiveGrid {
  fn iterate_tiles(&self) -> Box<dyn Iterator<Item = (Draw, Pos)> + '_> {
    self.tile_colorer.borrow_mut().refresh(&self.grid);

    Box::new(
      (0..self.grid.height() as i32)
        .flat_map(move |y| {
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
                Draw::new(tile).with_fg(self.tile_colorer().tile_color(self, pos))
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
        })
        .chain(
          self
            .cursor_pos
            .to_string()
            .chars()
            .enumerate()
            .map(move |(idx, c)| {
              (
                Draw::new(c).with_fg(color::AnsiValue::grayscale(23)),
                Pos { x: 102 + idx as i32, y: 30 },
              )
            })
            .collect::<Vec<_>>(),
        ),
    )
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
              self.cursor_pos += self.cursor_move_delta();
            }
          }
        } else if letter == ' ' {
          self.cursor_pos += self.cursor_move_delta();
        } else if letter == '\t' {
          self.to_right = !self.to_right;
        }
      }
      Key::Backspace => {
        if let Some(tile) = self.grid.get_mut(self.cursor_pos) {
          if !matches!(tile, XWordTile::Wall) {
            if matches!(tile, XWordTile::Letter(_)) {
              *tile = XWordTile::Empty;
            }
            self.cursor_pos -= self.cursor_move_delta();
          }
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
