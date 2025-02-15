use std::collections::HashMap;

use util::{
  error::{TermgameError, TermgameResult},
  grid::{Grid, Gridlike, MutGridlike},
  pos::{Diff, Pos},
};
use xword_gen::xword::XWordTile;

pub struct Crossword {
  grid: Grid<XWordTile>,
  clue_map: HashMap<(Pos, bool), Pos>,
}

impl Crossword {
  fn build_clue_map(grid: &Grid<XWordTile>) -> HashMap<(Pos, bool), Pos> {
    grid
      .positions()
      .filter(|&pos| {
        grid
          .get(pos)
          .is_some_and(|tile| !matches!(tile, XWordTile::Wall))
      })
      .fold(HashMap::new(), |mut map, pos| {
        map.insert(
          (pos, false),
          map.get(&(pos - Diff::DY, false)).cloned().unwrap_or(pos),
        );
        map.insert(
          (pos, true),
          map.get(&(pos - Diff::DX, true)).cloned().unwrap_or(pos),
        );
        map
      })
  }

  pub fn from_grid(grid: Grid<XWordTile>) -> Self {
    let clue_map = Self::build_clue_map(&grid);
    Self { grid, clue_map }
  }

  pub fn grid(&self) -> &Grid<XWordTile> {
    &self.grid
  }

  pub fn width(&self) -> u32 {
    self.grid.width()
  }

  pub fn height(&self) -> u32 {
    self.grid.height()
  }

  pub fn clue_map(&self) -> &HashMap<(Pos, bool), Pos> {
    &self.clue_map
  }

  pub fn tile(&self, pos: Pos) -> TermgameResult<&XWordTile> {
    self
      .grid
      .get(pos)
      .ok_or_else(|| TermgameError::Internal(format!("Pos is out of bounds: {pos}")).into())
  }

  pub fn tile_mut(&mut self, pos: Pos) -> TermgameResult<&mut XWordTile> {
    self.grid.get_mut(pos).ok_or_else(|| {
      TermgameError::Internal(format!("Mutable access pos is out of bounds: {pos}")).into()
    })
  }

  pub fn is_empty(&self, pos: Pos) -> bool {
    self
      .grid
      .get(pos)
      .is_none_or(|tile| matches!(tile, XWordTile::Empty))
  }

  pub fn is_wall(&self, pos: Pos) -> bool {
    self
      .grid
      .get(pos)
      .is_none_or(|tile| matches!(tile, XWordTile::Wall))
  }
}
