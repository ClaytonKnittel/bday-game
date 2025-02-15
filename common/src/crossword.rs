use std::{collections::HashMap, fmt::Display};

use bitcode::{Decode, Encode};
use itertools::Itertools;
use util::{
  error::{TermgameError, TermgameResult},
  grid::{Grid, Gridlike, MutGridlike},
  pos::{Diff, Pos},
};
use xword_dict::XWordDict;

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct XWordClueNumber {
  pub number: u32,
  pub is_row: bool,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct XWordCluePosition {
  pub pos: Pos,
  pub clue_number: XWordClueNumber,
}

#[derive(Clone, Debug, PartialEq, Eq, Encode, Decode)]
pub enum XWordTile {
  Letter(char),
  Empty,
  Wall,
}

impl XWordTile {
  pub fn empty(&self) -> bool {
    matches!(self, XWordTile::Empty)
  }

  pub fn available(&self) -> bool {
    matches!(self, XWordTile::Empty | XWordTile::Letter(_))
  }
}

impl Display for XWordTile {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "{}",
      match self {
        XWordTile::Letter(c) => *c,
        XWordTile::Empty => '_',
        XWordTile::Wall => '*',
      }
    )
  }
}

pub struct XWordEntry {
  pub number: u32,
  pub pos: Pos,
  pub length: u32,
}

#[derive(Clone, Debug, Encode, Decode)]
pub struct Clue {
  pub clue_entries: Vec<String>,
  pub clue_num: u32,
}

#[derive(Clone)]
pub struct Crossword {
  grid: Grid<XWordTile>,
  clue_pos_map: HashMap<(Pos, bool), Pos>,
  clue_map: HashMap<(Pos, bool), Clue>,
}

impl Crossword {
  fn iterate_board_row_clues<'a, G: Gridlike<XWordTile> + 'a>(
    board: G,
  ) -> impl Iterator<Item = XWordEntry> + 'a {
    struct EmptySequences<'a, I> {
      x: u32,
      y: u32,
      clue_number: &'a mut u32,
      iter: Option<I>,
    }

    impl<I> Iterator for EmptySequences<'_, I>
    where
      I: Iterator<Item = XWordTile>,
    {
      type Item = XWordEntry;

      fn next(&mut self) -> Option<XWordEntry> {
        let iter = self.iter.as_mut()?;
        let number = *self.clue_number;

        loop {
          self.x += 1;
          match iter.next() {
            Some(XWordTile::Empty | XWordTile::Letter(_)) => break,
            Some(XWordTile::Wall) => {}
            None => return None,
          }
        }
        *self.clue_number += 1;
        let mut length = 1;
        let pos = Pos { x: (self.x - 1) as i32, y: self.y as i32 };

        loop {
          self.x += 1;
          match iter.next() {
            Some(XWordTile::Empty | XWordTile::Letter(_)) => length += 1,
            Some(XWordTile::Wall) => break,
            None => {
              self.iter = None;
              break;
            }
          }
        }

        Some(XWordEntry { number, pos, length })
      }
    }

    (0..board.height())
      .scan(0, move |clue_number, y| {
        let result: Vec<_> = EmptySequences {
          x: 0,
          y,
          clue_number,
          iter: Some(board.iter_row(y).cloned()),
        }
        .collect();
        Some(result.into_iter())
      })
      .flatten()
  }

  pub fn iterate_row_clues(grid: &Grid<XWordTile>) -> impl Iterator<Item = XWordEntry> + '_ {
    Self::iterate_board_row_clues(grid)
  }

  pub fn iterate_col_clues(grid: &Grid<XWordTile>) -> impl Iterator<Item = XWordEntry> + '_ {
    Self::iterate_board_row_clues(grid.transpose())
      .map(|entry| XWordEntry { pos: entry.pos.transpose(), ..entry })
  }

  pub fn clue_letter_positions_unbounded<'a>(
    clue_pos: XWordCluePosition,
  ) -> impl Iterator<Item = Pos> + 'a {
    (0..).map(move |idx| {
      clue_pos.pos
        + if clue_pos.clue_number.is_row {
          Diff { x: idx, y: 0 }
        } else {
          Diff { x: 0, y: idx }
        }
    })
  }

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

  pub fn from_grid(grid: Grid<XWordTile>, clue_map: HashMap<(Pos, bool), Clue>) -> Self {
    let clue_pos_map = Self::build_clue_map(&grid);
    Self { grid, clue_pos_map, clue_map }
  }

  pub fn make_clues(grid: Grid<XWordTile>, dictionary: &XWordDict) -> TermgameResult<Self> {
    let clue_map = Self::iterate_row_clues(&grid)
      .map(|clue| (clue, true))
      .chain(Self::iterate_col_clues(&grid).map(|clue| (clue, false)))
      .map(|(clue, is_row)| {
        let word = Self::clue_letter_positions_unbounded(XWordCluePosition {
          pos: clue.pos,
          clue_number: XWordClueNumber { number: clue.number, is_row },
        })
        .take(clue.length as usize)
        .try_fold("".to_owned(), |mut word, pos| match grid.get(pos) {
          Some(XWordTile::Letter(letter)) => {
            word.push(*letter);
            Ok(word)
          }
          _ => Err(TermgameError::Internal(format!(
            "Position {pos} in crossword is not an assigned letter."
          ))),
        })?;

        let clue_entry = dictionary.get_clue(&word);
        let clue_entries = clue_entry.map(|entry| entry.clue.clone()).collect_vec();

        Ok((
          (clue.pos, is_row),
          Clue { clue_entries, clue_num: clue.number },
        ))
      })
      .collect::<TermgameResult<_>>()?;
    Ok(Self::from_grid(grid, clue_map))
  }

  pub fn clone_clearing_tiles(&self) -> Crossword {
    let grid = self.grid.map(|tile| match tile {
      XWordTile::Empty | XWordTile::Letter(_) => XWordTile::Empty,
      XWordTile::Wall => XWordTile::Wall,
    });
    Self::from_grid(grid, self.clue_map.clone())
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

  pub fn clue_pos_map(&self) -> &HashMap<(Pos, bool), Pos> {
    &self.clue_pos_map
  }

  pub fn clue_map(&self) -> &HashMap<(Pos, bool), Clue> {
    &self.clue_map
  }

  pub fn clue_for_pos(&self, pos: Pos, is_row: bool) -> Option<&Clue> {
    self
      .clue_pos_map
      .get(&(pos, is_row))
      .and_then(|pos| self.clue_map.get(&(*pos, is_row)))
  }

  pub fn clue_for_pos_mut(&mut self, pos: Pos, is_row: bool) -> Option<&mut Clue> {
    self
      .clue_pos_map
      .get_mut(&(pos, is_row))
      .and_then(|pos| self.clue_map.get_mut(&(*pos, is_row)))
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

#[derive(Clone, Debug, Encode, Decode)]
pub struct CrosswordEncoding {
  grid: Grid<XWordTile>,
  clue_map: HashMap<(Pos, bool), Clue>,
}

impl From<Crossword> for CrosswordEncoding {
  fn from(value: Crossword) -> Self {
    Self {
      grid: value.grid,
      clue_map: value.clue_map,
    }
  }
}

impl From<CrosswordEncoding> for Crossword {
  fn from(value: CrosswordEncoding) -> Self {
    Self::from_grid(value.grid, value.clue_map)
  }
}
