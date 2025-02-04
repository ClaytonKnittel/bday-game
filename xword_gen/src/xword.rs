use std::collections::HashSet;

use util::{
  error::{TermgameError, TermgameResult},
  grid::{Grid, Gridlike},
  pos::Pos,
};

use crate::dlx::{ColorItem, Constraint, Dlx, HeaderType};

/// Each clue has one RowClue or ColClue constraint, one Clue constraint, and
/// one Tile constraint per letter in the answer.
enum XWordConstraint {
  /// RowClue is the row clue number this clue would get.
  RowClue { number: u32 },
  /// ColClue is the column clue number this clue would get.
  ColClue { number: u32 },
  /// Tiles indicate letters filled in on the board by a clue. These are
  /// secondary (color) constriants.
  Tile { pos: Pos, c: char },
  /// Clue number: each clue has a unique number. This prevents the same clue
  /// from being used twice.
  Clue { number: u32 },
}

struct XWordEntry {
  number: u32,
  pos: Pos,
  length: u32,
}

#[derive(Clone, Debug)]
pub struct XWord {
  board: Grid<bool>,
  bank: HashSet<String>,
}

impl XWord {
  pub fn from_layout(board: &str, bank: HashSet<String>) -> TermgameResult<Self> {
    let (width, height, board) = board.lines().try_fold(
      (None, 0, vec![]),
      |(width, height, mut board), line| -> TermgameResult<_> {
        let line = line.trim();
        board.extend(
          line
            .chars()
            .map(|c| match c {
              '_' => Ok(true),
              'X' => Ok(false),
              _ => Err(TermgameError::Parse(format!("Unrecognized board character '{c}'")).into()),
            })
            .collect::<TermgameResult<Vec<_>>>()?,
        );
        if let Some(width) = width {
          if line.len() != width {
            return Err(
              TermgameError::Parse(format!(
                "Board line lengths differ: {} vs {width}",
                line.len()
              ))
              .into(),
            );
          }
        }

        Ok((Some(line.len()), height + 1, board))
      },
    )?;

    let width = width.ok_or_else(|| TermgameError::Parse("Empty board string".to_owned()))? as u32;
    let board = Grid::from_vec(board, width, height as u32)?;

    Ok(Self { board, bank })
  }

  pub fn available(&self, pos: Pos) -> bool {
    self.board.get(pos).is_some_and(|&available| available)
  }

  fn iterate_board_row_clues<'a, G: Gridlike<bool> + 'a>(
    board: G,
  ) -> impl Iterator<Item = XWordEntry> + 'a {
    struct EmptySequences<'a, I> {
      x: u32,
      y: u32,
      clue_number: &'a mut u32,
      iter: Option<I>,
    }

    impl<'a, I> Iterator for EmptySequences<'a, I>
    where
      I: Iterator<Item = bool>,
    {
      type Item = XWordEntry;

      fn next(&mut self) -> Option<XWordEntry> {
        let iter = self.iter.as_mut()?;
        let number = *self.clue_number;

        loop {
          self.x += 1;
          match iter.next() {
            Some(true) => break,
            Some(false) => {}
            None => return None,
          }
        }
        *self.clue_number += 1;
        let mut length = 1;
        let pos = Pos {
          x: (self.x - 1) as i32,
          y: self.y as i32,
        };

        loop {
          self.x += 1;
          match iter.next() {
            Some(true) => length += 1,
            Some(false) => break,
            None => {
              self.iter = None;
              break;
            }
          }
        }

        Some(XWordEntry {
          number,
          pos,
          length,
        })
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

  fn iterate_row_clues(&self) -> impl Iterator<Item = XWordEntry> + use<'_> {
    Self::iterate_board_row_clues(&self.board)
  }

  fn iterate_col_clues(&self) -> impl Iterator<Item = XWordEntry> + use<'_> {
    Self::iterate_board_row_clues(self.board.transpose()).map(|entry| XWordEntry {
      pos: entry.pos.transpose(),
      ..entry
    })
  }

  pub fn solve(&self) -> Grid<Option<char>> {
    let mut dlx = Dlx::new(
      vec![
        ('p', HeaderType::Primary),
        ('q', HeaderType::Primary),
        ('a', HeaderType::Secondary),
      ],
      vec![
        (
          0,
          vec![Constraint::Primary('p'), ColorItem::new('a', 1).into()],
        ),
        (1, vec!['p'.into(), ColorItem::new('a', 2).into()]),
        (2, vec!['q'.into(), ColorItem::new('a', 3).into()]),
        (3, vec!['q'.into(), ColorItem::new('a', 1).into()]),
      ],
    );
    todo!();
  }
}

#[cfg(test)]
mod tests {
  #![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

  use std::collections::HashSet;

  use googletest::prelude::*;
  use util::{grid::Gridlike, pos::Pos};

  use super::XWord;

  #[gtest]
  fn test_empty() {
    let xword = XWord::from_layout("", HashSet::new());
    expect_that!(xword, err(anything()));
  }

  #[gtest]
  fn test_available() {
    let xword = XWord::from_layout(
      "__
       X_",
      HashSet::new(),
    );

    assert_that!(xword, ok(anything()));
    let xword = xword.unwrap();
    expect_true!(xword.available(Pos { x: 0, y: 0 }));
    expect_true!(xword.available(Pos { x: 1, y: 0 }));
    expect_false!(xword.available(Pos { x: 0, y: 1 }));
    expect_true!(xword.available(Pos { x: 1, y: 1 }));
  }

  #[gtest]
  fn test_small_dict() {
    let xword = XWord::from_layout(
      "__
       X_",
      ["ab", "bc"].into_iter().map(|str| str.to_owned()).collect(),
    );

    assert_that!(xword, ok(anything()));
    let xword = xword.unwrap();
    let solution = xword.solve();
    expect_that!(
      solution.get(Pos { x: 0, y: 0 }).cloned().flatten(),
      some(any!('a', 'c'))
    );
    expect_that!(
      solution.get(Pos { x: 1, y: 0 }).cloned().flatten(),
      some(eq('b'))
    );
    expect_that!(
      solution.get(Pos { x: 1, y: 1 }).cloned().flatten(),
      some(any!('a', 'c'))
    );
  }

  #[gtest]
  fn test_iterate_rows() {
    let xword = XWord::from_layout(
      "__
       X_",
      HashSet::new(),
    );

    assert_that!(xword, ok(anything()));
    let xword = xword.unwrap();
    expect_that!(
      xword
        .iterate_row_clues()
        .map(|clue| clue.number)
        .collect::<Vec<_>>(),
      container_eq([0, 1])
    );
    expect_that!(
      xword
        .iterate_row_clues()
        .map(|clue| clue.length)
        .collect::<Vec<_>>(),
      container_eq([2, 1])
    );
    expect_that!(
      xword
        .iterate_row_clues()
        .map(|clue| clue.pos)
        .collect::<Vec<_>>(),
      container_eq([Pos::zero(), Pos { x: 1, y: 1 }])
    );
  }

  #[gtest]
  fn test_iterate_cols() {
    let xword = XWord::from_layout(
      "__
       X_",
      HashSet::new(),
    );

    assert_that!(xword, ok(anything()));
    let xword = xword.unwrap();
    expect_that!(
      xword
        .iterate_col_clues()
        .map(|clue| clue.number)
        .collect::<Vec<_>>(),
      container_eq([0, 1])
    );
    expect_that!(
      xword
        .iterate_col_clues()
        .map(|clue| clue.length)
        .collect::<Vec<_>>(),
      container_eq([1, 2])
    );
    expect_that!(
      xword
        .iterate_col_clues()
        .map(|clue| clue.pos)
        .collect::<Vec<_>>(),
      container_eq([Pos::zero(), Pos { x: 1, y: 0 }])
    );
  }
}
