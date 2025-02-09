use std::{
  cmp::Ordering,
  collections::{hash_map::Entry, HashMap, HashSet},
  fmt::Display,
  iter,
};

use itertools::Itertools;
use util::{
  error::{TermgameError, TermgameResult},
  grid::{Grid, Gridlike, MutGridlike},
  pos::{Diff, Pos},
};

use dlx::{ColorItem, Constraint, Dlx, DlxIteratorWithNames, HeaderType};

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct XWordClueNumber {
  number: u32,
  is_row: bool,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct XWordCluePosition {
  pos: Pos,
  clue_number: XWordClueNumber,
}

/// Each clue has one CluePos constraint, one Clue constraint, and one Tile
/// constraint per letter in the answer.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum XWordConstraint {
  /// ClueNumber is the clue position identifier this clue would get.
  ClueNumber(XWordClueNumber),
  /// Tiles indicate letters filled in on the board by a clue. These are
  /// secondary (color) constriants, whose color is the character at this tile.
  Tile { pos: Pos },
  /// Clue id: each clue has a unique id. This prevents the same clue from
  /// being used twice.
  Clue { id: u32 },
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct XWordClueAssignment {
  id: u32,
  clue_pos: XWordCluePosition,
}

struct XWordEntry {
  number: u32,
  pos: Pos,
  length: u32,
}

#[derive(Clone, Debug)]
pub struct XWordWord {
  pub word: String,
  pub required: bool,
}

impl XWordWord {
  pub fn new(word: String) -> Self {
    XWordWord {
      word,
      required: false,
    }
  }

  pub fn new_required(word: String) -> Self {
    XWordWord {
      word,
      required: true,
    }
  }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum XWordTile {
  Letter(char),
  Empty,
  Wall,
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

struct LetterFrequencyMap<'a> {
  /// Map from word_length -> ((letter, index) -> count, (set of words))
  frequencies: HashMap<u32, (HashMap<(char, u32), u32>, HashSet<&'a str>)>,
}

impl<'a> LetterFrequencyMap<'a> {
  fn new() -> Self {
    Self {
      frequencies: HashMap::new(),
    }
  }

  fn from_words(words: impl IntoIterator<Item = &'a str>) -> Self {
    let mut map = Self::new();
    for word in words.into_iter() {
      map.insert(word);
    }
    map
  }

  fn insert(&mut self, word: &'a str) {
    let len = word.chars().count() as u32;
    let (char_map, words) = self.frequencies.entry(len).or_default();
    for (idx, letter) in word.chars().enumerate() {
      *char_map.entry((letter, idx as u32)).or_default() += 1;
    }
    words.insert(word);
  }

  fn words_with_length(&self, word_length: u32) -> impl Iterator<Item = &'a str> + '_ {
    self
      .frequencies
      .get(&word_length)
      .map(|(_, words)| words.iter().cloned())
      .into_iter()
      .flatten()
  }

  fn likelihood(&self, word_length: u32, char_pos: (char, u32)) -> f32 {
    self
      .frequencies
      .get(&word_length)
      .map(|(char_map, words)| {
        char_map.get(&char_pos).cloned().unwrap_or(0) as f32 / words.len() as f32
      })
      .unwrap_or(0f32)
  }
}

#[derive(Clone, Debug)]
pub struct XWord {
  board: Grid<bool>,
  required_words: HashMap<u32, String>,
  bank: HashMap<u32, String>,
}

impl XWord {
  pub fn from_grid_with_required(
    board: Grid<bool>,
    required_words: impl IntoIterator<Item = String>,
    bank: impl IntoIterator<Item = String>,
  ) -> TermgameResult<Self> {
    let required_words: HashMap<_, _> = required_words
      .into_iter()
      .enumerate()
      .map(|(id, word)| (id as u32, word))
      .collect();
    let bank = bank
      .into_iter()
      .enumerate()
      .map(|(id, word)| ((id + required_words.len()) as u32, word))
      .collect();

    Ok(Self {
      board,
      required_words,
      bank,
    })
  }

  pub fn from_grid(
    board: Grid<bool>,
    bank: impl IntoIterator<Item = String>,
  ) -> TermgameResult<Self> {
    Self::from_grid_with_required(board, iter::empty(), bank)
  }

  pub fn from_layout_with_required(
    board: &str,
    required_words: HashSet<String>,
    bank: HashSet<String>,
  ) -> TermgameResult<Self> {
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

    Self::from_grid_with_required(board, required_words, bank)
  }

  pub fn from_layout(board: &str, bank: HashSet<String>) -> TermgameResult<Self> {
    Self::from_layout_with_required(board, HashSet::new(), bank)
  }

  #[cfg(test)]
  fn testonly_word_id(&self, word: &str) -> Option<u32> {
    self
      .required_words
      .iter()
      .chain(self.bank.iter())
      .find_map(|(&idx, bank_word)| (word == bank_word).then_some(idx))
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

    impl<I> Iterator for EmptySequences<'_, I>
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

  fn clue_letter_positions<'a>(
    &self,
    clue_pos: &'a XWordCluePosition,
    length: u32,
  ) -> impl Iterator<Item = Pos> + 'a {
    (0..length as i32).map(move |idx| {
      clue_pos.pos
        + if clue_pos.clue_number.is_row {
          Diff { x: idx, y: 0 }
        } else {
          Diff { x: 0, y: idx }
        }
    })
  }

  fn word_letter_positions<'a>(
    &self,
    clue_pos: &'a XWordCluePosition,
    word: &'a str,
  ) -> impl Iterator<Item = (char, Pos)> + 'a {
    let word_len = word.chars().count() as u32;
    word
      .chars()
      .zip(self.clue_letter_positions(clue_pos, word_len))
  }

  fn build_constraints(&self) -> impl Iterator<Item = (XWordConstraint, HeaderType)> + '_ {
    self
      .iterate_col_clues()
      .map(|XWordEntry { number, .. }| {
        (
          XWordConstraint::ClueNumber(XWordClueNumber {
            number,
            is_row: false,
          }),
          HeaderType::Primary,
        )
      })
      .chain(self.iterate_row_clues().map(|XWordEntry { number, .. }| {
        (
          XWordConstraint::ClueNumber(XWordClueNumber {
            number,
            is_row: true,
          }),
          HeaderType::Primary,
        )
      }))
      .chain((0..self.board.height() as i32).flat_map(move |y| {
        (0..self.board.width() as i32).flat_map(move |x| {
          let pos = Pos { x, y };
          self.board.get(pos).and_then(|available| {
            available.then_some((XWordConstraint::Tile { pos }, HeaderType::Secondary))
          })
        })
      }))
      .chain(
        self
          .required_words
          .iter()
          .map(|(&id, _)| (XWordConstraint::Clue { id }, HeaderType::Primary)),
      )
      .chain(
        self
          .bank
          .iter()
          .map(|(&id, _)| (XWordConstraint::Clue { id }, HeaderType::Secondary)),
      )
  }

  /// Returns an iterator over all locations for clues in the board, and the
  /// length of words in that position.
  fn iter_board_entries(&self) -> impl Iterator<Item = (XWordCluePosition, u32)> + '_ {
    self
      .iterate_row_clues()
      .map(
        |XWordEntry {
           number,
           pos,
           length,
         }| {
          (
            XWordCluePosition {
              pos,
              clue_number: XWordClueNumber {
                number,
                is_row: true,
              },
            },
            length,
          )
        },
      )
      .chain(self.iterate_col_clues().map(
        |XWordEntry {
           number,
           pos,
           length,
         }| {
          (
            XWordCluePosition {
              pos,
              clue_number: XWordClueNumber {
                number,
                is_row: false,
              },
            },
            length,
          )
        },
      ))
  }

  fn letter_likelihood_score(
    &self,
    letter: char,
    pos: Pos,
    is_row: bool,
    frequency_map: &LetterFrequencyMap,
  ) -> f32 {
    let diff = if is_row {
      Diff { x: 1, y: 0 }
    } else {
      Diff { x: 0, y: 1 }
    };
    let letter_idx = (1..)
      .take_while(|&delta| {
        self
          .board
          .get(pos - delta * diff)
          .is_some_and(|&empty| empty)
      })
      .count() as u32;
    let word_length = letter_idx
      + 1
      + (1..)
        .take_while(|&delta| {
          self
            .board
            .get(pos + delta * diff)
            .is_some_and(|&empty| empty)
        })
        .count() as u32;

    frequency_map.likelihood(word_length, (letter, letter_idx))
  }

  fn word_likelihood_score(
    &self,
    word: &str,
    clue_pos: &XWordCluePosition,
    frequency_map: &LetterFrequencyMap,
  ) -> f32 {
    self
      .word_letter_positions(clue_pos, word)
      .map(|(letter, pos)| {
        self.letter_likelihood_score(letter, pos, !clue_pos.clue_number.is_row, frequency_map)
      })
      .product()
  }

  fn build_word_assignments(
    &self,
  ) -> impl Iterator<Item = (XWordClueAssignment, Vec<Constraint<XWordConstraint>>)> + '_ {
    let frequency_map = LetterFrequencyMap::from_words(
      self
        .required_words
        .values()
        .chain(self.bank.values())
        .map(|str| str.as_str()),
    );
    let mut word_map: HashMap<_, _> = self
      .required_words
      .iter()
      .map(|(id, word)| (id, word, true))
      .chain(self.bank.iter().map(|(id, word)| (id, word, false)))
      .map(|(&id, word, is_required)| (word.as_str(), (id, is_required, 0)))
      .collect();

    // All constraints are grouped by board entry, and within each category
    // sorted by a "fitness" score of the clue in that position.
    self
      .iter_board_entries()
      .flat_map(move |(clue_pos, length)| {
        let clue_pos_constraint =
          Constraint::Primary(XWordConstraint::ClueNumber(clue_pos.clue_number.clone()));

        frequency_map
          .words_with_length(length)
          .map(|word| {
            let (id, is_required, clue_instance_id) = word_map.get_mut(word).unwrap();
            let id = *id;

            let word_constraint = if *is_required {
              Constraint::Primary(XWordConstraint::Clue { id })
            } else {
              let constraint = Constraint::Secondary(ColorItem::new(
                XWordConstraint::Clue { id },
                *clue_instance_id,
              ));
              *clue_instance_id += 1;
              constraint
            };

            let mut constraints = vec![clue_pos_constraint.clone(), word_constraint];
            constraints.extend(self.word_letter_positions(&clue_pos, word).map(|(c, pos)| {
              Constraint::Secondary(ColorItem::new(XWordConstraint::Tile { pos }, c as u32))
            }));

            (
              (
                XWordClueAssignment {
                  id,
                  clue_pos: clue_pos.clone(),
                },
                constraints,
              ),
              self.word_likelihood_score(word, &clue_pos, &frequency_map),
            )
          })
          .sorted_unstable_by(|(_, score1), (_, score2)| {
            score2.partial_cmp(score1).unwrap_or(Ordering::Equal)
          })
          .map(|(constraints, _)| constraints)
      })
  }

  pub fn build_grid_from_assignments<I>(&self, iter: I) -> TermgameResult<Grid<XWordTile>>
  where
    I: IntoIterator<Item = XWordClueAssignment>,
  {
    let mut answer_grid = self.board.map(|&is_free| {
      if is_free {
        XWordTile::Empty
      } else {
        XWordTile::Wall
      }
    });
    for XWordClueAssignment { id, clue_pos } in iter {
      let word = self
        .required_words
        .get(&id)
        .or_else(|| self.bank.get(&id))
        .ok_or_else(|| TermgameError::Internal(format!("Unknown word id {id}")))?;
      for (c, tile_pos) in self.word_letter_positions(&clue_pos, word) {
        let tile = answer_grid.get_mut(tile_pos).ok_or_else(|| {
          TermgameError::Internal(format!("Position {tile_pos} is out of bounds"))
        })?;
        match tile {
          XWordTile::Letter(existing_c) => {
            if c != *existing_c {
              return Err(
                TermgameError::Internal(format!(
                  "Conflicting letter assignment at position {tile_pos}: {c} vs {existing_c}"
                ))
                .into(),
              );
            }
          }
          XWordTile::Empty => {
            *tile = XWordTile::Letter(c);
          }
          XWordTile::Wall => {
            return Err(TermgameError::Internal(format!("Position {tile_pos} is a wall")).into())
          }
        }
      }
    }

    Ok(answer_grid)
  }

  pub fn build_dlx(&self) -> Dlx<XWordConstraint, XWordClueAssignment> {
    let constraints = self.build_constraints();
    let word_assignments = self.build_word_assignments();
    Dlx::new(constraints, word_assignments)
  }

  pub fn solve(&self) -> TermgameResult<Grid<XWordTile>> {
    self.build_grid_from_assignments(
      self
        .build_dlx()
        .find_solutions()
        .with_names()
        .next()
        .ok_or_else(|| TermgameError::Internal("No solution found".to_owned()))?,
    )
  }
}

#[cfg(test)]
mod tests {
  #![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

  use std::collections::HashSet;

  use dlx::{ColorItem, Constraint, HeaderType};
  use googletest::prelude::*;
  use util::{
    error::TermgameResult,
    grid::{Grid, Gridlike},
    pos::Pos,
  };

  use crate::xword::{
    XWordClueAssignment, XWordClueNumber, XWordCluePosition, XWordConstraint, XWordTile,
  };

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

  #[gtest]
  fn test_constraints() {
    let xword = XWord::from_layout(
      "__
       X_",
      ["ab", "bc"].into_iter().map(|str| str.to_owned()).collect(),
    );

    assert_that!(xword, ok(anything()));
    let xword = xword.unwrap();
    let constraints: Vec<_> = xword.build_constraints().collect();
    expect_that!(
      constraints,
      unordered_elements_are![
        &(
          XWordConstraint::ClueNumber(XWordClueNumber {
            number: 0,
            is_row: true
          }),
          HeaderType::Primary
        ),
        &(
          XWordConstraint::ClueNumber(XWordClueNumber {
            number: 1,
            is_row: true
          }),
          HeaderType::Primary
        ),
        &(
          XWordConstraint::ClueNumber(XWordClueNumber {
            number: 0,
            is_row: false
          }),
          HeaderType::Primary
        ),
        &(
          XWordConstraint::ClueNumber(XWordClueNumber {
            number: 1,
            is_row: false
          }),
          HeaderType::Primary
        ),
        &(
          XWordConstraint::Tile {
            pos: Pos { x: 0, y: 0 }
          },
          HeaderType::Secondary
        ),
        &(
          XWordConstraint::Tile {
            pos: Pos { x: 1, y: 0 }
          },
          HeaderType::Secondary
        ),
        &(
          XWordConstraint::Tile {
            pos: Pos { x: 1, y: 1 }
          },
          HeaderType::Secondary
        ),
        &(XWordConstraint::Clue { id: 0 }, HeaderType::Secondary),
        &(XWordConstraint::Clue { id: 1 }, HeaderType::Secondary),
      ]
    );
  }

  #[gtest]
  fn test_iter_board_entries() -> TermgameResult {
    let xword = XWord::from_layout(
      "____
       _X__",
      HashSet::new(),
    )?;

    expect_that!(
      xword.iter_board_entries().collect::<Vec<_>>(),
      unordered_elements_are![
        &(
          XWordCluePosition {
            pos: Pos { x: 0, y: 0 },
            clue_number: XWordClueNumber {
              number: 0,
              is_row: true
            }
          },
          4
        ),
        &(
          XWordCluePosition {
            pos: Pos { x: 0, y: 1 },
            clue_number: XWordClueNumber {
              number: 1,
              is_row: true
            }
          },
          1
        ),
        &(
          XWordCluePosition {
            pos: Pos { x: 2, y: 1 },
            clue_number: XWordClueNumber {
              number: 2,
              is_row: true
            }
          },
          2
        ),
        &(
          XWordCluePosition {
            pos: Pos { x: 0, y: 0 },
            clue_number: XWordClueNumber {
              number: 0,
              is_row: false
            }
          },
          2
        ),
        &(
          XWordCluePosition {
            pos: Pos { x: 1, y: 0 },
            clue_number: XWordClueNumber {
              number: 1,
              is_row: false
            }
          },
          1
        ),
        &(
          XWordCluePosition {
            pos: Pos { x: 2, y: 0 },
            clue_number: XWordClueNumber {
              number: 2,
              is_row: false
            }
          },
          2
        ),
        &(
          XWordCluePosition {
            pos: Pos { x: 3, y: 0 },
            clue_number: XWordClueNumber {
              number: 3,
              is_row: false
            }
          },
          2
        ),
      ]
    );

    Ok(())
  }

  // #[gtest]
  // fn test_entry_map() {
  //   let xword = XWord::from_layout(
  //     "__
  //      X_",
  //     ["ab", "bc"].into_iter().map(|str| str.to_owned()).collect(),
  //   );

  //   assert_that!(xword, ok(anything()));
  //   let xword = xword.unwrap();
  //   let entry_map = xword.build_entry_map();
  //   expect_that!(
  //     entry_map,
  //     unordered_elements_are![
  //       (
  //         /*length=*/ &1,
  //         &vec![
  //           XWordCluePosition {
  //             pos: Pos { x: 1, y: 1 },
  //             clue_number: XWordClueNumber {
  //               number: 1,
  //               is_row: true
  //             }
  //           },
  //           XWordCluePosition {
  //             pos: Pos::zero(),
  //             clue_number: XWordClueNumber {
  //               number: 0,
  //               is_row: false
  //             }
  //           }
  //         ]
  //       ),
  //       (
  //         /*length=*/ &2,
  //         &vec![
  //           XWordCluePosition {
  //             pos: Pos::zero(),
  //             clue_number: XWordClueNumber {
  //               number: 0,
  //               is_row: true
  //             }
  //           },
  //           XWordCluePosition {
  //             pos: Pos { x: 1, y: 0 },
  //             clue_number: XWordClueNumber {
  //               number: 1,
  //               is_row: false
  //             }
  //           }
  //         ]
  //       ),
  //     ]
  //   );
  // }

  #[gtest]
  fn test_word_assignments() {
    let xword = XWord::from_layout(
      "__
       X_",
      ["ab", "c"].into_iter().map(|str| str.to_owned()).collect(),
    );

    assert_that!(xword, ok(anything()));
    let xword = xword.unwrap();

    let ab_id = xword.testonly_word_id("ab").expect("word ab not found");
    let c_id = xword.testonly_word_id("c").expect("word c not found");

    let word_assignments: Vec<_> = xword.build_word_assignments().collect();
    expect_that!(
      word_assignments,
      unordered_elements_are![
        &(
          XWordClueAssignment {
            id: ab_id,
            clue_pos: XWordCluePosition {
              pos: Pos::zero(),
              clue_number: XWordClueNumber {
                number: 0,
                is_row: true
              }
            }
          },
          vec![
            Constraint::Primary(XWordConstraint::ClueNumber(XWordClueNumber {
              number: 0,
              is_row: true
            })),
            Constraint::Secondary(ColorItem::new(XWordConstraint::Clue { id: ab_id }, 0)),
            Constraint::Secondary(ColorItem::new(
              XWordConstraint::Tile { pos: Pos::zero() },
              'a' as u32
            )),
            Constraint::Secondary(ColorItem::new(
              XWordConstraint::Tile {
                pos: Pos { x: 1, y: 0 }
              },
              'b' as u32
            )),
          ]
        ),
        &(
          XWordClueAssignment {
            id: ab_id,
            clue_pos: XWordCluePosition {
              pos: Pos { x: 1, y: 0 },
              clue_number: XWordClueNumber {
                number: 1,
                is_row: false
              }
            }
          },
          vec![
            Constraint::Primary(XWordConstraint::ClueNumber(XWordClueNumber {
              number: 1,
              is_row: false
            })),
            Constraint::Secondary(ColorItem::new(XWordConstraint::Clue { id: ab_id }, 1)),
            Constraint::Secondary(ColorItem::new(
              XWordConstraint::Tile {
                pos: Pos { x: 1, y: 0 }
              },
              'a' as u32
            )),
            Constraint::Secondary(ColorItem::new(
              XWordConstraint::Tile {
                pos: Pos { x: 1, y: 1 }
              },
              'b' as u32
            )),
          ]
        ),
        &(
          XWordClueAssignment {
            id: c_id,
            clue_pos: XWordCluePosition {
              pos: Pos { x: 1, y: 1 },
              clue_number: XWordClueNumber {
                number: 1,
                is_row: true
              }
            }
          },
          vec![
            Constraint::Primary(XWordConstraint::ClueNumber(XWordClueNumber {
              number: 1,
              is_row: true
            })),
            Constraint::Secondary(ColorItem::new(XWordConstraint::Clue { id: c_id }, 0)),
            Constraint::Secondary(ColorItem::new(
              XWordConstraint::Tile {
                pos: Pos { x: 1, y: 1 }
              },
              'c' as u32
            )),
          ]
        ),
        &(
          XWordClueAssignment {
            id: c_id,
            clue_pos: XWordCluePosition {
              pos: Pos::zero(),
              clue_number: XWordClueNumber {
                number: 0,
                is_row: false
              }
            }
          },
          vec![
            Constraint::Primary(XWordConstraint::ClueNumber(XWordClueNumber {
              number: 0,
              is_row: false
            })),
            Constraint::Secondary(ColorItem::new(XWordConstraint::Clue { id: c_id }, 1)),
            Constraint::Secondary(ColorItem::new(
              XWordConstraint::Tile { pos: Pos::zero() },
              'c' as u32
            )),
          ]
        ),
      ]
    );
  }

  #[gtest]
  fn test_small_dict() {
    let xword = XWord::from_layout(
      "__
       X_",
      ["a", "c", "ab", "bc"]
        .into_iter()
        .map(|str| str.to_owned())
        .collect(),
    );

    assert_that!(xword, ok(anything()));
    let xword = xword.unwrap();
    let solution = xword.solve();
    assert_that!(solution, ok(anything()));
    let solution = solution.unwrap();
    expect_that!(
      solution.get(Pos { x: 0, y: 0 }).cloned(),
      some(any!(&XWordTile::Letter('a'), &XWordTile::Letter('c')))
    );
    expect_that!(
      solution.get(Pos { x: 1, y: 0 }).cloned(),
      some(eq(&XWordTile::Letter('b')))
    );
    expect_that!(
      solution.get(Pos { x: 0, y: 1 }).cloned(),
      some(eq(&XWordTile::Wall))
    );
    expect_that!(
      solution.get(Pos { x: 1, y: 1 }).cloned(),
      some(any!(&XWordTile::Letter('a'), &XWordTile::Letter('c')))
    );
  }

  #[gtest]
  fn test_mini() {
    let xword = XWord::from_layout(
      "X___X
       _____
       _____
       _____
       _____",
      [
        "hug", "korea", "isbns", "snark", "sines", "kiss", "hosni", "urban", "genre", "asks",
      ]
      .into_iter()
      .map(|str| str.to_owned())
      .collect(),
    );

    assert_that!(xword, ok(anything()));
    let xword = xword.unwrap();
    let solution = xword.solve();
    assert_that!(solution, ok(anything()));
    let solution = solution.unwrap();

    use XWordTile::*;

    #[rustfmt::skip]
    let expected_solution = Grid::from_vec(
      vec![
        Wall,        Letter('h'), Letter('u'), Letter('g'), Wall,
        Letter('k'), Letter('o'), Letter('r'), Letter('e'), Letter('a'),
        Letter('i'), Letter('s'), Letter('b'), Letter('n'), Letter('s'),
        Letter('s'), Letter('n'), Letter('a'), Letter('r'), Letter('k'),
        Letter('s'), Letter('i'), Letter('n'), Letter('e'), Letter('s'),
      ], 5, 5,
    ).unwrap();
    expect_eq!(solution, expected_solution);
  }
}
