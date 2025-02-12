use std::{
  cmp::Ordering,
  collections::{HashMap, HashSet},
  fmt::Display,
  iter::{self, once},
};

use itertools::Itertools;
use util::{
  bitcode::{Decode, Encode},
  error::{TermgameError, TermgameResult},
  grid::{Grid, Gridlike, MutGridlike},
  pos::{Diff, Pos},
  union_find::UnionFind,
  variant::Variant2,
};

use dlx::{ColorItem, Constraint, Dlx, DlxIteratorWithNames, HeaderType, StepwiseDlxIterResult};

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct XWordClueNumber {
  number: u32,
  is_row: bool,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct XWordCluePosition {
  pos: Pos,
  clue_number: XWordClueNumber,
}

/// Tiles indicate letters filled in on the board by a clue. Each letter
/// placement by a horizontal/vertical clue selects 5 of the 10 constraints
/// for that letter at that position, with the opposing horizontal/vertical
/// clue choosing the same letter taking the complement 5 constraints.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct XWordTileConstraint {
  pos: Pos,
  bit: u32,
}

impl XWordTileConstraint {
  fn into_constraint(self) -> (XWordConstraint, HeaderType) {
    (XWordConstraint::Tile(self), HeaderType::Primary)
  }

  fn into_item_constraint(self) -> Constraint<XWordConstraint> {
    Constraint::Primary(XWordConstraint::Tile(self))
  }
}

const NUM_TILE_BITS: u32 = 10;

/// Each clue has one CluePos constraint, one Clue constraint, and one Tile
/// constraint per letter in the answer.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum XWordConstraint {
  /// ClueNumber is the clue position identifier this clue would get.
  ClueNumber(XWordClueNumber),
  Tile(XWordTileConstraint),
  /// Clue id: each clue has a unique id. This prevents the same clue from
  /// being used twice.
  Clue {
    id: u32,
  },
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
    XWordWord { word, required: false }
  }

  pub fn new_required(word: String) -> Self {
    XWordWord { word, required: true }
  }
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

type LetterFrequencyMapEntry<'a> = (HashMap<(char, u32), u32>, HashSet<&'a str>);
struct LetterFrequencyMap<'a> {
  /// Map from word_length -> ((letter, index) -> count, (set of words))
  frequencies: HashMap<u32, LetterFrequencyMapEntry<'a>>,
}

impl<'a> LetterFrequencyMap<'a> {
  fn new() -> Self {
    Self { frequencies: HashMap::new() }
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

#[derive(Default)]
struct ProblemParameters {
  constraints: Vec<(XWordConstraint, HeaderType)>,
  word_assignments: Vec<(XWordClueAssignment, Vec<Constraint<XWordConstraint>>)>,
}

impl ProblemParameters {
  fn build_dlx(self) -> Dlx<XWordConstraint, XWordClueAssignment> {
    Dlx::new(self.constraints, self.word_assignments)
  }
}

trait XWordInternal {
  fn words(&self) -> impl Iterator<Item = (u32, &'_ str)>;
  fn find_word(&self, word_id: u32) -> Option<&'_ str>;
  fn num_words(&self) -> u32;

  fn universal_words(&self) -> impl Iterator<Item = &'_ str>;

  fn board(&self) -> &Grid<XWordTile>;

  fn should_fill_board() -> bool;
  fn word_constraint_type() -> HeaderType;

  #[cfg(test)]
  fn testonly_word_id(&self, word: &str) -> Option<u32> {
    self
      .words()
      .find_map(|(idx, bank_word)| (word == bank_word).then_some(idx))
  }

  fn empty(&self, pos: Pos) -> bool {
    self
      .board()
      .get(pos)
      .is_some_and(|tile| matches!(tile, XWordTile::Empty))
  }

  fn available(&self, pos: Pos) -> bool {
    self.board().get(pos).is_some_and(|tile| tile.available())
  }

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

  fn iterate_row_clues(&self) -> impl Iterator<Item = XWordEntry> + '_ {
    Self::iterate_board_row_clues(self.board())
  }

  fn iterate_col_clues(&self) -> impl Iterator<Item = XWordEntry> + '_ {
    Self::iterate_board_row_clues(self.board().transpose())
      .map(|entry| XWordEntry { pos: entry.pos.transpose(), ..entry })
  }

  fn clue_letter_positions_unbounded<'a>(
    &self,
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

  fn clue_letter_positions<'a>(
    &self,
    clue_pos: XWordCluePosition,
    length: u32,
  ) -> impl Iterator<Item = Pos> + 'a {
    debug_assert!((0..length as i32).all(|idx| {
      self.available(
        clue_pos.pos
          + if clue_pos.clue_number.is_row {
            Diff { x: idx, y: 0 }
          } else {
            Diff { x: 0, y: idx }
          },
      )
    }));

    self
      .clue_letter_positions_unbounded(clue_pos)
      .take(length as usize)
  }

  fn find_empty_word_tile(&self, clue_position: XWordCluePosition, length: u32) -> Option<Pos> {
    self
      .clue_letter_positions(clue_position, length)
      .find(|&pos| self.empty(pos))
  }

  fn word_letter_positions<'a>(
    &self,
    clue_pos: XWordCluePosition,
    word: &'a str,
  ) -> impl Iterator<Item = (char, Pos)> + 'a {
    let word_len = word.chars().count() as u32;
    word
      .chars()
      .zip(self.clue_letter_positions(clue_pos, word_len))
  }

  fn tile_constraints_for_pos(&self, pos: Pos) -> impl Iterator<Item = XWordTileConstraint> + '_ {
    debug_assert!(self.board().get(pos).is_some_and(|tile| tile.empty()));
    (0..NUM_TILE_BITS).map(move |bit| XWordTileConstraint { pos, bit })
  }

  fn build_clue_constraints(&self) -> impl Iterator<Item = (XWordConstraint, HeaderType)> + '_ {
    self
      .words()
      .map(|(id, _)| (XWordConstraint::Clue { id }, Self::word_constraint_type()))
  }

  /// Returns an iterator over all locations for clues in the board, and the
  /// length of words in that position.
  fn iter_board_entries(&self) -> impl Iterator<Item = (XWordCluePosition, u32)> + '_ {
    self
      .iterate_row_clues()
      .map(|XWordEntry { number, pos, length }| {
        (
          XWordCluePosition {
            pos,
            clue_number: XWordClueNumber { number, is_row: true },
          },
          length,
        )
      })
      .chain(
        self
          .iterate_col_clues()
          .map(|XWordEntry { number, pos, length }| {
            (
              XWordCluePosition {
                pos,
                clue_number: XWordClueNumber { number, is_row: false },
              },
              length,
            )
          }),
      )
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
          .board()
          .get(pos - delta * diff)
          .is_some_and(|tile| tile.available())
      })
      .count() as u32;
    let word_length = letter_idx
      + 1
      + (1..)
        .take_while(|&delta| {
          self
            .board()
            .get(pos + delta * diff)
            .is_some_and(|tile| tile.available())
        })
        .count() as u32;

    frequency_map.likelihood(word_length, (letter, letter_idx))
  }

  fn word_likelihood_score(
    &self,
    word: &str,
    clue_pos: XWordCluePosition,
    frequency_map: &LetterFrequencyMap,
  ) -> f32 {
    self
      .word_letter_positions(clue_pos, word)
      .map(|(letter, pos)| {
        self.letter_likelihood_score(letter, pos, !clue_pos.clue_number.is_row, frequency_map)
      })
      .product()
  }

  fn letter_tile_constraints(
    pos: Pos,
    letter: char,
    is_row: bool,
  ) -> impl Iterator<Item = XWordTileConstraint> {
    debug_assert!(letter.is_ascii_lowercase());
    let bits = (letter as u32) - b'a' as u32;
    (0..5).map(move |bit_idx| XWordTileConstraint {
      pos,
      bit: 2 * bit_idx
        + if is_row {
          (bits >> bit_idx) & 0x1
        } else {
          1 - ((bits >> bit_idx) & 0x1)
        },
    })
  }

  fn build_frequency_map(&self) -> LetterFrequencyMap {
    LetterFrequencyMap::from_words(self.universal_words())
  }

  fn build_word_assignments_from_entries(
    &self,
    board_entries: impl Iterator<Item = (XWordCluePosition, u32)>,
  ) -> impl Iterator<Item = (XWordClueAssignment, Vec<Constraint<XWordConstraint>>)> + '_ {
    let frequency_map = self.build_frequency_map();
    let mut word_map: HashMap<_, _> = self.words().map(|(id, word)| (word, (id, 0))).collect();

    // All constraints are grouped by board entry, and within each category
    // sorted by a "fitness" score of the clue in that position.
    board_entries
      .flat_map(move |(clue_pos, length)| -> Vec<_> {
        let clue_pos_constraint =
          Constraint::Primary(XWordConstraint::ClueNumber(clue_pos.clue_number));

        frequency_map
          .words_with_length(length)
          .filter(|word| {
            self
              .word_letter_positions(clue_pos, word)
              .all(|(c, pos)| match self.board().get(pos) {
                Some(XWordTile::Empty) => true,
                Some(&XWordTile::Letter(letter)) => letter == c,
                _ => unreachable!(),
              })
          })
          .map(|word| {
            let (id, clue_instance_id) = word_map.get_mut(word).unwrap();
            let id = *id;

            let xword_clue = XWordConstraint::Clue { id };
            let word_constraint = match Self::word_constraint_type() {
              HeaderType::Primary => Constraint::Primary(xword_clue),
              HeaderType::Secondary => {
                let constraint =
                  Constraint::Secondary(ColorItem::new(xword_clue, *clue_instance_id));
                *clue_instance_id += 1;
                constraint
              }
            };

            let mut constraints = vec![clue_pos_constraint.clone(), word_constraint];
            constraints.extend(
              self
                .word_letter_positions(clue_pos, word)
                .flat_map(|(c, pos)| {
                  Self::letter_tile_constraints(pos, c, clue_pos.clue_number.is_row)
                    .map(XWordTileConstraint::into_item_constraint)
                }),
            );

            (
              (XWordClueAssignment { id, clue_pos }, constraints),
              self.word_likelihood_score(word, clue_pos, &frequency_map),
            )
          })
          .collect()
      })
      .sorted_unstable_by(|(_, score1), (_, score2)| {
        score2.partial_cmp(score1).unwrap_or(Ordering::Equal)
      })
      .map(|(constraints, _)| constraints)
  }

  /// Returns an iterator over all tile constraints for a given clue position.
  fn clue_tile_constraints(
    &self,
    clue_pos: XWordCluePosition,
    length: u32,
  ) -> impl Iterator<Item = (XWordConstraint, HeaderType)> + '_ {
    let is_row = clue_pos.clue_number.is_row;

    self
      .clue_letter_positions(clue_pos, length)
      .flat_map(move |pos| {
        match self.board().get(pos) {
          Some(&XWordTile::Letter(letter)) => Variant2::Opt1(
            Self::letter_tile_constraints(pos, letter, is_row)
              .map(XWordTileConstraint::into_constraint),
          ),
          Some(&XWordTile::Empty) => {
            // Empty tiles are always intersected by a row and a col clue, we
            // arbitrarily choose the row clue to insert into constraints (if
            // we allowed both to, there would be duplicates).
            Variant2::Opt2(
              is_row
                .then(|| {
                  self
                    .tile_constraints_for_pos(pos)
                    .map(XWordTileConstraint::into_constraint)
                })
                .into_iter()
                .flatten(),
            )
          }
          _ => unreachable!(),
        }
      })
  }

  /// Returns an iterator over (clue_position, word_length, constraints) for all clues.
  fn build_tile_constraints(
    &self,
  ) -> impl Iterator<
    Item = (
      XWordCluePosition,
      u32,
      impl Iterator<Item = (XWordConstraint, HeaderType)> + '_,
    ),
  > + '_ {
    self
      .iterate_row_clues()
      .map(|entry| (entry, true))
      .chain(self.iterate_col_clues().map(|entry| (entry, false)))
      .map(|(XWordEntry { number, pos, length }, is_row)| {
        let clue_number = XWordClueNumber { number, is_row };
        let clue_pos = XWordCluePosition { clue_number, pos };
        (
          clue_pos,
          length,
          self
            .clue_tile_constraints(clue_pos, length)
            .chain(iter::once((
              XWordConstraint::ClueNumber(clue_number),
              // TODO make
              HeaderType::Primary,
            ))),
        )
      })
  }

  fn build_grid_from_assignments<I>(
    &self,
    mut answer_grid: Grid<XWordTile>,
    iter: I,
  ) -> TermgameResult<Grid<XWordTile>>
  where
    I: IntoIterator<Item = XWordClueAssignment>,
  {
    for XWordClueAssignment { id, clue_pos } in iter {
      let word = self
        .find_word(id)
        .ok_or_else(|| TermgameError::Internal(format!("Unknown word id {id}")))?;
      for (c, tile_pos) in self.word_letter_positions(clue_pos, word) {
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
}

#[derive(Clone, Debug)]
pub struct XWord {
  board: Grid<XWordTile>,
  bank: HashMap<u32, String>,
}

impl XWord {
  pub fn from_grid(
    board: Grid<XWordTile>,
    bank: impl IntoIterator<Item = String>,
  ) -> TermgameResult<Self> {
    let bank = bank
      .into_iter()
      .enumerate()
      .map(|(id, word)| (id as u32, word))
      .collect();

    Ok(Self { board, bank })
  }

  pub fn build_grid(board: &str) -> TermgameResult<Grid<XWordTile>> {
    let (width, height, board) = board.lines().try_fold(
      (None, 0, vec![]),
      |(width, height, mut board), line| -> TermgameResult<_> {
        let line = line.trim();
        board.extend(
          line
            .chars()
            .map(|c| match c {
              '_' => Ok(XWordTile::Empty),
              'X' => Ok(XWordTile::Wall),
              'a'..='z' => Ok(XWordTile::Letter(c)),
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
    Grid::from_vec(board, width, height as u32)
  }

  fn entries_for_partition<'a>(
    &'a self,
    partition_id: Pos,
    uf: &'a UnionFind<Pos>,
  ) -> impl Iterator<Item = (XWordCluePosition, u32)> + 'a {
    self.iter_board_entries().filter(move |(clue_pos, length)| {
      self
        .find_empty_word_tile(clue_pos.pos, clue_pos.clue_number, *length)
        .is_some_and(|empty_pos| uf.find_immut(empty_pos) == partition_id)
    })
  }

  fn build_partitioned_word_assignments<'a>(
    &'a self,
    uf: &'a UnionFind<Pos>,
  ) -> impl Iterator<
    Item = (
      Pos,
      impl Iterator<Item = (XWordClueAssignment, Vec<Constraint<XWordConstraint>>)> + 'a,
    ),
  > + 'a {
    uf.root_level_keys().into_iter().map(move |partition_id| {
      (
        partition_id,
        self.build_word_assignments_from_entries(self.entries_for_partition(partition_id, uf)),
      )
    })
  }

  fn build_partition_uf(&self) -> UnionFind<Pos> {
    let mut uf = UnionFind::from_keys(self.board().positions().filter(|&pos| self.empty(pos)));
    for (XWordEntry { number, pos, length }, is_row) in self
      .iterate_row_clues()
      .map(|entry| (entry, true))
      .chain(self.iterate_col_clues().map(|entry| (entry, false)))
    {
      for (pos1, pos2) in self
        .clue_letter_positions(
          XWordCluePosition {
            pos,
            clue_number: XWordClueNumber { number, is_row },
          },
          length,
        )
        .filter(|&pos| self.empty(pos))
        .tuple_windows()
      {
        uf.union(pos1, pos2);
      }
    }

    // Debug check that all clues are self-consistent.
    for (entry, is_row) in self
      .iterate_row_clues()
      .map(|entry| (entry, true))
      .chain(self.iterate_col_clues().map(|entry| (entry, false)))
    {
      let XWordEntry { number, pos, length } = entry;
      debug_assert!(self
        .clue_letter_positions(
          XWordCluePosition {
            pos,
            clue_number: XWordClueNumber { number, is_row }
          },
          length
        )
        .filter(|&pos| self.empty(pos))
        .map(|pos| { uf.find(pos) })
        .all_equal());
    }

    uf
  }

  fn build_partitioned_subproblems(&self) -> HashMap<Pos, ProblemParameters> {
    let mut uf = self.build_partition_uf();

    let mut subproblem_map = HashMap::<Pos, ProblemParameters>::new();
    for (pos, constraints) in
      self
        .build_tile_constraints()
        .filter_map(|(clue_position, length, constraints)| {
          self
            .find_empty_word_tile(clue_position, length)
            .map(|pos| (pos, constraints))
        })
    {
      let uf_id = uf.find(pos);
      subproblem_map
        .entry(uf_id)
        .or_default()
        .constraints
        .extend(constraints);
    }

    for (partition_id, assignments) in self.build_partitioned_word_assignments(&mut uf) {
      let subproblem = subproblem_map.entry(partition_id).or_default();
      subproblem.word_assignments.extend(assignments);
    }

    subproblem_map
      .values_mut()
      .for_each(|ProblemParameters { constraints, .. }| {
        constraints.extend(self.build_clue_constraints());
      });

    subproblem_map
  }

  pub fn build_dlx_solvers(&self) -> HashMap<Pos, Dlx<XWordConstraint, XWordClueAssignment>> {
    self
      .build_partitioned_subproblems()
      .into_iter()
      .map(|(pos, params)| (pos, params.build_dlx()))
      .collect()
  }

  pub fn solve(&self) -> TermgameResult<Grid<XWordTile>> {
    self
      .build_dlx_solvers()
      .into_values()
      .try_fold(self.board().clone(), |board, mut dlx| {
        self.build_grid_from_assignments(
          board,
          dlx
            .find_solutions()
            .with_names()
            .next()
            .ok_or_else(|| TermgameError::Internal("No solution found".to_owned()))?,
        )
      })
  }

  pub fn solve_parallel(&self) -> TermgameResult<Grid<XWordTile>> {
    // Should use tokio primitives
    todo!();
  }

  pub fn stepwise_board_iter(&self) -> impl Iterator<Item = Grid<XWordTile>> + '_ {
    enum IterOrSolution<I> {
      Iter(I),
      Solution(Vec<XWordClueAssignment>),
    }

    let dlx_iters: Vec<_> = self
      .build_dlx_solvers()
      .into_values()
      .map(|dlx| IterOrSolution::Iter(dlx.into_solutions_stepwise().with_names()))
      .collect();

    once(()).cycle().scan(dlx_iters, move |dlx_iters, _| {
      if dlx_iters
        .iter()
        .all(|iter| matches!(iter, IterOrSolution::Solution(_)))
      {
        return None;
      }

      let selected_items = dlx_iters.iter_mut().flat_map(|iter| match iter {
        IterOrSolution::Iter(dlx_iter) => {
          if let Some(result) = dlx_iter.next() {
            match result {
              StepwiseDlxIterResult::Step(solution) => solution,
              StepwiseDlxIterResult::Solution(solution) => {
                *iter = IterOrSolution::Solution(solution.clone());
                solution
              }
            }
          } else {
            *iter = IterOrSolution::Solution(vec![]);
            vec![]
          }
        }
        IterOrSolution::Solution(solution) => solution.clone(),
      });
      Some(
        self
          .build_grid_from_assignments(self.board().clone(), selected_items)
          .unwrap(),
      )
    })
  }
}

impl XWordInternal for XWord {
  fn words(&self) -> impl Iterator<Item = (u32, &'_ str)> {
    self.bank.iter().map(|(&id, word)| (id, word.as_str()))
  }

  fn find_word(&self, word_id: u32) -> Option<&'_ str> {
    self.bank.get(&word_id).map(|str| str.as_str())
  }

  fn num_words(&self) -> u32 {
    self.bank.len() as u32
  }

  fn universal_words(&self) -> impl Iterator<Item = &'_ str> {
    self.words().map(|(_, word)| word)
  }

  fn board(&self) -> &Grid<XWordTile> {
    &self.board
  }

  fn should_fill_board() -> bool {
    true
  }

  fn word_constraint_type() -> HeaderType {
    // No words are required to be used.
    HeaderType::Secondary
  }
}

#[derive(Clone, Debug)]
pub struct XWordWithRequired {
  xword: XWord,
  required_words: HashMap<u32, String>,
}

impl XWordWithRequired {
  pub fn from_grid(
    board: Grid<XWordTile>,
    required_words: impl IntoIterator<Item = String>,
    bank: impl IntoIterator<Item = String>,
  ) -> TermgameResult<Self> {
    let required_words: HashMap<_, _> = required_words
      .into_iter()
      .enumerate()
      .map(|(id, word)| (id as u32, word))
      .collect();

    let required_words_set: HashSet<_> = required_words
      .values()
      .map(|word| word.to_owned())
      .collect();
    let bank = bank
      .into_iter()
      .filter(|word| !required_words_set.contains(word));

    Ok(Self {
      xword: XWord::from_grid(board, bank)?,
      required_words,
    })
  }

  fn build_partitioned_word_assignments(
    &self,
  ) -> impl Iterator<Item = (XWordClueAssignment, Vec<Constraint<XWordConstraint>>)> + '_ {
    self.build_word_assignments_from_entries(self.iter_board_entries())
  }

  fn build_subproblem(&self) -> ProblemParameters {
    let mut params = ProblemParameters::default();

    params.constraints.extend(
      self
        .build_tile_constraints()
        .flat_map(|(_, _, constraints)| constraints),
    );

    for (partition_id, assignments) in self.build_partitioned_word_assignments(&mut uf) {
      let subproblem = subproblem_map.entry(partition_id).or_default();
      subproblem.word_assignments.extend(assignments);
    }

    subproblem_map
      .values_mut()
      .for_each(|ProblemParameters { constraints, .. }| {
        constraints.extend(self.build_clue_constraints());
      });

    subproblem_map
  }

  pub fn build_dlx_solvers(&self) -> HashMap<Pos, Dlx<XWordConstraint, XWordClueAssignment>> {
    self
      .build_partitioned_subproblems()
      .into_iter()
      .map(|(pos, params)| (pos, params.build_dlx()))
      .collect()
  }

  pub fn solve(&self) -> TermgameResult<Grid<XWordTile>> {
    self
      .build_dlx_solvers()
      .into_values()
      .try_fold(self.board().clone(), |board, mut dlx| {
        self.build_grid_from_assignments(
          board,
          dlx
            .find_solutions()
            .with_names()
            .next()
            .ok_or_else(|| TermgameError::Internal("No solution found".to_owned()))?,
        )
      })
  }

  pub fn stepwise_board_iter(&self) -> impl Iterator<Item = Grid<XWordTile>> + '_ {
    todo!();
    std::iter::empty()
  }
}

impl XWordInternal for XWordWithRequired {
  fn words(&self) -> impl Iterator<Item = (u32, &'_ str)> {
    self
      .required_words
      .iter()
      .map(|(&id, word)| (id, word.as_str()))
  }

  fn find_word(&self, word_id: u32) -> Option<&'_ str> {
    self.required_words.get(&word_id).map(|str| str.as_str())
  }

  fn num_words(&self) -> u32 {
    self.required_words.len() as u32
  }

  fn universal_words(&self) -> impl Iterator<Item = &'_ str> {
    self.xword.words().chain(self.words()).map(|(_, str)| str)
  }

  fn board(&self) -> &Grid<XWordTile> {
    self.xword.board()
  }

  fn should_fill_board() -> bool {
    false
  }

  fn word_constraint_type() -> HeaderType {
    // Require all words to be placed in the puzzle.
    HeaderType::Primary
  }

  #[cfg(test)]
  fn testonly_word_id(&self, word: &str) -> Option<u32> {
    self.xword.testonly_word_id(word).or_else(|| {
      self
        .words()
        .find_map(|(idx, bank_word)| (word == bank_word).then_some(idx + self.xword.num_words()))
    })
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
    LetterFrequencyMap, XWordClueAssignment, XWordClueNumber, XWordCluePosition, XWordConstraint,
    XWordInternal, XWordTile, XWordWithRequired, NUM_TILE_BITS,
  };

  use super::{XWord, XWordTileConstraint};

  #[gtest]
  fn test_empty() {
    let grid = XWord::build_grid("");
    expect_that!(grid, err(anything()));
  }

  #[gtest]
  fn test_available() -> TermgameResult {
    let xword = XWord::from_grid(
      XWord::build_grid(
        "__
         X_",
      )?,
      HashSet::new(),
    )?;

    expect_true!(xword.available(Pos { x: 0, y: 0 }));
    expect_true!(xword.available(Pos { x: 1, y: 0 }));
    expect_false!(xword.available(Pos { x: 0, y: 1 }));
    expect_true!(xword.available(Pos { x: 1, y: 1 }));

    Ok(())
  }

  #[gtest]
  fn test_iterate_rows() -> TermgameResult {
    let xword = XWord::from_grid(
      XWord::build_grid(
        "__
         X_",
      )?,
      HashSet::new(),
    )?;

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

    Ok(())
  }

  #[gtest]
  fn test_iterate_cols() -> TermgameResult {
    let xword = XWord::from_grid(
      XWord::build_grid(
        "__
         X_",
      )?,
      HashSet::new(),
    )?;

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

    Ok(())
  }

  #[gtest]
  fn test_iter_board_entries() -> TermgameResult {
    let xword = XWord::from_grid(
      XWord::build_grid(
        "____
         _X__",
      )?,
      HashSet::new(),
    )?;

    expect_that!(
      xword.iter_board_entries().collect::<Vec<_>>(),
      unordered_elements_are![
        &(
          XWordCluePosition {
            pos: Pos { x: 0, y: 0 },
            clue_number: XWordClueNumber { number: 0, is_row: true }
          },
          4
        ),
        &(
          XWordCluePosition {
            pos: Pos { x: 0, y: 1 },
            clue_number: XWordClueNumber { number: 1, is_row: true }
          },
          1
        ),
        &(
          XWordCluePosition {
            pos: Pos { x: 2, y: 1 },
            clue_number: XWordClueNumber { number: 2, is_row: true }
          },
          2
        ),
        &(
          XWordCluePosition {
            pos: Pos { x: 0, y: 0 },
            clue_number: XWordClueNumber { number: 0, is_row: false }
          },
          2
        ),
        &(
          XWordCluePosition {
            pos: Pos { x: 1, y: 0 },
            clue_number: XWordClueNumber { number: 1, is_row: false }
          },
          1
        ),
        &(
          XWordCluePosition {
            pos: Pos { x: 2, y: 0 },
            clue_number: XWordClueNumber { number: 2, is_row: false }
          },
          2
        ),
        &(
          XWordCluePosition {
            pos: Pos { x: 3, y: 0 },
            clue_number: XWordClueNumber { number: 3, is_row: false }
          },
          2
        ),
      ]
    );

    Ok(())
  }

  #[gtest]
  fn test_letter_frequency_map_likelihood() {
    let map = LetterFrequencyMap::from_words(["a", "b", "c", "ab", "ac", "cc"]);

    expect_float_eq!(map.likelihood(1, ('a', 0)), 1. / 3.);
    expect_float_eq!(map.likelihood(1, ('b', 0)), 1. / 3.);
    expect_float_eq!(map.likelihood(1, ('c', 0)), 1. / 3.);
    expect_float_eq!(map.likelihood(1, ('d', 0)), 0.);

    expect_float_eq!(map.likelihood(2, ('a', 0)), 2. / 3.);
    expect_float_eq!(map.likelihood(2, ('b', 0)), 0.);
    expect_float_eq!(map.likelihood(2, ('c', 0)), 1. / 3.);
    expect_float_eq!(map.likelihood(2, ('d', 0)), 0.);

    expect_float_eq!(map.likelihood(2, ('a', 1)), 0.);
    expect_float_eq!(map.likelihood(2, ('b', 1)), 1. / 3.);
    expect_float_eq!(map.likelihood(2, ('c', 1)), 2. / 3.);
    expect_float_eq!(map.likelihood(2, ('d', 1)), 0.);

    expect_float_eq!(map.likelihood(3, ('a', 0)), 0.);
    expect_float_eq!(map.likelihood(3, ('b', 0)), 0.);
    expect_float_eq!(map.likelihood(3, ('c', 0)), 0.);
  }

  #[gtest]
  fn test_letter_frequency_map_words_with_length() {
    let map = LetterFrequencyMap::from_words(["a", "b", "c", "ab", "ac", "cc"]);

    expect_that!(map.words_with_length(0).collect::<Vec<_>>(), empty());
    expect_that!(
      map.words_with_length(1).collect::<Vec<_>>(),
      unordered_elements_are![&"a", &"b", &"c"]
    );
    expect_that!(
      map.words_with_length(2).collect::<Vec<_>>(),
      unordered_elements_are![&"ab", &"ac", &"cc"]
    );
    expect_that!(map.words_with_length(3).collect::<Vec<_>>(), empty());
  }

  #[gtest]
  fn test_letter_likelihood_score() -> TermgameResult {
    let xword = XWord::from_grid(
      XWord::build_grid(
        "____
         _X__
         _XX_
         XXX_",
      )?,
      [
        "a", "b", //
        "cd", "ce", "ee", "gh", //
        "ijk", "ikl", "jkl", //
        "zyxw", "zzzz", "yzxy", "xxxx", "wwww", //
      ]
      .into_iter()
      .map(|str| str.to_owned()),
    )?;

    let frequency_map = LetterFrequencyMap::from_words(xword.bank.values().map(|str| str.as_str()));

    // First-position letters in columns across the top row:
    expect_float_eq!(
      xword.letter_likelihood_score('a', Pos { x: 1, y: 0 }, false, &frequency_map),
      1. / 2.
    );
    expect_float_eq!(
      xword.letter_likelihood_score('b', Pos { x: 1, y: 0 }, false, &frequency_map),
      1. / 2.
    );
    expect_float_eq!(
      xword.letter_likelihood_score('c', Pos { x: 1, y: 0 }, false, &frequency_map),
      0.
    );

    expect_float_eq!(
      xword.letter_likelihood_score('c', Pos { x: 2, y: 0 }, false, &frequency_map),
      2. / 4.
    );
    expect_float_eq!(
      xword.letter_likelihood_score('e', Pos { x: 2, y: 0 }, false, &frequency_map),
      1. / 4.
    );
    expect_float_eq!(
      xword.letter_likelihood_score('h', Pos { x: 2, y: 0 }, false, &frequency_map),
      0.
    );

    expect_float_eq!(
      xword.letter_likelihood_score('i', Pos { x: 0, y: 0 }, false, &frequency_map),
      2. / 3.
    );
    expect_float_eq!(
      xword.letter_likelihood_score('j', Pos { x: 0, y: 0 }, false, &frequency_map),
      1. / 3.
    );
    expect_float_eq!(
      xword.letter_likelihood_score('k', Pos { x: 0, y: 0 }, false, &frequency_map),
      0.
    );

    expect_float_eq!(
      xword.letter_likelihood_score('z', Pos { x: 3, y: 0 }, false, &frequency_map),
      2. / 5.
    );
    expect_float_eq!(
      xword.letter_likelihood_score('y', Pos { x: 3, y: 0 }, false, &frequency_map),
      1. / 5.
    );
    expect_float_eq!(
      xword.letter_likelihood_score('a', Pos { x: 3, y: 0 }, false, &frequency_map),
      0.
    );

    // First-position letters in rows:
    expect_float_eq!(
      xword.letter_likelihood_score('c', Pos { x: 2, y: 1 }, true, &frequency_map),
      1. / 2.
    );
    expect_float_eq!(
      xword.letter_likelihood_score('z', Pos { x: 0, y: 0 }, true, &frequency_map),
      2. / 5.
    );
    expect_float_eq!(
      xword.letter_likelihood_score('a', Pos { x: 2, y: 1 }, true, &frequency_map),
      0.
    );

    // Middle-position letters in columns:
    expect_float_eq!(
      xword.letter_likelihood_score('e', Pos { x: 2, y: 1 }, false, &frequency_map),
      2. / 4.
    );
    expect_float_eq!(
      xword.letter_likelihood_score('h', Pos { x: 2, y: 1 }, false, &frequency_map),
      1. / 4.
    );
    expect_float_eq!(
      xword.letter_likelihood_score('x', Pos { x: 3, y: 2 }, false, &frequency_map),
      3. / 5.
    );
    expect_float_eq!(
      xword.letter_likelihood_score('a', Pos { x: 2, y: 1 }, false, &frequency_map),
      0.
    );

    // Middle-position letters in rows:
    expect_float_eq!(
      xword.letter_likelihood_score('e', Pos { x: 3, y: 1 }, true, &frequency_map),
      2. / 4.
    );
    expect_float_eq!(
      xword.letter_likelihood_score('d', Pos { x: 3, y: 1 }, true, &frequency_map),
      1. / 4.
    );
    expect_float_eq!(
      xword.letter_likelihood_score('w', Pos { x: 3, y: 0 }, true, &frequency_map),
      2. / 5.
    );
    expect_float_eq!(
      xword.letter_likelihood_score('a', Pos { x: 3, y: 0 }, true, &frequency_map),
      0.
    );

    Ok(())
  }

  #[gtest]
  fn test_word_likelihood_score() -> TermgameResult {
    let xword = XWord::from_grid(
      XWord::build_grid(
        "____
         _X__
         _XX_
         X___",
      )?,
      [
        "a", "b", //
        "cd", "ce", "ee", "gh", //
        "ijk", "ikl", "jkl", //
        "zyxw", "zzzz", "yzxy", "xxxx", "wwww", //
      ]
      .into_iter()
      .map(|str| str.to_owned()),
    )?;

    let frequency_map = LetterFrequencyMap::from_words(xword.bank.values().map(|str| str.as_str()));

    expect_float_eq!(
      xword.word_likelihood_score(
        "yab",
        XWordCluePosition {
          pos: Pos { x: 0, y: 0 },
          clue_number: XWordClueNumber { number: 0, is_row: false },
        },
        &frequency_map,
      ),
      (1. / 5.) * (1. / 2.) * (1. / 2.)
    );

    expect_float_eq!(
      xword.word_likelihood_score(
        "weal",
        XWordCluePosition {
          pos: Pos { x: 3, y: 0 },
          clue_number: XWordClueNumber { number: 3, is_row: false },
        },
        &frequency_map,
      ),
      (2. / 5.) * (2. / 4.) * (1. / 2.) * (2. / 3.)
    );

    expect_float_eq!(
      xword.word_likelihood_score(
        "ey",
        XWordCluePosition {
          pos: Pos { x: 2, y: 1 },
          clue_number: XWordClueNumber { number: 2, is_row: true },
        },
        &frequency_map,
      ),
      (2. / 4.) * (1. / 5.)
    );

    Ok(())
  }

  #[gtest]
  fn test_letter_tile_constraints() -> googletest::Result<()> {
    let pos = Pos { x: 2, y: 5 };
    for is_row in [false, true] {
      for letter in 'a'..='z' {
        let letter_constraints: Vec<_> =
          XWord::letter_tile_constraints(pos, letter, is_row).collect();
        assert_eq!(letter_constraints.len(), 5);

        let contains_any_matcher = || {
          contains(any![
            eq(&letter_constraints[0]),
            eq(&letter_constraints[1]),
            eq(&letter_constraints[2]),
            eq(&letter_constraints[3]),
            eq(&letter_constraints[4]),
          ])
        };
        let contains_none_matcher = || {
          each(all![
            not(eq(&letter_constraints[0])),
            not(eq(&letter_constraints[1])),
            not(eq(&letter_constraints[2])),
            not(eq(&letter_constraints[3])),
            not(eq(&letter_constraints[4])),
          ])
        };

        // Verify that no letters in the opposing direction are compatible
        // unless they are the same.
        for other_letter in 'a'..='z' {
          let other_constraints: Vec<_> =
            XWord::letter_tile_constraints(pos, other_letter, !is_row).collect();

          if letter == other_letter {
            verify_that!(other_constraints, contains_none_matcher())?;
          } else {
            verify_that!(other_constraints, contains_any_matcher())?;
          }
        }
      }
    }
    Ok(())
  }

  #[gtest]
  fn test_word_assignments() -> TermgameResult {
    let xword = XWord::from_grid(
      XWord::build_grid(
        "__
         X_",
      )?,
      ["ab", "c"].into_iter().map(|str| str.to_owned()),
    )?;

    let ab_id = xword.testonly_word_id("ab").expect("word ab not found");
    let c_id = xword.testonly_word_id("c").expect("word c not found");

    let uf = xword.build_partition_uf();
    let word_assignments: Vec<_> = xword
      .build_partitioned_word_assignments(&uf)
      .next()
      .expect("Unexpected empty iterator over partitioned word assignments.")
      .1
      .collect();
    expect_that!(
      word_assignments,
      unordered_elements_are![
        (
          pat!(XWordClueAssignment {
            id: &ab_id,
            clue_pos: pat!(XWordCluePosition {
              pos: &Pos::zero(),
              clue_number: pat!(XWordClueNumber { number: &0, is_row: &true })
            })
          }),
          contains_each![
            pat!(Constraint::Primary(pat!(XWordConstraint::ClueNumber(
              pat!(XWordClueNumber { number: &0, is_row: &true })
            )))),
            pat!(Constraint::Secondary(&ColorItem::new(
              XWordConstraint::Clue { id: ab_id },
              0
            ))),
          ]
        ),
        (
          pat!(XWordClueAssignment {
            id: &ab_id,
            clue_pos: pat!(XWordCluePosition {
              pos: pat!(Pos { x: &1, y: &0 }),
              clue_number: pat!(XWordClueNumber { number: &1, is_row: &false })
            })
          }),
          contains_each![
            pat!(Constraint::Primary(pat!(XWordConstraint::ClueNumber(
              pat!(XWordClueNumber { number: &1, is_row: &false })
            )))),
            pat!(Constraint::Secondary(&ColorItem::new(
              XWordConstraint::Clue { id: ab_id },
              1
            ))),
          ]
        ),
        (
          pat!(XWordClueAssignment {
            id: &c_id,
            clue_pos: pat!(XWordCluePosition {
              pos: pat!(Pos { x: &1, y: &1 }),
              clue_number: pat!(XWordClueNumber { number: &1, is_row: &true })
            })
          }),
          contains_each![
            pat!(Constraint::Primary(pat!(XWordConstraint::ClueNumber(
              pat!(XWordClueNumber { number: &1, is_row: &true })
            )))),
            pat!(Constraint::Secondary(&ColorItem::new(
              XWordConstraint::Clue { id: c_id },
              0
            ))),
          ]
        ),
        (
          pat!(XWordClueAssignment {
            id: &c_id,
            clue_pos: pat!(XWordCluePosition {
              pos: pat!(Pos { x: &0, y: &0 }),
              clue_number: pat!(XWordClueNumber { number: &0, is_row: &false })
            })
          }),
          contains_each![
            pat!(Constraint::Primary(pat!(XWordConstraint::ClueNumber(
              pat!(XWordClueNumber { number: &0, is_row: &false })
            )))),
            pat!(Constraint::Secondary(&ColorItem::new(
              XWordConstraint::Clue { id: c_id },
              1
            ))),
          ]
        ),
      ]
    );

    Ok(())
  }

  #[gtest]
  fn test_word_assignments_order() -> TermgameResult {
    let xword = XWord::from_grid(
      XWord::build_grid(
        "__
         X_",
      )?,
      ["ab", "ca", "c"].into_iter().map(|str| str.to_owned()),
    )?;

    let ab_id = xword.testonly_word_id("ab").expect("word ab not found");
    let ca_id = xword.testonly_word_id("ca").expect("word ca not found");

    let uf = xword.build_partition_uf();
    let word_assignments: Vec<_> = xword
      .build_partitioned_word_assignments(&uf)
      .next()
      .expect("Unexpected empty iterator over partitioned word assignments.")
      .1
      .collect();

    let first_row_assignments: Vec<_> = word_assignments
      .iter()
      .filter_map(|(XWordClueAssignment { clue_pos, .. }, constraints)| {
        (clue_pos.pos == Pos { x: 0, y: 0 } && clue_pos.clue_number.is_row).then_some(constraints)
      })
      .cloned()
      .collect();

    type XWordColorItem = ColorItem<XWordConstraint>;

    // "ca" should appear before "ab" in the first row, since "ca" is possible
    // but "ab" is not.
    expect_that!(
      first_row_assignments,
      elements_are![
        contains_each![
          pat!(Constraint::Primary(pat!(XWordConstraint::ClueNumber(
            pat!(XWordClueNumber { number: &0, is_row: &true })
          )))),
          pat!(Constraint::Secondary(property!(
            &XWordColorItem.item(),
            &XWordConstraint::Clue { id: ca_id }
          ))),
        ],
        contains_each![
          pat!(Constraint::Primary(pat!(XWordConstraint::ClueNumber(
            pat!(XWordClueNumber { number: &0, is_row: &true })
          )))),
          pat!(Constraint::Secondary(property!(
            &XWordColorItem.item(),
            &XWordConstraint::Clue { id: ab_id }
          ))),
        ],
      ]
    );

    Ok(())
  }

  #[gtest]
  fn test_build_partition_uf() -> TermgameResult {
    let xword = XWord::from_grid(
      XWord::build_grid(
        "___X___
         ___XXX_
         XaXXbcd
         ___X_XX
         ___X___",
      )?,
      [],
    )?;

    let mut uf = xword.build_partition_uf();
    assert_eq!(uf.root_level_keys().len(), 3);

    let group1 = uf.find(Pos::zero());
    let group2 = uf.find(Pos { x: 4, y: 0 });
    let group3 = uf.find(Pos { x: 4, y: 3 });

    let mut tiles_in_group = |group: Pos| -> Vec<_> {
      xword
        .board
        .positions()
        .filter(|&pos| xword.empty(pos) && uf.find(pos) == group)
        .collect()
    };

    let group1_tiles = tiles_in_group(group1);
    expect_eq!(group1_tiles.len(), 12);
    expect_that!(group1_tiles, each(field!(Pos.x, lt(&3))));

    let group2_tiles = tiles_in_group(group2);
    expect_eq!(group2_tiles.len(), 4);
    expect_that!(
      group2_tiles,
      each(all![field!(Pos.x, gt(&3)), field!(Pos.y, lt(&2))])
    );

    let group3_tiles = tiles_in_group(group3);
    expect_eq!(group3_tiles.len(), 4);
    expect_that!(
      group3_tiles,
      each(all![field!(Pos.x, gt(&3)), field!(Pos.y, gt(&2))])
    );

    Ok(())
  }

  #[gtest]
  fn test_build_partitioned_subproblems() -> TermgameResult {
    let xword = XWord::from_grid(
      XWord::build_grid(
        "__
         X_",
      )?,
      ["ab", "bc"].into_iter().map(|str| str.to_owned()),
    )?;

    let subproblems = xword.build_partitioned_subproblems();
    assert_eq!(subproblems.len(), 1);
    let params = subproblems.into_values().next().unwrap();
    let constraints: HashSet<_> = params.constraints.into_iter().collect();

    let expected_constraints: HashSet<_> = [false, true]
      .into_iter()
      .flat_map(|is_row| {
        (0..=1).map(move |number| {
          (
            XWordConstraint::ClueNumber(XWordClueNumber { number, is_row }),
            HeaderType::Primary,
          )
        })
      })
      .chain(
        [Pos { x: 0, y: 0 }, Pos { x: 1, y: 0 }, Pos { x: 1, y: 1 }]
          .into_iter()
          .flat_map(|pos| {
            (0..NUM_TILE_BITS).map(move |bit| {
              (
                XWordConstraint::Tile(XWordTileConstraint { pos, bit }),
                HeaderType::Primary,
              )
            })
          }),
      )
      .chain((0..=1).map(|id| (XWordConstraint::Clue { id }, HeaderType::Secondary)))
      .collect();

    expect_that!(constraints, container_eq(expected_constraints));

    Ok(())
  }

  #[gtest]
  fn test_small_dict() -> TermgameResult {
    let xword = XWord::from_grid(
      XWord::build_grid(
        "__
         X_",
      )?,
      ["a", "c", "ab", "bc"].into_iter().map(|str| str.to_owned()),
    )?;

    let solution = xword.solve()?;
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

    Ok(())
  }

  #[gtest]
  fn test_small_dict_required() -> TermgameResult {
    let xword = XWordWithRequired::from_grid(
      XWord::build_grid(
        "__
         X_",
      )?,
      ["ab"].into_iter().map(|str| str.to_owned()),
      ["a", "c", "e", "cd", "de", "ab", "bc"]
        .into_iter()
        .map(|str| str.to_owned()),
    )?;

    let solution = xword.solve()?;
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

    Ok(())
  }

  #[gtest]
  fn test_mini() -> TermgameResult {
    let xword = XWord::from_grid(
      XWord::build_grid(
        "X___X
         _____
         _____
         _____
         _____",
      )?,
      [
        "hug", "korea", "isbns", "snark", "sines", "kiss", "hosni", "urban", "genre", "asks",
      ]
      .into_iter()
      .map(|str| str.to_owned()),
    )?;

    let solution = xword.solve()?;

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

    Ok(())
  }

  #[gtest]
  fn test_mini_with_partial() -> TermgameResult {
    let xword = XWord::from_grid(
      XWord::build_grid(
        "Xa
         _b",
      )?,
      ["a", "xa", "x", "c", "aa", "cb"]
        .into_iter()
        .map(|str| str.to_owned()),
    )?;

    let solution = xword.solve()?;

    use XWordTile::*;

    #[rustfmt::skip]
    let expected_solution = Grid::from_vec(
      vec![
        Wall,        Letter('a'),
        Letter('c'), Letter('b'),
      ], 2, 2,
    ).unwrap();
    expect_eq!(solution, expected_solution);

    Ok(())
  }

  #[gtest]
  fn test_two_partitions() -> TermgameResult {
    let xword = XWord::from_grid(
      XWord::build_grid(
        "_Xc__
         __aX_
         __tX_",
      )?,
      ["hat", "aba", "ba", "hah", "h", "cog", "o", "guy", "u", "y"]
        .into_iter()
        .map(|str| str.to_owned()),
    )?;

    let solution = xword.solve()?;

    use XWordTile::*;

    #[rustfmt::skip]
    let expected_solution = Grid::from_vec(
      vec![
        Letter('h'), Wall,        Letter('c'), Letter('o'), Letter('g'),
        Letter('a'), Letter('b'), Letter('a'), Wall,        Letter('u'),
        Letter('h'), Letter('a'), Letter('t'), Wall,        Letter('y'),
      ], 5, 3,
    ).unwrap();
    expect_eq!(solution, expected_solution);

    Ok(())
  }

  #[gtest]
  fn test_two_partitions_with_required() -> TermgameResult {
    let xword = XWordWithRequired::from_grid(
      XWord::build_grid(
        "_Xc_
         __aX
         __tX",
      )?,
      ["ttt"].into_iter().map(|str| str.to_owned()),
      ["hat", "aba", "tat", "ba", "bt", "h", "co", "o"]
        .into_iter()
        .map(|str| str.to_owned()),
    )?;

    let solution = xword.solve()?;

    use XWordTile::*;

    #[rustfmt::skip]
    let expected_solution = Grid::from_vec(
      vec![
        Letter('h'), Wall,        Letter('c'), Letter('o'),
        Letter('a'), Letter('b'), Letter('a'), Wall,
        Letter('t'), Letter('t'), Letter('t'), Wall,
      ], 4, 3,
    )?;
    expect_eq!(solution, expected_solution);

    Ok(())
  }
}
