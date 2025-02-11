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
};

use dlx::{ColorItem, Constraint, Dlx, DlxIteratorWithNames, HeaderType, StepwiseDlxIterResult};

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

#[derive(Clone, Debug)]
pub struct XWord {
  board: Grid<XWordTile>,
  required_words: HashMap<u32, String>,
  bank: HashMap<u32, String>,
}

impl XWord {
  pub fn from_grid_with_required(
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
      .filter(|word| !required_words_set.contains(word))
      .enumerate()
      .map(|(id, word)| ((id + required_words.len()) as u32, word))
      .collect();

    Ok(Self { board, required_words, bank })
  }

  pub fn from_grid(
    board: Grid<XWordTile>,
    bank: impl IntoIterator<Item = String>,
  ) -> TermgameResult<Self> {
    Self::from_grid_with_required(board, iter::empty(), bank)
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

  pub fn board(&self) -> &Grid<XWordTile> {
    &self.board
  }

  #[cfg(test)]
  fn testonly_word_id(&self, word: &str) -> Option<u32> {
    self
      .required_words
      .iter()
      .chain(self.bank.iter())
      .find_map(|(&idx, bank_word)| (word == bank_word).then_some(idx))
  }

  pub fn empty(&self, pos: Pos) -> bool {
    self
      .board
      .get(pos)
      .is_some_and(|tile| matches!(tile, XWordTile::Empty))
  }

  pub fn available(&self, pos: Pos) -> bool {
    self.board.get(pos).is_some_and(|tile| tile.available())
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

  fn iterate_row_clues(&self) -> impl Iterator<Item = XWordEntry> + use<'_> {
    Self::iterate_board_row_clues(&self.board)
  }

  fn iterate_col_clues(&self) -> impl Iterator<Item = XWordEntry> + use<'_> {
    Self::iterate_board_row_clues(self.board.transpose())
      .map(|entry| XWordEntry { pos: entry.pos.transpose(), ..entry })
  }

  fn clue_letter_positions_unbounded<'a>(
    &self,
    clue_pos: &'a XWordCluePosition,
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
    clue_pos: &'a XWordCluePosition,
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

  fn tile_constraints_for_pos(&self, pos: Pos) -> impl Iterator<Item = XWordTileConstraint> + '_ {
    debug_assert!(self.board.get(pos).is_some_and(|tile| tile.empty()));
    (0..NUM_TILE_BITS).map(move |bit| XWordTileConstraint { pos, bit })
  }

  fn build_clue_constraints(&self) -> impl Iterator<Item = (XWordConstraint, HeaderType)> + '_ {
    self
      .required_words
      .iter()
      .map(|(&id, _)| (XWordConstraint::Clue { id }, HeaderType::Primary))
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
          .board
          .get(pos - delta * diff)
          .is_some_and(|tile| tile.available())
      })
      .count() as u32;
    let word_length = letter_idx
      + 1
      + (1..)
        .take_while(|&delta| {
          self
            .board
            .get(pos + delta * diff)
            .is_some_and(|tile| tile.available())
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
      .flat_map(move |(clue_pos, length)| -> Vec<_> {
        let clue_pos_constraint =
          Constraint::Primary(XWordConstraint::ClueNumber(clue_pos.clue_number.clone()));

        frequency_map
          .words_with_length(length)
          .filter(|word| {
            self
              .word_letter_positions(&clue_pos, word)
              .all(|(c, pos)| match self.board.get(pos) {
                Some(XWordTile::Empty) => true,
                Some(&XWordTile::Letter(letter)) => letter == c,
                _ => unreachable!(),
              })
          })
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
            constraints.extend(
              self
                .word_letter_positions(&clue_pos, word)
                .flat_map(|(c, pos)| {
                  Self::letter_tile_constraints(pos, c, clue_pos.clue_number.is_row)
                    .map(XWordTileConstraint::into_item_constraint)
                }),
            );

            (
              (
                XWordClueAssignment { id, clue_pos: clue_pos.clone() },
                constraints,
              ),
              self.word_likelihood_score(word, &clue_pos, &frequency_map),
            )
          })
          .collect()
      })
      .sorted_unstable_by(|(_, score1), (_, score2)| {
        score2.partial_cmp(score1).unwrap_or(Ordering::Equal)
      })
      .map(|(constraints, _)| constraints)
  }

  fn build_partition_uf(&self) -> UnionFind<Pos> {
    let mut uf = UnionFind::from_keys(self.board.positions().filter(|&pos| self.empty(pos)));
    for (XWordEntry { number, pos, length }, is_row) in self
      .iterate_row_clues()
      .map(|entry| (entry, true))
      .chain(self.iterate_col_clues().map(|entry| (entry, false)))
    {
      for (pos1, pos2) in self
        .clue_letter_positions(
          &XWordCluePosition {
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
          &XWordCluePosition {
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

  pub fn build_grid_from_assignments<I>(
    &self,
    mut answer_grid: Grid<XWordTile>,
    iter: I,
  ) -> TermgameResult<Grid<XWordTile>>
  where
    I: IntoIterator<Item = XWordClueAssignment>,
  {
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

  fn find_empty_word_tile(
    &self,
    pos: Pos,
    clue_number: XWordClueNumber,
    length: u32,
    uf: &mut UnionFind<Pos>,
  ) -> Option<Pos> {
    self
      .clue_letter_positions(&XWordCluePosition { pos, clue_number }, length)
      .find_map(|pos| self.empty(pos).then(|| uf.find(pos)))
  }

  fn find_empty_word_tile_unchecked(
    &self,
    pos: Pos,
    clue_number: XWordClueNumber,
    uf: &mut UnionFind<Pos>,
  ) -> Option<Pos> {
    self
      .clue_letter_positions_unbounded(&XWordCluePosition { pos, clue_number })
      .take_while(|&pos| self.available(pos))
      .find_map(|pos| self.empty(pos).then(|| uf.find(pos)))
  }

  fn assign_required_clue_partitions(&self) -> HashMap<Pos, Vec<&str>> {
    todo!();
  }

  fn build_partitioned_subproblems(&self) -> HashMap<Pos, ProblemParameters> {
    let mut uf = self.build_partition_uf();

    // For each subproblem, forbid the use of certain constraints which are not
    // possible to obtain.
    let mut subproblem_map = HashMap::<Pos, ProblemParameters>::new();
    for (clue_number, pos, length) in self
      .iterate_row_clues()
      .map(|entry| (entry, true))
      .chain(self.iterate_col_clues().map(|entry| (entry, false)))
      .map(|(XWordEntry { number, pos, length }, is_row)| {
        (XWordClueNumber { number, is_row }, pos, length)
      })
    {
      if let Some(uf_id) = self.find_empty_word_tile(pos, clue_number.clone(), length, &mut uf) {
        for pos in self.clue_letter_positions(
          &XWordCluePosition { pos, clue_number: clue_number.clone() },
          length,
        ) {
          let constraints = &mut subproblem_map.entry(uf_id).or_default().constraints;
          match self.board.get(pos) {
            Some(&XWordTile::Letter(letter)) => {
              constraints.extend(
                Self::letter_tile_constraints(pos, letter, clue_number.is_row)
                  .map(XWordTileConstraint::into_constraint),
              );
            }
            Some(&XWordTile::Empty) => {
              // Empty tiles are always intersected by a row and a col clue, we
              // arbitrarily choose the row clue to insert into constraints (if
              // we allowed both to, there would be duplicates).
              if clue_number.is_row {
                constraints.extend(
                  self
                    .tile_constraints_for_pos(pos)
                    .map(XWordTileConstraint::into_constraint),
                );
              }
            }
            _ => unreachable!(),
          }
        }

        subproblem_map.entry(uf_id).or_default().constraints.push((
          XWordConstraint::ClueNumber(clue_number),
          HeaderType::Primary,
        ));
      }
    }

    let required_clues = self.assign_required_clue_partitions();

    for ref assignment @ (
      XWordClueAssignment {
        clue_pos: XWordCluePosition { pos, ref clue_number },
        ..
      },
      _,
    ) in self.build_word_assignments()
    {
      if let Some(pos) = self.find_empty_word_tile_unchecked(pos, clue_number.clone(), &mut uf) {
        subproblem_map
          .entry(uf.find(pos))
          .or_default()
          .word_assignments
          .push(assignment.clone());
      }
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
      .try_fold(self.board.clone(), |board, mut dlx| {
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
          .build_grid_from_assignments(self.board.clone(), selected_items)
          .unwrap(),
      )
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
    XWordTile, NUM_TILE_BITS,
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
        &XWordCluePosition {
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
        &XWordCluePosition {
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
        &XWordCluePosition {
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

    let word_assignments: Vec<_> = xword.build_word_assignments().collect();
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

    let word_assignments: Vec<_> = xword.build_word_assignments().collect();

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
    let xword = XWord::from_grid_with_required(
      XWord::build_grid(
        "__
         X_",
      )?,
      ["ab"].into_iter().map(|str| str.to_owned()),
      ["a", "c", "e", "cd", "de", "ab", "bc"]
        .into_iter()
        .map(|str| str.to_owned()),
    )?;

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

    Ok(())
  }

  #[gtest]
  fn test_mini_with_partial() -> TermgameResult {
    let xword = XWord::from_grid(
      XWord::build_grid(
        "Xa
         _b",
      )?,
      ["a", "c", "aa", "cb", "ab"]
        .into_iter()
        .map(|str| str.to_owned()),
    )?;

    let solution = xword.solve();
    assert_that!(solution, ok(anything()));
    let solution = solution.unwrap();

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
      [
        "cat", "hat", "aba", "ba", "hah", "h", "cog", "o", "guy", "u", "y",
      ]
      .into_iter()
      .map(|str| str.to_owned()),
    )?;

    let solution = xword.solve();
    assert_that!(solution, ok(anything()));
    let solution = solution.unwrap();

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
}
