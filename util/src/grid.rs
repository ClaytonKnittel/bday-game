use std::fmt::{Debug, Display};

use bitcode::{Decode, Encode};

use crate::{
  error::{TermgameError, TermgameResult},
  pos::Pos,
};

pub trait Gridlike<T> {
  fn width(&self) -> u32;
  fn height(&self) -> u32;
  fn in_bounds(&self, pos: Pos) -> bool;

  fn get(&self, pos: Pos) -> Option<&T>;

  fn iter_row<'a, 'b>(&'a self, y: u32) -> impl Iterator<Item = &'b T>
  where
    'a: 'b,
    T: 'a;
  fn iter_col<'a, 'b>(&'a self, x: u32) -> impl Iterator<Item = &'b T>
  where
    'a: 'b,
    T: 'a;

  fn transpose(&self) -> impl Gridlike<T>;
}

pub trait MutGridlike<T>: Gridlike<T> {
  fn get_mut(&mut self, pos: Pos) -> Option<&mut T>;
  fn transpose_mut(&mut self) -> impl MutGridlike<T>;
}

#[derive(Clone, PartialEq, Eq, Encode, Decode)]
pub struct Grid<T> {
  grid: Vec<T>,
  width: u32,
  height: u32,
}

impl<T> Grid<T> {
  pub fn from_vec(grid: Vec<T>, width: u32, height: u32) -> TermgameResult<Self> {
    let expected_size = width as usize * height as usize;
    if grid.len() != expected_size {
      return Err(
        TermgameError::Internal(format!(
          "Expected grid.len() == expected_size, {} != {expected_size}",
          grid.len()
        ))
        .into(),
      );
    }

    Ok(Self { grid, width, height })
  }

  fn idx(&self, pos: Pos) -> usize {
    debug_assert!(self.in_bounds(pos));
    let x = pos.x as usize;
    let y = pos.y as usize;
    x + y * self.width as usize
  }

  pub fn positions(&self) -> impl Iterator<Item = Pos> {
    let width = self.width;
    (0..self.height as i32).flat_map(move |y| (0..width as i32).map(move |x| Pos { x, y }))
  }

  pub fn map<F, U>(&self, f: F) -> Grid<U>
  where
    F: FnMut(&T) -> U,
  {
    Grid {
      grid: self.grid.iter().map(f).collect(),
      width: self.width,
      height: self.height,
    }
  }
}

impl<T> Grid<T>
where
  T: Default,
{
  pub fn new(width: u32, height: u32) -> Self {
    Self {
      grid: (0..width * height).map(|_| T::default()).collect(),
      width,
      height,
    }
  }
}

impl<T> Gridlike<T> for Grid<T> {
  fn width(&self) -> u32 {
    self.width
  }

  fn height(&self) -> u32 {
    self.height
  }

  fn in_bounds(&self, pos: Pos) -> bool {
    pos.x >= 0 && pos.x < self.width() as i32 && pos.y >= 0 && pos.y < self.height() as i32
  }

  fn get(&self, pos: Pos) -> Option<&T> {
    self
      .in_bounds(pos)
      .then(|| self.grid.get(self.idx(pos)))
      .flatten()
  }

  fn iter_row<'a, 'b>(&'a self, y: u32) -> impl Iterator<Item = &'b T>
  where
    'a: 'b,
    T: 'a,
  {
    let y = y as i32;
    (0..self.width()).flat_map(move |x| self.get(Pos { x: x as i32, y }))
  }

  fn iter_col<'a, 'b>(&'a self, x: u32) -> impl Iterator<Item = &'b T>
  where
    'a: 'b,
    T: 'a,
  {
    let x = x as i32;
    (0..self.height()).flat_map(move |y| self.get(Pos { x, y: y as i32 }))
  }

  fn transpose(&self) -> impl Gridlike<T> {
    TransposeGrid { grid: self }
  }
}

impl<T> MutGridlike<T> for Grid<T> {
  fn get_mut(&mut self, pos: Pos) -> Option<&mut T> {
    self
      .in_bounds(pos)
      .then(|| {
        let index = self.idx(pos);
        self.grid.get_mut(index)
      })
      .flatten()
  }

  fn transpose_mut(&mut self) -> impl MutGridlike<T> {
    MutTransposeGrid { grid: self }
  }
}

impl<T: Debug> Debug for Grid<T> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    (0..self.height).try_fold((), |_, y| {
      self.iter_row(y).try_fold((), |_, t| write!(f, "{t:?} "))?;
      writeln!(f)
    })
  }
}

impl<T: Display> Display for Grid<T> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    (0..self.height).try_fold((), |_, y| {
      self.iter_row(y).try_fold((), |_, t| write!(f, "{t} "))?;
      writeln!(f)
    })
  }
}

pub struct TransposeGrid<'a, T> {
  grid: &'a Grid<T>,
}

impl<T> Gridlike<T> for TransposeGrid<'_, T> {
  fn width(&self) -> u32 {
    self.grid.height()
  }

  fn height(&self) -> u32 {
    self.grid.width()
  }

  fn in_bounds(&self, pos: Pos) -> bool {
    self.grid.in_bounds(pos.transpose())
  }

  fn get(&self, pos: Pos) -> Option<&T> {
    self.grid.get(pos.transpose())
  }

  fn iter_row<'b, 'c>(&'b self, y: u32) -> impl Iterator<Item = &'c T>
  where
    'b: 'c,
    T: 'b,
  {
    self.grid.iter_col(y)
  }

  fn iter_col<'b, 'c>(&'b self, x: u32) -> impl Iterator<Item = &'c T>
  where
    'b: 'c,
    T: 'b,
  {
    self.grid.iter_row(x)
  }

  fn transpose(&self) -> impl Gridlike<T> {
    &self.grid
  }
}

pub struct MutTransposeGrid<'a, T> {
  grid: &'a mut Grid<T>,
}

impl<T> Gridlike<T> for MutTransposeGrid<'_, T> {
  fn width(&self) -> u32 {
    self.grid.height()
  }

  fn height(&self) -> u32 {
    self.grid.width()
  }

  fn in_bounds(&self, pos: Pos) -> bool {
    self.grid.in_bounds(pos.transpose())
  }

  fn get(&self, pos: Pos) -> Option<&T> {
    self.grid.get(pos.transpose())
  }

  fn iter_row<'b, 'c>(&'b self, y: u32) -> impl Iterator<Item = &'c T>
  where
    'b: 'c,
    T: 'b,
  {
    self.grid.iter_col(y)
  }

  fn iter_col<'b, 'c>(&'b self, x: u32) -> impl Iterator<Item = &'c T>
  where
    'b: 'c,
    T: 'b,
  {
    self.grid.iter_row(x)
  }

  fn transpose(&self) -> impl Gridlike<T> {
    &self.grid
  }
}

impl<T> MutGridlike<T> for MutTransposeGrid<'_, T> {
  fn get_mut(&mut self, pos: Pos) -> Option<&mut T> {
    self.grid.get_mut(pos.transpose())
  }

  fn transpose_mut(&mut self) -> impl MutGridlike<T> {
    &mut self.grid
  }
}

impl<G, T> Gridlike<T> for &G
where
  G: Gridlike<T>,
{
  fn width(&self) -> u32 {
    (**self).width()
  }
  fn height(&self) -> u32 {
    (**self).height()
  }
  fn in_bounds(&self, pos: Pos) -> bool {
    (**self).in_bounds(pos)
  }
  fn get(&self, pos: Pos) -> Option<&T> {
    (**self).get(pos)
  }
  fn iter_row<'a, 'b>(&'a self, y: u32) -> impl Iterator<Item = &'b T>
  where
    'a: 'b,
    T: 'a,
  {
    (**self).iter_row(y)
  }
  fn iter_col<'a, 'b>(&'a self, x: u32) -> impl Iterator<Item = &'b T>
  where
    'a: 'b,
    T: 'a,
  {
    (**self).iter_col(x)
  }
  fn transpose(&self) -> impl Gridlike<T> {
    (**self).transpose()
  }
}

impl<G, T> Gridlike<T> for &mut G
where
  G: Gridlike<T>,
{
  fn width(&self) -> u32 {
    (**self).width()
  }
  fn height(&self) -> u32 {
    (**self).height()
  }
  fn in_bounds(&self, pos: Pos) -> bool {
    (**self).in_bounds(pos)
  }
  fn get(&self, pos: Pos) -> Option<&T> {
    (**self).get(pos)
  }
  fn iter_row<'a, 'b>(&'a self, y: u32) -> impl Iterator<Item = &'b T>
  where
    'a: 'b,
    T: 'a,
  {
    (**self).iter_row(y)
  }
  fn iter_col<'a, 'b>(&'a self, x: u32) -> impl Iterator<Item = &'b T>
  where
    'a: 'b,
    T: 'a,
  {
    (**self).iter_col(x)
  }
  fn transpose(&self) -> impl Gridlike<T> {
    (**self).transpose()
  }
}

impl<G, T> MutGridlike<T> for &mut G
where
  G: MutGridlike<T>,
{
  fn get_mut(&mut self, pos: Pos) -> Option<&mut T> {
    (**self).get_mut(pos)
  }
  fn transpose_mut(&mut self) -> impl MutGridlike<T> {
    (**self).transpose_mut()
  }
}
