use crate::{
  error::{TermgameError, TermgameResult},
  pos::Pos,
};

pub trait Gridlike<T> {
  fn width(&self) -> u32;
  fn height(&self) -> u32;
  fn in_bounds(&self, pos: Pos) -> bool;

  fn get(&self, pos: Pos) -> Option<&T>;
  fn get_mut(&mut self, pos: Pos) -> Option<&mut T>;

  fn iter_row<'a, 'b>(&'a self, y: u32) -> impl Iterator<Item = &'b T>
  where
    'a: 'b,
    T: 'a;
  fn iter_col<'a, 'b>(&'a self, x: u32) -> impl Iterator<Item = &'b T>
  where
    'a: 'b,
    T: 'a;

  fn transpose(&mut self) -> impl Gridlike<T>;
}

#[derive(Clone, Debug)]
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

    Ok(Self {
      grid,
      width,
      height,
    })
  }

  fn idx(&self, pos: Pos) -> usize {
    debug_assert!(self.in_bounds(pos));
    let x = pos.x as usize;
    let y = pos.y as usize;
    x + y * self.width as usize
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

  fn get_mut(&mut self, pos: Pos) -> Option<&mut T> {
    self
      .in_bounds(pos)
      .then(|| {
        let index = self.idx(pos);
        self.grid.get_mut(index)
      })
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

  fn transpose(&mut self) -> impl Gridlike<T> {
    TransposeGrid { grid: self }
  }
}

pub struct TransposeGrid<'a, T> {
  grid: &'a mut Grid<T>,
}

impl<'a, T> Gridlike<T> for TransposeGrid<'a, T> {
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

  fn get_mut(&mut self, pos: Pos) -> Option<&mut T> {
    self.grid.get_mut(pos.transpose())
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

  fn transpose(&mut self) -> impl Gridlike<T> {
    &mut self.grid
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

  fn get_mut(&mut self, pos: Pos) -> Option<&mut T> {
    (**self).get_mut(pos)
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

  fn transpose(&mut self) -> impl Gridlike<T> {
    (**self).transpose()
  }
}
