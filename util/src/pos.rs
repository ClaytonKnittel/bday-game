use std::{
  fmt::Display,
  ops::{Add, Sub},
};

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct Pos {
  pub x: i32,
  pub y: i32,
}

impl Pos {
  pub const fn zero() -> Self {
    Self { x: 0, y: 0 }
  }

  pub const fn transpose(&self) -> Self {
    Self {
      x: self.y,
      y: self.x,
    }
  }
}

impl Sub for Pos {
  type Output = Diff;

  fn sub(self, rhs: Self) -> Diff {
    Diff {
      x: self.x - rhs.x,
      y: self.y - rhs.y,
    }
  }
}

impl Add<Diff> for Pos {
  type Output = Self;

  fn add(self, rhs: Diff) -> Self {
    Self {
      x: self.x + rhs.x,
      y: self.y + rhs.y,
    }
  }
}

impl Display for Pos {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "({}, {})", self.x, self.y)
  }
}

#[derive(Clone, Copy)]
pub struct Diff {
  pub x: i32,
  pub y: i32,
}

impl Add for Diff {
  type Output = Self;

  fn add(self, rhs: Self) -> Self {
    Self {
      x: self.x + rhs.x,
      y: self.y + rhs.y,
    }
  }
}

impl Display for Diff {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "({}, {})", self.x, self.y)
  }
}
