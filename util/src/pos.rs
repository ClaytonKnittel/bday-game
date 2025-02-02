use std::ops::{Add, Sub};

#[derive(Clone, Copy)]
pub struct Pos {
  pub x: i32,
  pub y: i32,
}

impl Pos {
  pub const fn zero() -> Self {
    Self { x: 0, y: 0 }
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
