use std::{
  fmt::Display,
  ops::{Add, AddAssign, Mul, Neg, Sub, SubAssign},
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
    Self { x: self.y, y: self.x }
  }
}

impl Sub for Pos {
  type Output = Diff;

  fn sub(self, rhs: Self) -> Diff {
    Diff { x: self.x - rhs.x, y: self.y - rhs.y }
  }
}

impl Sub<Diff> for Pos {
  type Output = Self;

  fn sub(self, rhs: Diff) -> Self {
    Self { x: self.x - rhs.x, y: self.y - rhs.y }
  }
}

impl SubAssign<Diff> for Pos {
  fn sub_assign(&mut self, rhs: Diff) {
    self.x -= rhs.x;
    self.y -= rhs.y;
  }
}

impl Add<Diff> for Pos {
  type Output = Self;

  fn add(self, rhs: Diff) -> Self {
    Self { x: self.x + rhs.x, y: self.y + rhs.y }
  }
}

impl AddAssign<Diff> for Pos {
  fn add_assign(&mut self, rhs: Diff) {
    self.x += rhs.x;
    self.y += rhs.y;
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
    Self { x: self.x + rhs.x, y: self.y + rhs.y }
  }
}

impl Mul<Diff> for i32 {
  type Output = Diff;

  fn mul(self, rhs: Diff) -> Diff {
    Diff { x: self * rhs.x, y: self * rhs.y }
  }
}

impl Mul<i32> for Diff {
  type Output = Diff;

  fn mul(self, rhs: i32) -> Self {
    Self { x: self.x * rhs, y: self.y * rhs }
  }
}

impl Neg for Diff {
  type Output = Diff;

  fn neg(self) -> Self::Output {
    Self { x: -self.x, y: -self.y }
  }
}

impl Display for Diff {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "({}, {})", self.x, self.y)
  }
}
