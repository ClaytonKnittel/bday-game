use std::fmt::Display;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Variant2<T, U> {
  Opt1(T),
  Opt2(U),
}

impl<T, U> Display for Variant2<T, U>
where
  T: Display,
  U: Display,
{
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Self::Opt1(t) => write!(f, "{t}"),
      Self::Opt2(u) => write!(f, "{u}"),
    }
  }
}

impl<T, U, R> Iterator for Variant2<T, U>
where
  T: Iterator<Item = R>,
  U: Iterator<Item = R>,
{
  type Item = R;

  fn next(&mut self) -> Option<R> {
    match self {
      Self::Opt1(t) => t.next(),
      Self::Opt2(u) => u.next(),
    }
  }
}
