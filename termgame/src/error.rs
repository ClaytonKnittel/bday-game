use core::fmt;
use std::{
  error::Error,
  fmt::{Display, Formatter},
};

#[derive(Debug)]
pub enum TermgameError {
  Render(String),
}

impl Display for TermgameError {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match self {
      TermgameError::Render(msg) => write!(f, "Render error: {msg}"),
    }
  }
}

impl Error for TermgameError {}

pub type TermgameResult<T = ()> = Result<T, Box<dyn Error>>;
