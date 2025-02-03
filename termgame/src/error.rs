use core::fmt;
use std::{
  error::Error,
  fmt::{Display, Formatter},
};

#[derive(Debug)]
pub enum TermgameError {
  Render(String),
  Internal(String),
}

impl Display for TermgameError {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match self {
      TermgameError::Render(msg) => write!(f, "Render error: {msg}"),
      TermgameError::Internal(msg) => write!(f, "Internal error: {msg}"),
    }
  }
}

impl Error for TermgameError {}

pub type TermgameResult<T = ()> = Result<T, Box<dyn Error>>;
