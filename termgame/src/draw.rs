use std::fmt::Display;

use termion::{color, style};

#[derive(Clone)]
pub struct Draw {
  item: char,
  fg_color: Option<color::AnsiValue>,
  z_idx: i32,
  italic: bool,
}

impl Draw {
  pub fn new(item: char) -> Self {
    Self {
      item,
      fg_color: None,
      z_idx: 0,
      italic: false,
    }
  }

  pub fn item(&self) -> char {
    self.item
  }

  pub fn with_fg(self, color: color::AnsiValue) -> Self {
    Self {
      fg_color: Some(color),
      ..self
    }
  }

  pub fn with_z(self, z_idx: i32) -> Self {
    Self { z_idx, ..self }
  }

  pub fn z_idx(&self) -> i32 {
    self.z_idx
  }

  pub fn with_italic(self) -> Self {
    Self {
      italic: true,
      ..self
    }
  }
}

impl Display for Draw {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let fg_str = if let Some(color) = self.fg_color {
      color.fg_string()
    } else {
      color::Reset.fg_str().to_owned()
    };
    let italic_str = if self.italic {
      style::Italic.to_string()
    } else {
      "".to_owned()
    };
    write!(f, "{}{}{}{}", style::Reset, italic_str, fg_str, self.item)
  }
}

impl PartialEq for Draw {
  fn eq(&self, other: &Self) -> bool {
    self.item == other.item
      && self.z_idx == other.z_idx
      && self.italic == other.italic
      && match (self.fg_color, other.fg_color) {
        (Some(color::AnsiValue(c1)), Some(color::AnsiValue(c2))) => c1 == c2,
        (None, None) => true,
        _ => false,
      }
  }
}

impl Eq for Draw {}
