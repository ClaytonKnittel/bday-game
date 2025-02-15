use std::fmt::Display;

use termion::{color, style};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DrawStyle {
  Relative,
  FixedPosTopLeft,
  FixedPosTopRight,
  FixedPosBottomRight,
}

#[derive(Clone)]
pub struct Draw {
  item: char,
  fg_color: Option<color::AnsiValue>,
  bg_color: Option<color::AnsiValue>,
  z_idx: i32,
  italic: bool,
  bold: bool,
  underlined: bool,
  draw_style: DrawStyle,
}

impl Draw {
  pub fn new(item: char) -> Self {
    Self {
      item,
      fg_color: None,
      bg_color: None,
      z_idx: 0,
      italic: false,
      bold: false,
      underlined: false,
      draw_style: DrawStyle::Relative,
    }
  }

  pub fn item(&self) -> char {
    self.item
  }

  pub fn with_fg(self, color: color::AnsiValue) -> Self {
    Self { fg_color: Some(color), ..self }
  }

  pub fn with_bg(self, color: color::AnsiValue) -> Self {
    Self { bg_color: Some(color), ..self }
  }

  pub fn with_z(self, z_idx: i32) -> Self {
    Self { z_idx, ..self }
  }

  pub fn z_idx(&self) -> i32 {
    self.z_idx
  }

  pub fn with_italic(self) -> Self {
    Self { italic: true, ..self }
  }

  pub fn with_bold(self) -> Self {
    Self { bold: true, ..self }
  }

  pub fn with_underline(self) -> Self {
    Self { underlined: true, ..self }
  }

  pub fn with_draw_style(self, draw_style: DrawStyle) -> Self {
    Self { draw_style, ..self }
  }

  pub fn draw_style(&self) -> DrawStyle {
    self.draw_style
  }
}

impl Display for Draw {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let fg_str = if let Some(color) = self.fg_color {
      color.fg_string()
    } else {
      color::Reset.fg_str().to_owned()
    };
    let bg_str = if let Some(color) = self.bg_color {
      color.bg_string()
    } else {
      color::Reset.bg_str().to_owned()
    };
    let italic_str = if self.italic {
      style::Italic.to_string()
    } else {
      "".to_owned()
    };
    let bold_str = if self.bold {
      style::Bold.to_string()
    } else {
      "".to_owned()
    };
    let underline_str = if self.underlined {
      style::Underline.to_string()
    } else {
      "".to_owned()
    };
    write!(
      f,
      "{}{italic_str}{bold_str}{underline_str}{fg_str}{bg_str}{}",
      style::Reset,
      self.item
    )
  }
}

impl PartialEq for Draw {
  fn eq(&self, other: &Self) -> bool {
    self.item == other.item
      && self.z_idx == other.z_idx
      && self.italic == other.italic
      && self.bold == other.bold
      && self.underlined == other.underlined
      && self.draw_style == other.draw_style
      && match (self.fg_color, other.fg_color) {
        (Some(color::AnsiValue(c1)), Some(color::AnsiValue(c2))) => c1 == c2,
        (None, None) => true,
        _ => false,
      }
      && match (self.bg_color, other.bg_color) {
        (Some(color::AnsiValue(c1)), Some(color::AnsiValue(c2))) => c1 == c2,
        (None, None) => true,
        _ => false,
      }
  }
}

impl Eq for Draw {}
