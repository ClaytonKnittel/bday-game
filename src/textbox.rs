use std::iter;

use termgame::{draw::Draw, entity::Entity};
use util::pos::Pos;

const Z_IDX: i32 = 50;

pub struct TextBox {
  src: Pos,
  text: String,
  line_len: u32,
  fixed_width: bool,
}

impl TextBox {
  pub fn new(src: Pos, text: String, line_len: u32) -> Self {
    Self { src, text, line_len, fixed_width: false }
  }

  pub fn with_fixed_width(self) -> Self {
    Self { fixed_width: true, ..self }
  }

  pub fn display_height(&self) -> u32 {
    self.to_lines().len() as u32 + 2
  }

  pub fn set_pos(&mut self, pos: Pos) {
    self.src = pos;
  }

  fn to_lines(&self) -> Vec<String> {
    let mut text = self.text.as_str();
    let mut lines = Vec::new();
    loop {
      if text.chars().count() <= self.line_len as usize {
        lines.push(text.to_string());
        return lines;
      }
      let last_idx = text.chars().take(self.line_len as usize).enumerate().fold(
        self.line_len as usize,
        |last_idx, (idx, c)| {
          if c == ' ' {
            idx
          } else {
            last_idx
          }
        },
      );
      lines.push(text[..last_idx].to_string());
      text = &text[last_idx + 1..];
    }
  }
}

/*
  +-------------+
  | Sample text |
  +-------------+
*/

impl Entity for TextBox {
  fn iterate_tiles(&self) -> Box<dyn Iterator<Item = (Draw, Pos)> + '_> {
    let lines = self.to_lines();
    let num_lines = lines.len() as i32;
    Box::new(
      self
        .fixed_width
        .then_some(self.line_len as usize)
        .or_else(|| lines.iter().map(|line| line.chars().count()).max())
        .map(|max_line_len| {
          let max_line_len = max_line_len as i32;
          let x = self.src.x;
          let y = self.src.y;

          [
            (Draw::new('+').with_z(Z_IDX), Pos { x, y }),
            (
              Draw::new('+').with_z(Z_IDX),
              Pos { x: x + max_line_len + 3, y },
            ),
            (
              Draw::new('+').with_z(Z_IDX),
              Pos { x, y: y - num_lines - 1 },
            ),
            (
              Draw::new('+').with_z(Z_IDX),
              Pos {
                x: x + max_line_len + 3,
                y: y - num_lines - 1,
              },
            ),
          ]
          .into_iter()
          .chain((0..max_line_len + 2).flat_map(move |dx| {
            [
              (Draw::new('-').with_z(Z_IDX), Pos { x: x + dx + 1, y }),
              (
                Draw::new('-').with_z(Z_IDX),
                Pos { x: x + dx + 1, y: y - num_lines - 1 },
              ),
            ]
          }))
          .chain((0..num_lines).flat_map(move |dy| {
            [
              (Draw::new('|').with_z(Z_IDX), Pos { x, y: y - dy - 1 }),
              (
                Draw::new('|').with_z(Z_IDX),
                Pos { x: x + max_line_len + 3, y: y - dy - 1 },
              ),
            ]
          }))
          .chain(lines.into_iter().enumerate().flat_map(move |(row, line)| {
            let row = row as i32;
            let y = y - num_lines + row;
            line
              .chars()
              .chain(iter::repeat(' '))
              .take(max_line_len as usize)
              .collect::<Vec<_>>()
              .into_iter()
              .enumerate()
              .map(move |(col, c)| (Draw::new(c).with_z(Z_IDX), Pos { x: x + col as i32 + 2, y }))
              .chain([
                (Draw::new(' ').with_z(Z_IDX), Pos { x: x + 1, y }),
                (
                  Draw::new(' ').with_z(Z_IDX),
                  Pos { x: x + max_line_len + 2, y },
                ),
              ])
          }))
        })
        .into_iter()
        .flatten(),
    )
  }

  fn as_any(&self) -> &dyn std::any::Any {
    self
  }
  fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
    self
  }
}
