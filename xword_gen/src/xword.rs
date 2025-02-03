use std::collections::HashSet;

use util::{
  error::{TermgameError, TermgameResult},
  pos::Pos,
};

#[derive(Clone, Debug)]
pub struct XWord {
  board: Vec<bool>,
  bank: HashSet<String>,
  width: u32,
  height: u32,
}

impl XWord {
  pub fn from_layout(board: &str, bank: HashSet<String>) -> TermgameResult<Self> {
    let (width, height, board) = board.lines().try_fold(
      (None, 0, vec![]),
      |(width, height, mut board), line| -> TermgameResult<_> {
        let line = line.trim();
        board.extend(
          line
            .chars()
            .map(|c| match c {
              '_' => Ok(true),
              'X' => Ok(false),
              _ => Err(TermgameError::Parse(format!("Unrecognized board character '{c}'")).into()),
            })
            .collect::<TermgameResult<Vec<_>>>()?,
        );
        if let Some(width) = width {
          if line.len() != width {
            return Err(
              TermgameError::Parse(format!(
                "Board line lengths differ: {} vs {width}",
                line.len()
              ))
              .into(),
            );
          }
        }

        Ok((Some(line.len()), height + 1, board))
      },
    )?;

    Ok(Self {
      board,
      bank,
      width: width.ok_or_else(|| TermgameError::Parse("Empty board string".to_owned()))? as u32,
      height: height as u32,
    })
  }

  pub fn available(&self, pos: Pos) -> bool {
    assert!(pos.x >= 0 && pos.x < self.width as i32 && pos.y >= 0 && pos.y < self.height as i32);
    *self
      .board
      .get((pos.x + pos.y * self.width as i32) as usize)
      .unwrap()
  }
}

#[cfg(test)]
mod tests {
  use std::collections::HashSet;

  use googletest::prelude::*;
  use util::pos::Pos;

  use super::XWord;

  #[gtest]
  fn test_empty() {
    let xword = XWord::from_layout("", HashSet::new());
    expect_that!(xword, err(anything()));
  }

  #[gtest]
  fn test_simple() {
    let xword = XWord::from_layout(
      "__
       X_",
      HashSet::new(),
    );

    assert_that!(xword, ok(anything()));
    let xword = xword.unwrap();
    expect_true!(xword.available(Pos { x: 0, y: 0 }));
    expect_true!(xword.available(Pos { x: 1, y: 0 }));
    expect_false!(xword.available(Pos { x: 0, y: 1 }));
    expect_true!(xword.available(Pos { x: 1, y: 1 }));
  }
}
