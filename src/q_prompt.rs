use std::any::Any;

use common::config::MAX_CLUE_LEN;
use termgame::{draw::Draw, entity::Entity, window::WindowDimensions, Key};
use util::{error::TermgameResult, pos::Pos};

use crate::textbox::TextBox;

pub struct QPrompt {
  question: TextBox,
  answer: TextBox,
  clue: TextBox,
  on_answer: bool,
}

impl QPrompt {
  pub fn new() -> Self {
    Self {
      question: TextBox::new(
        Pos { x: 10, y: 10 },
        "Enter a custom clue and answer:".to_owned(),
        50,
      ),
      answer: TextBox::new(
        Pos { x: 10, y: 12 },
        "____________".to_owned(),
        MAX_CLUE_LEN,
      ),
      clue: TextBox::new(Pos { x: 10, y: 14 }, "".to_owned(), 50),
      on_answer: true,
    }
  }
}

impl Entity for QPrompt {
  fn iterate_tiles<'a>(
    &'a self,
    window_dimensions: &'a WindowDimensions,
  ) -> Box<dyn Iterator<Item = (Draw, Pos)> + 'a> {
    Box::new(
      self
        .question
        .iterate_tiles(window_dimensions)
        .chain(self.answer.iterate_tiles(window_dimensions))
        .chain(self.clue.iterate_tiles(window_dimensions)),
    )
  }

  fn as_any(&self) -> &dyn Any {
    self
  }
  fn as_any_mut(&mut self) -> &mut dyn Any {
    self
  }

  fn keypress(&mut self, key: Key) -> TermgameResult {
    match key {
      Key::Char('\t') => {
        self.on_answer = !self.on_answer;
      }
      Key::Char('\n') => {}
      Key::Char(letter) => {
        if self.on_answer {
          if letter.is_ascii_lowercase() {
            let text = self.answer.text_mut();
            if let Some(idx) = text.chars().position(|c| c == '_') {
              let (lh, rh) = text.split_at(idx);
              let mut new_text = lh.to_owned();
              new_text.push(letter);
              new_text.push_str(&rh[1..]);
              *text = new_text;
            }
          }
        } else {
          let text = self.clue.text_mut();
          text.push(letter);
        }
      }
      Key::Backspace => {
        if self.on_answer {
          let text = self.answer.text_mut();
          if let Some(idx) = text.chars().rev().position(|c| c != '_') {
            let idx = MAX_CLUE_LEN as usize - idx - 1;
            let (lh, rh) = text.split_at(idx);
            let mut new_text = lh.to_owned();
            new_text.push('_');
            new_text.push_str(&rh[1..]);
            *text = new_text;
          }
        } else {
          let text = self.clue.text_mut();
          text.pop();
        }
      }
      _ => {}
    }

    Ok(())
  }
}
