use termgame::{draw::Draw, entity::Entity, window::WindowDimensions};
use util::pos::Pos;

use crate::textbox::TextBox;

pub struct QPrompt {
  question: TextBox,
  answer: TextBox,
  clue: TextBox,
}

impl QPrompt {
  pub fn new() -> Self {
    Self {
      question: TextBox::new(
        Pos { x: 10, y: 10 },
        "Enter a custom clue and answer:".to_owned(),
        50,
      ),
      answer: TextBox::new(Pos { x: 10, y: 12 }, "_____________".to_owned(), 13),
      clue: TextBox::new(Pos { x: 10, y: 14 }, "".to_owned(), 50),
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

  fn as_any(&self) -> &dyn std::any::Any {
    self
  }
  fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
    self
  }
}
