use termgame::{entity::Entity, Key};
use util::error::TermgameResult;

use crate::{crossword::CrosswordEntity, q_prompt::QPrompt};

#[allow(clippy::large_enum_variant)]
pub enum ScreenManager {
  Prompt(QPrompt),
  Crossword(CrosswordEntity),
}

impl ScreenManager {
  pub fn new() -> Self {
    Self::Prompt(QPrompt::new())
  }

  pub fn start_crossword(&mut self, crossword: CrosswordEntity) {
    *self = Self::Crossword(crossword)
  }
}

impl Entity for ScreenManager {
  fn iterate_tiles<'a>(
    &'a self,
    window_dimensions: &'a termgame::window::WindowDimensions,
  ) -> Box<dyn Iterator<Item = (termgame::draw::Draw, util::pos::Pos)> + 'a> {
    match self {
      Self::Prompt(q) => q.iterate_tiles(window_dimensions),
      Self::Crossword(c) => c.iterate_tiles(window_dimensions),
    }
  }

  fn as_any(&self) -> &dyn std::any::Any {
    self
  }
  fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
    self
  }

  fn keypress(&mut self, key: Key) -> TermgameResult {
    match self {
      Self::Prompt(q) => q.keypress(key),
      Self::Crossword(c) => c.keypress(key),
    }
  }
}
