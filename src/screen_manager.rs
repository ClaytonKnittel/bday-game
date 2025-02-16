use common::msg::ClientMessage;
use termgame::{entity::Entity, Key};
use util::error::TermgameResult;

use crate::{crossword::CrosswordEntity, q_prompt::QPrompt};

#[allow(clippy::large_enum_variant)]
pub enum ScreenManager {
  Prompt(QPrompt),
  Crossword(CrosswordEntity),
}

impl ScreenManager {
  pub fn new(uid: u64) -> Self {
    Self::Prompt(QPrompt::new(uid))
  }

  pub fn start_crossword(&mut self, crossword: CrosswordEntity) {
    *self = Self::Crossword(crossword)
  }

  pub fn take_actions(&mut self) -> Vec<ClientMessage> {
    match self {
      Self::Prompt(q) => q.take_actions(),
      Self::Crossword(c) => c.take_actions(),
    }
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
