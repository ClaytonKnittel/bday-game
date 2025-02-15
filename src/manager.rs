use termgame::{entity::Entity, Key};
use util::{error::TermgameResult, pos::Pos};

use crate::crossword::Crossword;

/// Poll the server every 2 frames.
const POLL_PERIOD: usize = 2;

struct Manager {
  xword: Crossword,
}

impl Manager {
  fn visit_mut<F, T>(&mut self, mut f: F, arg: T) -> TermgameResult
  where
    F: FnMut(&mut dyn Entity, T) -> TermgameResult,
    T: Clone,
  {
    f(&mut self.xword, arg.clone())?;
    Ok(())
  }
}

impl Entity for Manager {
  fn iterate_tiles(&self) -> Box<dyn Iterator<Item = (termgame::draw::Draw, util::pos::Pos)> + '_> {
    self.xword.iterate_tiles()
  }

  fn as_any(&self) -> &dyn std::any::Any {
    self
  }
  fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
    self
  }

  fn tick(&mut self, t: usize) -> TermgameResult {
    if t % POLL_PERIOD == 0 {}
    self.visit_mut(Entity::tick, t)
  }

  fn keypress(&mut self, key: Key) -> TermgameResult {
    self.visit_mut(Entity::keypress, key)
  }

  fn click(&mut self, pos: Pos) -> TermgameResult {
    self.visit_mut(Entity::click, pos)
  }

  fn drag(&mut self, pos: Pos) -> TermgameResult {
    self.visit_mut(Entity::drag, pos)
  }

  fn release(&mut self, pos: Pos) -> TermgameResult {
    self.visit_mut(Entity::release, pos)
  }
}
