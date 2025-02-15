use std::{any::Any, io::Write};

use termion::event::Key;
use util::{error::TermgameResult, pos::Pos};

use crate::{
  draw::Draw,
  window::{Window, WindowDimensions},
};

pub trait Entity: Any {
  fn iterate_tiles<'a>(
    &'a self,
    window_dimensions: &'a WindowDimensions,
  ) -> Box<dyn Iterator<Item = (Draw, Pos)> + 'a>;

  fn as_any(&self) -> &dyn Any;
  fn as_any_mut(&mut self) -> &mut dyn Any;

  #[allow(unused)]
  fn tick(&mut self, t: usize) -> TermgameResult {
    Ok(())
  }

  #[allow(unused)]
  fn keypress(&mut self, key: Key) -> TermgameResult {
    Ok(())
  }

  #[allow(unused)]
  fn click(&mut self, pos: Pos) -> TermgameResult {
    Ok(())
  }
  #[allow(unused)]
  fn drag(&mut self, pos: Pos) -> TermgameResult {
    Ok(())
  }
  #[allow(unused)]
  fn release(&mut self, pos: Pos) -> TermgameResult {
    Ok(())
  }

  fn render<W: Write>(&self, window: &mut Window<W>)
  where
    Self: Sized,
  {
    let dimensions = window.window_dimensions().clone();
    for (draw, pos) in self.iterate_tiles(&dimensions) {
      window.draw(draw, pos);
    }
  }
}

pub trait EntityAsAny {
  fn as_any(&self) -> &dyn Any;
  fn as_any_mut(&mut self) -> &mut dyn Any;
}

impl<T: Entity> EntityAsAny for T {
  fn as_any(&self) -> &dyn Any {
    self
  }

  fn as_any_mut(&mut self) -> &mut dyn Any {
    self
  }
}
