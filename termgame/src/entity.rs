use std::{any::Any, io::Write};

use termion::event::Key;

use crate::{draw::Draw, error::TermgameResult, pos::Pos, window::Window};

pub trait Entity: Any {
  fn iterate_tiles(&self) -> Box<dyn Iterator<Item = (Draw, Pos)> + '_>;

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
    self.iterate_tiles().for_each(|(draw, pos)| {
      window.draw(draw, pos);
    })
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
