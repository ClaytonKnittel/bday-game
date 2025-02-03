use std::io::Write;

use crate::{draw::Draw, pos::Pos, window::Window};

pub trait Entity {
  fn iterate_tiles(&self) -> Box<dyn Iterator<Item = (Draw, Pos)> + '_>;

  fn tick(&mut self, t: usize);

  fn click(&mut self, pos: Pos);
  fn drag(&mut self, pos: Pos);
  fn release(&mut self, pos: Pos);

  fn render<W: Write>(&self, window: &mut Window<W>)
  where
    Self: Sized,
  {
    self.iterate_tiles().for_each(|(draw, pos)| {
      window.draw(draw, pos);
    })
  }
}
