use std::io::Write;

use crate::{draw::Draw, pos::Pos, window::Window};

pub trait Entity {
  fn iterate_tiles(&self) -> Box<dyn Iterator<Item = (Draw, (i32, i32))> + '_>;

  fn tick(&mut self, t: usize);

  fn click(&mut self, pos: Pos);
  fn drag(&mut self, pos: Pos);
  fn release(&mut self, pos: Pos);

  fn render<W: Write>(&self, window: &mut Window<W>)
  where
    Self: Sized,
  {
    self.iterate_tiles().for_each(|(draw, pos)| {
      if 0 <= pos.0 && pos.0 < window.width() as i32 && 0 <= pos.1 && pos.1 < window.height() as i32
      {
        window.draw(draw, (pos.0 as u32, pos.1 as u32));
      }
    })
  }
}
