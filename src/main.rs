use termgame::{
  color, draw::Draw, entity::Entity, error::TermgameResult, event_loop::EventLoop, pos::Pos,
};

const Z_IDX: i32 = 20;

pub struct Track {
  y: u32,
  width: u32,
}

impl Track {
  pub fn new(y: u32, width: u32) -> Self {
    Self { y, width }
  }
}

impl Entity for Track {
  fn iterate_tiles(&self) -> Box<dyn Iterator<Item = (Draw, (i32, i32))> + '_> {
    Box::new((0..self.width).map(|x| {
      let tile = if x % 4 == 1 { '+' } else { '=' };
      let col = color::AnsiValue::grayscale(20);

      (
        Draw::new(tile).with_fg(col).with_z(Z_IDX),
        (x as i32, self.y as i32),
      )
    }))
  }

  fn tick(&mut self, _t: usize) {}

  fn click(&mut self, _pos: Pos) {}
  fn drag(&mut self, _pos: Pos) {}
  fn release(&mut self, _pos: Pos) {}
}

fn main() -> TermgameResult {
  let mut ev = EventLoop::new()?;
  ev.scene().add_entity(Box::new(Track::new(0, 100)));
  ev.run_event_loop()
}
