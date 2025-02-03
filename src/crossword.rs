use termgame::{color, draw::Draw, entity::Entity, pos::Pos};

const Z_IDX: i32 = 5;

pub struct Crossword {
  w: u32,
  h: u32,
}

impl Crossword {
  pub fn new(w: u32, h: u32) -> Self {
    Self { w, h }
  }
}

impl Entity for Crossword {
  fn iterate_tiles(&self) -> Box<dyn Iterator<Item = (Draw, Pos)> + '_> {
    Box::new((0..self.h).flat_map(|y| {
      (0..self.w).map(move |x| {
        let tile = if x % 4 == 0 && y % 2 == 0 {
          '+'
        } else if x % 4 == 0 {
          '|'
        } else if y % 2 == 0 {
          '-'
        } else {
          ' '
        };
        let col = color::AnsiValue::grayscale(20);

        (
          Draw::new(tile).with_fg(col).with_z(Z_IDX),
          Pos {
            x: x as i32,
            y: y as i32,
          },
        )
      })
    }))
  }

  fn tick(&mut self, _t: usize) {}

  fn click(&mut self, _pos: Pos) {}
  fn drag(&mut self, _pos: Pos) {}
  fn release(&mut self, _pos: Pos) {}
}
