use std::iter;

use termgame::{color::AnsiValue, draw::Draw, entity::Entity, window::WindowDimensions, Key};
use util::{error::TermgameResult, pos::Pos};

const Z_IDX: i32 = 100;

pub struct Pc {
  pos: Pos,
  color: AnsiValue,
}

impl Pc {
  pub fn new(pos: Pos, color: AnsiValue) -> Self {
    Self { pos, color }
  }

  pub fn pos(&self) -> Pos {
    self.pos
  }
}

impl Entity for Pc {
  fn iterate_tiles<'a>(
    &'a self,
    _: &'a WindowDimensions,
  ) -> Box<dyn Iterator<Item = (Draw, Pos)> + 'a> {
    Box::new(iter::once((
      Draw::new('C').with_fg(self.color).with_z(Z_IDX),
      self.pos,
    )))
  }

  fn as_any(&self) -> &dyn std::any::Any {
    self
  }
  fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
    self
  }

  // TODO: pass Scene to all interactive methods (or maybe just tick?),
  // lookup crossword with uid, use it to check that movement is valid.
  fn keypress(&mut self, key: Key) -> TermgameResult {
    match key {
      Key::Char('w') => {
        self.pos.y -= 1;
      }
      Key::Char('a') => {
        self.pos.x -= 2;
      }
      Key::Char('s') => {
        self.pos.y += 1;
      }
      Key::Char('d') => {
        self.pos.x += 2;
      }
      _ => {}
    }

    Ok(())
  }
}
