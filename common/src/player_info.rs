use bitcode::{Decode, Encode};
use termgame::color;
use util::pos::Pos;

#[derive(Clone, Copy, Debug, Encode, Decode)]
pub struct PlayerColor(u8);

impl From<color::AnsiValue> for PlayerColor {
  fn from(value: color::AnsiValue) -> Self {
    Self(value.0)
  }
}

impl From<PlayerColor> for color::AnsiValue {
  fn from(value: PlayerColor) -> Self {
    Self(value.0)
  }
}

#[derive(Clone, Debug, Encode, Decode)]
pub struct PlayerInfo {
  pub pos: Pos,
  pub color: PlayerColor,
}
