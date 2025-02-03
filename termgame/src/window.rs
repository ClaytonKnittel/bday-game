use std::io::Write;
use termion::cursor;

use crate::{draw::Draw, pos::Pos};

pub struct Window<W: Write> {
  stdout: W,
  width: u32,
  height: u32,
  canvas: Vec<Option<Draw>>,
  prev_canvas: Vec<Option<Draw>>,
}

impl<W: Write> Window<W> {
  pub fn new(stdout: W, width: u32, height: u32) -> Self {
    let mut s = Self {
      stdout,
      width,
      height,
      canvas: (0..(width * height)).map(|_| None).collect(),
      prev_canvas: (0..(width * height)).map(|_| None).collect(),
    };
    #[allow(clippy::expect_used)]
    s.allocate().expect("Failed to initialize window");
    s
  }

  pub fn width(&self) -> u32 {
    self.width
  }

  pub fn height(&self) -> u32 {
    self.height
  }

  fn idx_to_pos(&self, idx: usize) -> (u32, u32) {
    (idx as u32 % self.width, idx as u32 / self.width)
  }

  fn idx(&self, x: u32, y: u32) -> usize {
    (x + y * self.width) as usize
  }

  fn get(&self, x: u32, y: u32) -> &Option<Draw> {
    #[allow(clippy::unwrap_used)]
    self.canvas.get(self.idx(x, y)).unwrap()
  }

  fn get_mut(&mut self, x: u32, y: u32) -> &mut Option<Draw> {
    let idx = self.idx(x, y);
    #[allow(clippy::unwrap_used)]
    self.canvas.get_mut(idx).unwrap()
  }

  fn allocate(&mut self) -> std::io::Result<()> {
    write!(self.stdout, "{}{}", termion::clear::All, cursor::Goto(1, 1))
  }

  pub fn reset(&mut self) {
    std::mem::swap(&mut self.prev_canvas, &mut self.canvas);
    self.canvas = (0..(self.width * self.height)).map(|_| None).collect();
  }

  pub fn draw(&mut self, draw: Draw, pos: Pos) {
    let (x, y) = (pos.x, pos.y);
    if 0 > x || x >= self.width() as i32 || 0 > y || y >= self.height() as i32 {
      return;
    }

    let (x, y) = (x as u32, y as u32);
    *self.get_mut(x, y) = Some(self.get(x, y).clone().map_or(draw.clone(), |cur_el| {
      if cur_el.z_idx() < draw.z_idx() {
        draw
      } else {
        cur_el
      }
    }))
  }

  pub fn render(&mut self) -> std::io::Result<()> {
    let ((min_x, max_x), (min_y, max_y)) = self
      .canvas
      .iter()
      .zip(self.prev_canvas.iter())
      .enumerate()
      .fold(
        ((u32::MAX, 0), (u32::MAX, 0)),
        |((min_x, max_x), (min_y, max_y)), (idx, (d1, d2))| {
          if d1 != d2 {
            let (x, y) = self.idx_to_pos(idx);
            ((min_x.min(x), max_x.max(x)), (min_y.min(y), max_y.max(y)))
          } else {
            ((min_x, max_x), (min_y, max_y))
          }
        },
      );

    // Don't render if no change.
    if max_x < min_x {
      return Ok(());
    }

    for y in min_y..=max_y {
      write!(
        self.stdout,
        "{}",
        cursor::Goto((min_x + 1) as u16, (y + 1) as u16)
      )?;
      for x in min_x..=max_x {
        if let Some(draw) = self.get(x, y).clone() {
          write!(self.stdout, "{}", draw)?;
        } else {
          write!(self.stdout, " ")?;
        }
      }
    }
    write!(self.stdout, "{}", cursor::Goto(0, (self.height + 1) as u16))?;
    self.stdout.flush()
  }
}
