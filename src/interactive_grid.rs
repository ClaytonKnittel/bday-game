use bitcode::{Decode, Encode};
use termgame::{color, draw::Draw, entity::Entity};
use util::{
  grid::{Grid, Gridlike, MutGridlike},
  pos::{Diff, Pos},
};

#[derive(PartialEq, Eq, PartialOrd, Ord)]
enum Satisfaction {
  Good,
  Ok,
  Bad,
}

#[derive(Encode, Decode)]
pub struct InteractiveGrid {
  grid: Grid<bool>,
}

impl InteractiveGrid {
  pub fn new(width: u32, height: u32) -> Self {
    Self {
      grid: Grid::new(width, height),
    }
  }

  pub fn screen_width(&self) -> u32 {
    self.grid.width() * 2 - 1
  }

  pub fn screen_height(&self) -> u32 {
    self.grid.height()
  }

  fn length_sat(length: u32) -> Satisfaction {
    if length < 3 {
      Satisfaction::Bad
    } else if length < 5 {
      Satisfaction::Ok
    } else if length < 9 {
      Satisfaction::Good
    } else if length < 12 {
      Satisfaction::Ok
    } else {
      Satisfaction::Bad
    }
  }

  fn tile_sat(&self, pos: Pos) -> Satisfaction {
    // row sat:
    let row_length = 1
      + (1..)
        .map_while(|dx| {
          self
            .grid
            .get(pos + Diff { x: dx, y: 0 })
            .is_some_and(|&full| !full)
            .then_some(())
        })
        .count()
      + (1..)
        .map_while(|dx| {
          self
            .grid
            .get(pos + Diff { x: -dx, y: 0 })
            .is_some_and(|&full| !full)
            .then_some(())
        })
        .count();
    // col sat:
    let col_length = 1
      + (1..)
        .map_while(|dy| {
          self
            .grid
            .get(pos + Diff { x: 0, y: dy })
            .is_some_and(|&full| !full)
            .then_some(())
        })
        .count()
      + (1..)
        .map_while(|dy| {
          self
            .grid
            .get(pos + Diff { x: 0, y: -dy })
            .is_some_and(|&full| !full)
            .then_some(())
        })
        .count();

    Self::length_sat(row_length as u32).max(Self::length_sat(col_length as u32))
  }
}

impl Entity for InteractiveGrid {
  fn iterate_tiles(&self) -> Box<dyn Iterator<Item = (Draw, Pos)> + '_> {
    let sat_color = |sat: Satisfaction| match sat {
      Satisfaction::Good => color::AnsiValue::rgb(0, 4, 0),
      Satisfaction::Ok => color::AnsiValue::rgb(5, 3, 0),
      Satisfaction::Bad => color::AnsiValue::rgb(5, 0, 0),
    };

    Box::new((0..self.grid.height() as i32).flat_map(move |y| {
      (0..self.grid.width() as i32).map(move |x| {
        let pos = Pos { x, y };
        let draw = if self.grid.get(pos).is_some_and(|&selected| selected) {
          Draw::new('*')
        } else {
          Draw::new('_').with_fg(sat_color(self.tile_sat(pos)))
        };
        (draw, Pos { x: 2 * x, y })
      })
    }))
  }

  fn as_any(&self) -> &dyn std::any::Any {
    self
  }
  fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
    self
  }

  fn click(&mut self, pos: Pos) -> util::error::TermgameResult {
    if let Some(tile) = self.grid.get_mut(Pos {
      x: pos.x / 2,
      ..pos
    }) {
      *tile = !*tile;
    }
    Ok(())
  }

  fn drag(&mut self, pos: Pos) -> util::error::TermgameResult {
    if pos.x % 2 == 0 {
      self.click(pos)
    } else {
      Ok(())
    }
  }
}
