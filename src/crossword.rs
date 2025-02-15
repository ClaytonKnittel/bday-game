use std::{collections::HashMap, iter, mem};

use common::{
  crossword::Crossword,
  msg::ClientMessage,
  player_info::{PlayerColor, PlayerInfo},
};
use termgame::{color, draw::Draw, entity::Entity, Key};
use util::{
  error::TermgameResult,
  grid::{Grid, Gridlike},
  pos::{Diff, Pos},
};
use xword_gen::xword::XWordTile;

const Z_IDX: i32 = 5;

#[derive(Clone, Copy, Debug)]
enum CrosswordView {
  Expanded,
  Compressed,
}

pub struct PlayerInfoManager {
  uid: u64,
  player_info: PlayerInfo,
  other_player_info: HashMap<u64, PlayerInfo>,
  other_player_pos_map: HashMap<Pos, Vec<u64>>,
}

impl PlayerInfoManager {
  fn new(uid: u64) -> Self {
    Self {
      uid,
      player_info: PlayerInfo {
        pos: Pos::zero(),
        color: color::AnsiValue::rgb(1, 2, 3).into(),
      },
      other_player_info: HashMap::new(),
      other_player_pos_map: HashMap::new(),
    }
  }

  fn refresh(&mut self, mut player_info: HashMap<u64, PlayerInfo>) {
    if let Some(mut player_info) = player_info.remove(&self.uid) {
      // Don't overwrite pos.
      player_info.pos = self.player_info.pos;
      self.player_info = player_info;
    }
    self.other_player_info = player_info;

    self.other_player_pos_map.clear();
    for (uid, PlayerInfo { pos, .. }) in self.other_player_info.iter() {
      self
        .other_player_pos_map
        .entry(*pos)
        .or_default()
        .push(*uid);
    }
  }

  fn player_pos(&self) -> Pos {
    self.player_info.pos
  }

  pub fn update_player_pos(&mut self, uid: u64, pos: Pos) {
    if let Some(player_info) = self.other_player_info.get_mut(&uid) {
      let old_pos = player_info.pos;
      player_info.pos = pos;

      if let Some(poses) = self.other_player_pos_map.get_mut(&old_pos) {
        if let Some(idx) = poses.iter().position(|other_uid| *other_uid == uid) {
          poses.remove(idx);
          if poses.is_empty() {
            self.other_player_pos_map.remove(&pos);
          }
        }
      }
    }
    self.other_player_pos_map.entry(pos).or_default().push(uid);
  }
}

pub struct CrosswordEntity {
  crossword: Crossword,
  view: CrosswordView,
  to_right: bool,
  actions: Vec<ClientMessage>,
  player_info: PlayerInfoManager,
}

impl CrosswordEntity {
  fn char_display(c: char) -> char {
    c.to_ascii_uppercase()
  }

  fn xscale(&self) -> i32 {
    match self.view {
      CrosswordView::Expanded => 4,
      CrosswordView::Compressed => 2,
    }
  }

  fn yscale(&self) -> i32 {
    match self.view {
      CrosswordView::Expanded => 2,
      CrosswordView::Compressed => 1,
    }
  }

  pub fn from_grid(grid: Grid<XWordTile>, uid: u64) -> Self {
    Self {
      crossword: Crossword::from_grid(grid),
      view: CrosswordView::Expanded,
      to_right: true,
      actions: vec![],
      player_info: PlayerInfoManager::new(uid),
    }
  }

  pub fn swap_for(&mut self, xword: Crossword) {
    self.crossword = xword;
  }

  pub fn player_info_manager_mut(&mut self) -> &mut PlayerInfoManager {
    &mut self.player_info
  }

  pub fn refresh_player_info(&mut self, player_info: HashMap<u64, PlayerInfo>) {
    self.player_info.refresh(player_info);
  }

  pub fn width(&self) -> u32 {
    self.crossword.width()
  }

  pub fn height(&self) -> u32 {
    self.crossword.height()
  }

  pub fn player_screen_pos(&self) -> Pos {
    Pos {
      x: self.player_info.player_pos().x * self.xscale() + self.xscale() / 2,
      y: self.player_info.player_pos().y * self.yscale() + self.yscale() / 2,
    }
  }

  pub fn screen_width(&self) -> u32 {
    self.width() * self.xscale() as u32 + 1
  }

  pub fn screen_height(&self) -> u32 {
    self.height() * self.yscale() as u32 + 1
  }

  pub fn take_actions(&mut self) -> Vec<ClientMessage> {
    let mut actions = vec![];
    mem::swap(&mut actions, &mut self.actions);
    actions
  }

  pub fn tile_mut(&mut self, pos: Pos) -> TermgameResult<&mut XWordTile> {
    self.crossword.tile_mut(pos)
  }

  fn should_highlight(&self, pos: Pos) -> bool {
    pos != self.player_info.player_pos()
      && self
        .crossword
        .clue_map()
        .get(&(pos, self.to_right))
        .is_some_and(|row_id| {
          self
            .crossword
            .clue_map()
            .get(&(self.player_info.player_pos(), self.to_right))
            .is_some_and(|player_row_id| row_id == player_row_id)
        })
  }

  fn pos_player_highlight_color(&self, pos: Pos) -> Option<PlayerColor> {
    if self.player_info.player_pos() == pos {
      Some(self.player_info.player_info.color)
    } else {
      self
        .player_info
        .other_player_pos_map
        .get(&pos)
        .and_then(|uid| uid.first())
        .and_then(|uid| {
          self
            .player_info
            .other_player_info
            .get(uid)
            .map(|player_info| player_info.color)
        })
    }
  }

  fn can_move_to(&self, pos: Pos) -> bool {
    (0..self.crossword.width() as i32).contains(&pos.x)
      && (0..self.crossword.height() as i32).contains(&pos.y)
      && !self.is_wall(pos)
  }

  fn next_free_tile_impl(&self, pos: Pos, delta: Diff) -> Pos {
    if self.is_wall(pos + delta) {
      return pos;
    }

    let pos = pos + delta;
    iter::successors(Some(pos), |&pos| {
      let pos = pos + delta;
      (!self.is_wall(pos)).then_some(pos)
    })
    .find(|&pos| self.is_empty(pos))
    .unwrap_or(pos)
  }

  fn find_next_free_tile(&self, pos: Pos) -> Pos {
    self.next_free_tile_impl(pos, if self.to_right { Diff::DX } else { Diff::DY })
  }

  fn find_prev_free_tile(&self, pos: Pos) -> Pos {
    let delta = if self.to_right { -Diff::DX } else { -Diff::DY };
    if self.is_wall(pos + delta) {
      pos
    } else {
      pos + delta
    }
  }

  fn is_empty(&self, pos: Pos) -> bool {
    self.crossword.is_empty(pos)
  }

  fn is_wall(&self, pos: Pos) -> bool {
    self.crossword.is_wall(pos)
  }

  fn cross_at(&self, pos: Pos) -> char {
    const CROSSES: [char; 16] = [
      // ┼           ╀           ┾           ╄
      '\u{253C}', '\u{2540}', '\u{253E}', '\u{2544}',
      // ╁           ╂           ╆           ╊
      '\u{2541}', '\u{2542}', '\u{2546}', '\u{254A}',
      // ┽           ╃           ┿           ╇
      '\u{253D}', '\u{2543}', '\u{253F}', '\u{2547}',
      // ╅           ╉           ╈           ╋
      '\u{2545}', '\u{2549}', '\u{2548}', '\u{254B}',
    ];

    let u_in_bounds = self.crossword.grid().in_bounds(pos + Diff { x: 0, y: -1 });
    let self_in_bounds = self.crossword.grid().in_bounds(pos + Diff { x: 0, y: 0 });
    let l_in_bounds = self.crossword.grid().in_bounds(pos + Diff { x: -1, y: 0 });

    let ul = self.is_wall(pos + Diff { x: -1, y: -1 });
    let ur = self.is_wall(pos + Diff { x: 0, y: -1 });
    let dl = self.is_wall(pos + Diff { x: -1, y: 0 });
    let dr = self.is_wall(pos + Diff { x: 0, y: 0 });

    let u_solid = ul || ur;
    let r_solid = ur || dr;
    let d_solid = dl || dr;
    let l_solid = ul || dl;

    if !l_in_bounds && !u_in_bounds {
      // ┏
      '\u{250F}'
    } else if !self_in_bounds {
      if !l_in_bounds && !u_in_bounds {
        // ┛
        '\u{251B}'
      } else if !l_in_bounds {
        if u_solid {
          // ┻
          '\u{253B}'
        } else {
          // ┷
          '\u{2537}'
        }
      } else if !u_in_bounds {
        if l_solid {
          // ┫
          '\u{252B}'
        } else {
          // ┨
          '\u{2528}'
        }
      } else {
        unreachable!();
      }
    } else if !l_in_bounds {
      if r_solid {
        // ┣
        '\u{2523}'
      } else {
        // ┠
        '\u{2520}'
      }
    } else if !u_in_bounds {
      if d_solid {
        // ┳
        '\u{2533}'
      } else {
        // ┯
        '\u{252F}'
      }
    } else {
      let idx = u_solid as usize
        + ((r_solid as usize) << 1)
        + ((d_solid as usize) << 2)
        + ((l_solid as usize) << 3);
      CROSSES[idx]
    }
  }

  fn v_bar_at(&self, pos: Pos) -> char {
    let l = self.is_wall(pos + Diff { x: -1, y: 0 });
    let r = self.is_wall(pos + Diff { x: 0, y: 0 });

    if l || r {
      // ┃
      '\u{2503}'
    } else {
      // │
      '\u{2502}'
    }
  }

  fn h_bar_at(&self, pos: Pos) -> char {
    let u = self.is_wall(pos + Diff { x: 0, y: -1 });
    let d = self.is_wall(pos + Diff { x: 0, y: 0 });

    if u || d {
      // ━
      '\u{2501}'
    } else {
      // ─
      '\u{2500}'
    }
  }

  fn generate_expanded_view(&self) -> Box<dyn Iterator<Item = (Draw, Pos)> + '_> {
    let col = color::AnsiValue::grayscale(20);

    Box::new(
      (0..self.height() as i32)
        .flat_map(move |y| {
          (0..self.width() as i32)
            .flat_map(move |x| {
              let pos = Pos { x, y };
              let screen_pos = Pos {
                x: x * self.xscale(),
                y: y * self.yscale(),
              };

              (0..self.yscale()).flat_map(move |dy| {
                (0..self.xscale()).flat_map(move |dx| {
                  let screen_pos = screen_pos + Diff { x: dx, y: dy };
                  let grid_pos = Pos { x, y };
                  let letter = self.crossword.tile(grid_pos).ok()?.clone();

                  let mut fg = col;
                  let mut bg = None;

                  if dx != 0 && dy != 0 {
                    if let Some(color) = self.pos_player_highlight_color(pos) {
                      fg = color::AnsiValue::grayscale(5);
                      bg = Some(color.into());
                    }
                  }

                  let center = dx == self.xscale() / 2 && dy == self.yscale() / 2;

                  let draw = if dx == 0 && dy == 0 {
                    Draw::new(self.cross_at(grid_pos))
                  } else if dx == 0 {
                    Draw::new(self.v_bar_at(grid_pos))
                  } else if dy == 0 {
                    Draw::new(self.h_bar_at(grid_pos))
                  } else {
                    let mut draw = match letter {
                      XWordTile::Letter(c) => {
                        if center {
                          Draw::new(Self::char_display(c))
                        } else {
                          Draw::new(' ')
                        }
                      }
                      XWordTile::Empty => Draw::new(' '),
                      XWordTile::Wall => {
                        fg = color::AnsiValue::grayscale(16);
                        Draw::new(if dx == 1 {
                          // ▐
                          '\u{2590}'
                        } else if dx == 2 {
                          // █
                          '\u{2588}'
                        } else {
                          // ▋
                          '\u{258B}'
                        })
                      }
                    };

                    if center && self.should_highlight(grid_pos) {
                      draw = draw.with_underline();
                    }
                    draw
                  };

                  let mut draw = draw.with_fg(fg).with_z(Z_IDX);
                  if let Some(bg) = bg {
                    draw = draw.with_bg(bg);
                  }
                  Some((draw, screen_pos))
                })
              })
            })
            .chain((0..self.yscale()).map(move |dy| {
              let grid_pos = Pos { x: self.width() as i32, y };
              let tile = if y == 0 && dy == 0 {
                // ┓
                '\u{2513}'
              } else if dy == 0 {
                self.cross_at(grid_pos)
              } else {
                self.v_bar_at(grid_pos)
              };
              (
                Draw::new(tile).with_fg(col).with_z(Z_IDX),
                Pos {
                  x: self.width() as i32 * self.xscale(),
                  y: y * self.yscale() + dy,
                },
              )
            }))
        })
        .chain((0..self.width() as i32).flat_map(move |x| {
          (0..self.xscale()).map(move |dx| {
            let grid_pos = Pos { x, y: self.height() as i32 };
            let tile = if x == 0 && dx == 0 {
              // ┗
              '\u{2517}'
            } else if dx == 0 {
              self.cross_at(grid_pos)
            } else {
              self.h_bar_at(grid_pos)
            };
            (
              Draw::new(tile).with_fg(col).with_z(Z_IDX),
              Pos {
                x: x * self.xscale() + dx,
                y: self.height() as i32 * self.yscale(),
              },
            )
          })
        }))
        .chain(iter::once((
          Draw::new(
            // ┛
            '\u{251B}',
          )
          .with_fg(col)
          .with_z(Z_IDX),
          Pos {
            x: self.width() as i32 * self.xscale(),
            y: self.height() as i32 * self.yscale(),
          },
        ))),
    )
  }

  fn generate_compressed_view(&self) -> Box<dyn Iterator<Item = (Draw, Pos)> + '_> {
    let col = color::AnsiValue::grayscale(20);

    Box::new((0..self.height() as i32).flat_map(move |y| {
      (0..self.width() as i32).map(move |x| {
        let pos = Pos { x, y };
        let screen_pos = Pos { x: x * 2, y };

        let tile = match self.crossword.tile(pos) {
          Ok(&XWordTile::Letter(c)) => Self::char_display(c),
          Ok(XWordTile::Wall) => 'X',
          Ok(XWordTile::Empty) => '_',
          Err(_) => unreachable!(),
        };
        let mut draw = Draw::new(tile).with_fg(col).with_z(Z_IDX);

        if pos == self.player_info.player_pos() {
          draw = draw.with_fg(color::AnsiValue::rgb(5, 0, 3));
        }

        (draw, screen_pos)
      })
    }))
  }
}

impl Entity for CrosswordEntity {
  fn iterate_tiles(&self) -> Box<dyn Iterator<Item = (Draw, Pos)> + '_> {
    match self.view {
      CrosswordView::Expanded => self.generate_expanded_view(),
      CrosswordView::Compressed => self.generate_compressed_view(),
    }
  }

  fn as_any(&self) -> &dyn std::any::Any {
    self
  }
  fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
    self
  }

  fn keypress(&mut self, key: Key) -> util::error::TermgameResult {
    let mut player_pos = self.player_info.player_pos();

    match key {
      Key::Char(letter @ 'a'..='z') => {
        let tile = self.tile_mut(player_pos)?;
        match tile {
          XWordTile::Empty => *tile = XWordTile::Letter(letter),
          XWordTile::Letter(_) => *tile = XWordTile::Letter(letter),
          XWordTile::Wall => {}
        }

        player_pos = self.find_next_free_tile(player_pos);
      }
      Key::Char('\t') => {
        self.to_right = !self.to_right;
      }
      Key::Backspace => {
        let tile = self.tile_mut(player_pos)?;
        match tile {
          XWordTile::Letter(_) => *tile = XWordTile::Empty,
          XWordTile::Empty => {
            player_pos = self.find_prev_free_tile(player_pos);
            let tile = self.tile_mut(player_pos)?;
            *tile = XWordTile::Empty;
          }
          XWordTile::Wall => {}
        }
      }
      Key::Char('/') => {
        self.view = match self.view {
          CrosswordView::Expanded => CrosswordView::Compressed,
          CrosswordView::Compressed => CrosswordView::Expanded,
        };
      }
      Key::Left => player_pos.x -= 1,
      Key::Right => player_pos.x += 1,
      Key::Up => player_pos.y -= 1,
      Key::Down => player_pos.y += 1,
      _ => {}
    }

    if self.can_move_to(player_pos) {
      self.player_info.player_info.pos = player_pos;
      self.actions.push(ClientMessage::PositionUpdate {
        uid: self.player_info.uid,
        pos: player_pos,
      });
    }

    Ok(())
  }
}
