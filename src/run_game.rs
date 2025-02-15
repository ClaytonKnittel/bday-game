use std::fs;

use common::msg::ServerMessage;
use termgame::event_loop::EventLoop;
use util::{
  bitcode,
  error::{TermgameError, TermgameResult},
};

use crate::{client::Client, crossword::CrosswordEntity};

pub async fn play_puzzle() -> TermgameResult {
  let mut client = Client::new().await?;

  let uid = client
    .pending_server_messages()
    .find_map(|message| {
      message
        .map(|message| match message {
          ServerMessage::NewConnection { uid } => Some(uid),
          _ => None,
        })
        .transpose()
    })
    .ok_or_else(|| TermgameError::Internal("No UID found!".to_owned()))??;

  let mut ev = EventLoop::new()?;
  let grid = bitcode::decode(&fs::read("xword_gen/crossword.bin")?)?;
  let xword_uid = ev
    .scene()
    .add_entity(Box::new(CrosswordEntity::from_grid(grid, uid)));

  let mut ev_iter = ev.async_event_loop();
  for t in 0usize.. {
    if !ev_iter.poll(&mut ev).await? {
      break;
    }

    let scene = ev.scene();
    let xword: &mut CrosswordEntity = scene.entity_mut(xword_uid)?;

    for message in client.pending_server_messages() {
      match message? {
        ServerMessage::NewConnection { uid: _ } => {}
        ServerMessage::ConnectToExisting { success: _ } => {}
        ServerMessage::PlayerPositionUpdate { uid, pos } => {
          if let Some(player_info) = xword.player_info_mut(uid) {
            player_info.pos = pos;
          }
        }
        ServerMessage::Ping => {}
      }
    }

    for action in xword.take_actions() {
      client.send_message(action).await?;
    }

    let (screen_width, screen_height) = (xword.screen_width(), xword.screen_height());
    let pos = xword.player_screen_pos();

    let window = ev.window();
    let (width, height) = (window.width() as i32, window.height() as i32);
    let camera_pos = window.camera_pos_mut();

    camera_pos.x = (pos.x - width / 2)
      .max(0)
      .min(screen_width.saturating_sub(width as u32) as i32);
    camera_pos.y = (pos.y - height / 2)
      .max(0)
      .min(screen_height.saturating_sub(height as u32) as i32);
  }

  Ok(())
}
