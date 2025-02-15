use std::collections::HashMap;

use common::{
  crossword::Crossword,
  msg::{ClientMessage, ServerMessage},
  player_info::PlayerInfo,
};
use termgame::event_loop::EventLoop;
use util::error::{TermgameError, TermgameResult};

use crate::{client::Client, crossword::CrosswordEntity};

const SYNC_PERIOD: usize = 5 * 50;

async fn wait_for_uid(client: &mut Client) -> TermgameResult<u64> {
  loop {
    if let Some(message) = client.recv_server_message().await {
      match message {
        ServerMessage::NewConnection { uid } => return Ok(uid),
        _ => continue,
      }
    } else {
      return Err(TermgameError::Internal("Connection closed".to_owned()).into());
    }
  }
}

async fn wait_for_refresh(
  client: &mut Client,
) -> TermgameResult<(Crossword, HashMap<u64, PlayerInfo>)> {
  loop {
    if let Some(message) = client.recv_server_message().await {
      match message {
        ServerMessage::FullRefresh { crossword, player_info } => {
          return Ok((crossword.into(), player_info))
        }
        _ => continue,
      }
    } else {
      return Err(TermgameError::Internal("Connection closed".to_owned()).into());
    }
  }
}

pub async fn play_puzzle() -> TermgameResult {
  let mut client = Client::new().await?;
  let uid = wait_for_uid(&mut client).await?;

  client.send_message(ClientMessage::FullRefresh).await?;
  let (crossword, player_info) = wait_for_refresh(&mut client).await?;

  let mut ev = EventLoop::new()?;
  let xword_uid = ev
    .scene()
    .add_entity(Box::new(CrosswordEntity::with_crossword_and_player_info(
      crossword,
      player_info,
      uid,
    )));

  let mut ev_iter = ev.async_event_loop();
  for t in 0usize.. {
    if !ev_iter.poll(&mut ev).await? {
      break;
    }

    let scene = ev.scene();
    let xword: &mut CrosswordEntity = scene.entity_mut(xword_uid)?;

    if t % SYNC_PERIOD == 0 {
      client.send_message(ClientMessage::FullRefresh).await?;
    }

    for message in client.pending_server_messages() {
      match message? {
        ServerMessage::NewConnection { uid: _ } => {}
        ServerMessage::ConnectToExisting { success: _ } => {}
        ServerMessage::PlayerPositionUpdate { uid, pos } => {
          xword.player_info_manager_mut().update_player_pos(uid, pos);
        }
        ServerMessage::TileUpdate { pos, tile } => {
          if let Ok(xword_tile) = xword.tile_mut(pos) {
            *xword_tile = tile;
          }
        }
        ServerMessage::FullRefresh { crossword, player_info } => {
          xword.swap_for(crossword.into());
          xword.refresh_player_info(player_info);
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
