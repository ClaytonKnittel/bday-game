use std::collections::HashMap;

use common::{
  crossword::Crossword,
  msg::{ClientMessage, ServerMessage},
  player_info::PlayerInfo,
};
use termgame::event_loop::EventLoop;
use tokio::{
  fs::{self, File},
  io::AsyncWriteExt,
};
use util::{
  bitcode,
  error::{TermgameError, TermgameResult},
  pos::Pos,
};

use crate::{client::Client, crossword::CrosswordEntity, textbox::TextBox};

const SYNC_PERIOD: usize = 5 * 50;

const UID_FILE: &str = "./uid.bin";

async fn read_uid() -> TermgameResult<Option<u64>> {
  if !fs::try_exists(UID_FILE).await? {
    return Ok(None);
  }

  let uid_file = fs::read(UID_FILE).await;
  let uid_contents = match uid_file {
    Ok(uid) => uid,
    Err(err) => {
      println!("err: {err:?}");
      return Err(err.into());
    }
  };

  Ok(Some(bitcode::decode(&uid_contents)?))
}

async fn write_uid(uid: u64) -> TermgameResult {
  let result = bitcode::encode(&uid);
  let mut file = File::create(UID_FILE).await?;
  file.write_all(&result).await?;
  Ok(())
}

async fn load_player_info(client: &mut Client) -> TermgameResult<u64> {
  if let Some(uid) = read_uid().await? {
    client
      .send_message(ClientMessage::ConnectToExisting { uid })
      .await?;

    loop {
      if let Some(message) = client.recv_server_message().await {
        match message {
          ServerMessage::ConnectToExisting { success: true } => return Ok(uid),
          ServerMessage::ConnectToExisting { success: false } => break,
          _ => continue,
        }
      } else {
        return Err(TermgameError::Internal("Connection closed".to_owned()).into());
      }
    }
  }

  loop {
    client.send_message(ClientMessage::NewConnection).await?;

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

async fn play(client: &mut Client, uid: u64) -> TermgameResult {
  client.send_message(ClientMessage::FullRefresh).await?;
  let (crossword, player_info) = wait_for_refresh(client).await?;

  let mut ev = EventLoop::new()?;
  let xword_uid = ev
    .scene()
    .add_entity(Box::new(CrosswordEntity::with_crossword_and_player_info(
      crossword,
      player_info,
      uid,
    )));

  let textbox_uid = ev.scene().add_entity(Box::new(TextBox::new(
    Pos { x: 10, y: 4 },
    "testing 1 2 3".to_owned(),
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

pub async fn play_puzzle() -> TermgameResult {
  let mut client = Client::new().await?;
  let uid = load_player_info(&mut client).await?;

  let result = play(&mut client, uid).await;

  let uid_write_result = write_uid(uid).await;
  result.and(uid_write_result)
}
