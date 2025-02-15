use std::fs;

use common::msg::ServerMessage;
use termgame::event_loop::EventLoop;
use util::{bitcode, error::TermgameResult};

use crate::{client::Client, crossword::CrosswordEntity};

pub async fn play_puzzle() -> TermgameResult {
  let mut client = Client::new().await?;

  let mut ev = EventLoop::new()?;
  let grid = bitcode::decode(&fs::read("xword_gen/crossword.bin")?)?;
  let xword_uid = ev
    .scene()
    .add_entity(Box::new(CrosswordEntity::from_grid(grid)));

  let mut ev_iter = ev.async_event_loop();
  for t in 0usize.. {
    if !ev_iter.poll(&mut ev).await? {
      break;
    }

    let scene = ev.scene();
    let xword: &mut CrosswordEntity = scene.entity_mut(xword_uid)?;

    for message in client.pending_server_messages() {
      match message? {
        ServerMessage::NewConnection { uid } => {}
        ServerMessage::ConnectToExisting { success } => {}
        ServerMessage::Ping => {}
      }
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
