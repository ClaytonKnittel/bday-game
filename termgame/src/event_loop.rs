use std::{
  io::StdoutLock,
  sync::Mutex,
  thread,
  time::{Duration, SystemTime},
};

use termion::{
  async_stdin,
  cursor::HideCursor,
  event::{Event, Key, MouseEvent},
  input::{MouseTerminal, TermRead},
  raw::{IntoRawMode, RawTerminal},
  screen::{AlternateScreen, IntoAlternateScreen},
};

use crate::{
  entity::Entity,
  error::{TermgameError, TermgameResult},
  pos::Pos,
  scene::Scene,
  window::Window,
};

type Term<'a> = HideCursor<MouseTerminal<AlternateScreen<RawTerminal<StdoutLock<'a>>>>>;

pub struct EventLoopOptions {
  // TODO:
  pub fps: f32,
}

pub struct EventLoop<'a> {
  window: Window<Term<'a>>,
  scene: Scene,
  done: Mutex<bool>,
}

impl<'a> EventLoop<'a> {
  pub fn new() -> TermgameResult<Self> {
    let stdout = HideCursor::from(MouseTerminal::from(
      std::io::stdout()
        .lock()
        .into_raw_mode()?
        .into_alternate_screen()?,
    ));

    let (width, height) = termion::terminal_size()?;
    let window = Window::new(stdout, width as u32, height as u32);
    Ok(Self {
      window,
      scene: Scene::new(),
      done: Mutex::new(false),
    })
  }

  pub fn scene(&mut self) -> &mut Scene {
    &mut self.scene
  }

  pub fn run_event_loop<F>(&mut self, mut callback: F) -> TermgameResult
  where
    F: FnMut(&mut Scene, &mut Window<Term<'a>>, usize) -> TermgameResult,
  {
    let mut stdin = async_stdin().events();

    for t in 0usize.. {
      let start = SystemTime::now();
      if *self
        .done
        .lock()
        .map_err(|_| TermgameError::Internal("Failed to acquire mutex on `done`".to_owned()))?
      {
        return Ok(());
      }
      for evt in stdin.by_ref() {
        match evt? {
          Event::Key(Key::Esc) => return Ok(()),
          Event::Key(key) => self.scene.keypress(key)?,
          Event::Mouse(me) => match me {
            MouseEvent::Press(_, x, y) => {
              self.scene.click(Pos {
                x: x as i32 - 1,
                y: y as i32 - 1,
              })?;
            }
            MouseEvent::Hold(x, y) => {
              self.scene.drag(Pos {
                x: x as i32 - 1,
                y: y as i32 - 1,
              })?;
            }
            MouseEvent::Release(x, y) => {
              self.scene.release(Pos {
                x: x as i32 - 1,
                y: y as i32 - 1,
              })?;
            }
          },
          data => return Err(TermgameError::Internal(format!("got unknown key {data:?}")).into()),
        }
      }
      self.window.reset();
      self.scene.tick(t)?;
      callback(&mut self.scene, &mut self.window, t)?;
      self.scene.render(&mut self.window);
      self.window.render()?;
      let end = SystemTime::now();

      let sleep_duration = Duration::from_millis(20).saturating_sub(end.duration_since(start)?);
      thread::sleep(sleep_duration);
    }

    unreachable!();
  }
}
