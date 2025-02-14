use std::{
  error::Error,
  io::StdoutLock,
  thread,
  time::{Duration, SystemTime},
};

use termion::{
  async_stdin,
  cursor::HideCursor,
  event::{Event, Key, MouseEvent},
  input::{Events, MouseTerminal, TermRead},
  raw::{IntoRawMode, RawTerminal},
  screen::{AlternateScreen, IntoAlternateScreen},
  AsyncReader,
};
use tokio::time;
use util::{
  error::{TermgameError, TermgameResult},
  pos::Diff,
};

use crate::{entity::Entity, scene::Scene, window::Window};

type Term<'a> = HideCursor<MouseTerminal<AlternateScreen<RawTerminal<StdoutLock<'a>>>>>;

pub struct EventLoopOptions {
  // TODO:
  pub fps: f32,
}

pub struct EventLoop<'a> {
  window: Window<Term<'a>>,
  scene: Scene,
  done: bool,
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
    Ok(Self { window, scene: Scene::new(), done: false })
  }

  pub fn window(&mut self) -> &mut Window<Term<'a>> {
    &mut self.window
  }

  pub fn scene(&mut self) -> &mut Scene {
    &mut self.scene
  }

  fn handle_events(
    &mut self,
    events: impl Iterator<Item = Result<Event, impl Error>>,
  ) -> TermgameResult {
    let camera_pos = self.window.camera_pos();
    for evt in events {
      match evt.map_err(|err| TermgameError::Internal(format!("Read event failed: {err}")))? {
        Event::Key(Key::Esc) => {
          self.done = true;
        }
        Event::Key(key) => self.scene.keypress(key)?,
        Event::Mouse(me) => match me {
          MouseEvent::Press(_, x, y) => {
            self
              .scene
              .click(camera_pos + Diff { x: x as i32 - 1, y: y as i32 - 1 })?;
          }
          MouseEvent::Hold(x, y) => {
            self
              .scene
              .drag(camera_pos + Diff { x: x as i32 - 1, y: y as i32 - 1 })?;
          }
          MouseEvent::Release(x, y) => {
            self
              .scene
              .release(camera_pos + Diff { x: x as i32 - 1, y: y as i32 - 1 })?;
          }
        },
        _ => {}
      }
    }

    Ok(())
  }

  fn render(&mut self) -> TermgameResult {
    self.window.reset();
    self.scene.render(&mut self.window);
    self.window.render()?;
    Ok(())
  }

  pub fn run_event_loop<F>(&mut self, mut callback: F) -> TermgameResult
  where
    F: FnMut(&mut Scene, &mut Window<Term<'a>>, usize) -> TermgameResult,
  {
    let mut stdin = async_stdin().events();

    for t in 0usize.. {
      let start = SystemTime::now();

      self.handle_events(stdin.by_ref())?;
      if self.done {
        return Ok(());
      }

      self.scene.tick(t)?;
      callback(&mut self.scene, &mut self.window, t)?;
      self.render()?;

      let end = SystemTime::now();

      let sleep_duration = Duration::from_millis(20).saturating_sub(end.duration_since(start)?);
      thread::sleep(sleep_duration);
    }

    unreachable!();
  }

  pub fn async_event_loop(&mut self) -> AsyncEventLoopIter {
    AsyncEventLoopIter {
      stdin_events: async_stdin().events(),
      t: 0,
      start: SystemTime::now(),
    }
  }
}

pub struct AsyncEventLoopIter {
  stdin_events: Events<AsyncReader>,
  t: usize,
  start: SystemTime,
}

impl AsyncEventLoopIter {
  pub async fn poll(&mut self, event_loop: &mut EventLoop<'_>) -> TermgameResult<bool> {
    event_loop.render()?;

    let end = SystemTime::now();
    let sleep_duration = Duration::from_millis(20).saturating_sub(end.duration_since(self.start)?);
    time::sleep(sleep_duration).await;

    self.start = SystemTime::now();
    event_loop.handle_events(self.stdin_events.by_ref())?;
    if event_loop.done {
      return Ok(false);
    }

    event_loop.scene.tick(self.t)?;

    Ok(true)
  }
}
