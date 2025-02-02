use std::{io::StdoutLock, sync::Mutex, time::SystemTime};

use termion::{
  async_stdin,
  cursor::HideCursor,
  event::{Event, Key, MouseEvent},
  input::{MouseTerminal, TermRead},
  raw::{IntoRawMode, RawTerminal},
};

use crate::{entity::Entity, error::TermgameResult, pos::Pos, scene::Scene, window::Window};

type Term<'a> = HideCursor<MouseTerminal<RawTerminal<StdoutLock<'a>>>>;

pub struct EventLoopOptions {
  // TODO:
  pub fps: f32,
}

pub struct EventLoop<'a> {
  window: Window<Term<'a>>,
}

impl<'a> EventLoop<'a> {
  pub fn new() -> Self {
    let stdout = HideCursor::from(MouseTerminal::from(
      std::io::stdout().lock().into_raw_mode().unwrap(),
    ));
    let window = Window::new(stdout, 120, 40);
    Self { window }
  }

  pub fn run_event_loop<F>(&mut self, mut callback: F) -> TermgameResult
  where
    F: FnMut(&mut Scene) -> bool,
  {
    let mut stdin = async_stdin().events();
    let done = Mutex::new(false);

    let mut scene = Scene::new();

    'outer: for t in 0usize.. {
      let start = SystemTime::now();
      if *done.lock().unwrap() {
        break;
      }
      for evt in stdin.by_ref() {
        match evt {
          Ok(Event::Key(Key::Char('q'))) => break 'outer,
          Ok(Event::Mouse(me)) => match me {
            MouseEvent::Press(_, x, y) => {
              scene.click(Pos {
                x: x as u32 - 1,
                y: y as u32 - 1,
              });
            }
            MouseEvent::Hold(x, y) => {
              scene.drag(Pos {
                x: x as u32 - 1,
                y: y as u32 - 1,
              });
            }
            MouseEvent::Release(x, y) => {
              scene.release(Pos {
                x: x as u32 - 1,
                y: y as u32 - 1,
              });
            }
          },
          Err(_) => break 'outer,
          _ => {}
        }
      }
      self.window.reset();
      scene.tick(t);
      scene.render(&mut self.window);
      self.window.render()?;
      let end = SystemTime::now();

      let sleep_duration =
        std::time::Duration::from_millis(20).saturating_sub(end.duration_since(start).unwrap());
      std::thread::sleep(sleep_duration);
    }

    self.window.cleanup()?;
    Ok(())
  }
}
