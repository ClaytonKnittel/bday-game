fn main() {
  let stdout = HideCursor::from(MouseTerminal::from(
    std::io::stdout().lock().into_raw_mode().unwrap(),
  ));
  let mut window = window::Window::new(stdout, 120, 40);
  let mut stdin = async_stdin().events();
  let done = Mutex::new(false);

  let mut r = rngs::StdRng::seed_from_u64(27418995609531717u64);

  let bunny = Bunny::new(window.width(), window.height(), &mut r, &done);

  let mut scene = Scene::new();
  scene.add_entity(Box::new(bunny));

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
            scene.click(x as u32 - 1, y as u32 - 1);
          }
          MouseEvent::Hold(x, y) => {
            scene.drag(x as u32 - 1, y as u32 - 1);
          }
          MouseEvent::Release(x, y) => {
            scene.release(x as u32 - 1, y as u32 - 1);
          }
        },
        Err(_) => break 'outer,
        _ => {}
      }
    }
    window.reset();
    scene.tick(t);
    scene.render(&mut window);
    window.render().expect("Failed 2 render");
    let end = SystemTime::now();

    let sleep_duration =
      std::time::Duration::from_millis(20).saturating_sub(end.duration_since(start).unwrap());
    std::thread::sleep(sleep_duration);
  }

  window.cleanup().expect("Failed to cleanup");
}
