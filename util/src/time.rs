use std::time::{Duration, SystemTime};

pub fn time_fn<F: FnOnce() -> T, T>(f: F) -> (Duration, T) {
  let start = SystemTime::now();
  let result = f();
  let end = SystemTime::now();
  (end.duration_since(start).unwrap(), result)
}
