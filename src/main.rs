#![deny(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

mod crossword;
mod interactive_grid;
mod pc;

use std::{
  collections::{hash_map::Entry, HashMap, HashSet},
  fs::{self, File},
  io::{BufRead, BufReader},
  process::ExitCode,
};

use crossword::Crossword;
use interactive_grid::InteractiveGrid;
use itertools::Itertools;
use pc::Pc;
use termgame::{color::AnsiValue, event_loop::EventLoop};
use util::{
  bitcode,
  error::{TermgameError, TermgameResult},
  grid::Grid,
  pos::Pos,
};
use xword_gen::{
  dlx::DlxIteratorWithNames,
  xword::{XWord, XWordTile},
};

const GRID_PATH: &str = "./grid.bin";

fn read_grid(path: &str) -> TermgameResult<InteractiveGrid> {
  Ok(InteractiveGrid::from_grid(bitcode::decode(&fs::read(
    path,
  )?)?))
}

fn read_dict(path: &str) -> TermgameResult<HashMap<String, u32>> {
  let mut result = HashMap::new();
  let reader = BufReader::new(File::open(path)?);
  for line in reader.lines() {
    let line = line?;
    let items: Vec<_> = line.split('\t').collect();
    if items.len() != 4 {
      continue;
    }

    let answer = items[2];
    // let clue = items[3];

    let answer_len = answer.chars().count();
    if answer_len <= 2
      || !answer.chars().all(|c| c.is_alphabetic())
      || answer.chars().all(|c| c.to_ascii_lowercase() == 'x')
      || (answer.chars().all_equal() && answer_len > 3)
    {
      continue;
    }
    match result.entry(answer.to_ascii_lowercase()) {
      Entry::Occupied(mut entry) => *entry.get_mut() += 1,
      Entry::Vacant(entry) => {
        entry.insert(1);
      }
    }
  }

  Ok(result)
}

fn build_dict() -> TermgameResult<HashSet<String>> {
  let dict = read_dict("xword_gen/clues.txt")?;

  let total: u64 = dict.iter().map(|(_, &freq)| freq as u64).sum();
  println!("Total: {total}, size {}", dict.len());

  Ok(
    dict
      .iter()
      .map(|(str, &freq)| (str.to_owned(), freq))
      .sorted_by_key(|&(_, freq)| !freq)
      .take(120000)
      .map(|(str, _)| str)
      .collect(),
  )
}

fn mega_grid() -> TermgameResult<Grid<bool>> {
  Ok(bitcode::decode(&fs::read("./grid.bin")?)?)
}

fn run() -> TermgameResult {
  let mut ev = EventLoop::new()?;
  // let grid = bitcode::decode(&fs::read("xword_gen/crossword.bin")?)?;
  let grid = mega_grid()?.map(|&is_empty| {
    if is_empty {
      XWordTile::Empty
    } else {
      XWordTile::Wall
    }
  });
  let xword_uid = ev.scene().add_entity(Box::new(Crossword::from_grid(grid)));

  let xword_solver = XWord::from_grid(mega_grid()?, build_dict()?)?;
  let mut dlx = xword_solver.build_dlx();
  let mut x_iter = dlx
    .find_solutions_stepwise()
    .with_names()
    .map(|partial_solution| xword_solver.build_grid_from_assignments(partial_solution));

  // let grid = read_grid(GRID_PATH).unwrap_or_else(|_| InteractiveGrid::new(50, 50));
  // let grid_uid = ev.scene().add_entity(Box::new(grid));

  let pc_uid = ev
    .scene()
    .add_entity(Box::new(Pc::new(Pos::zero(), AnsiValue::rgb(5, 0, 5))));
  ev.run_event_loop(|scene, window, t| {
    let width = window.width() as i32;
    let height = window.height() as i32;

    if t % 2 == 0 {
      if let Some(grid) = x_iter.next() {
        scene.entity_mut::<Crossword>(xword_uid)?.swap_grid(grid?);
      }
    }

    let pc: &Pc = scene.entity(pc_uid)?;
    let xword: &Crossword = scene.entity(xword_uid)?;

    // let grid: &InteractiveGrid = scene.entity(grid_uid)?;
    let camera_pos = window.camera_pos_mut();

    camera_pos.x = (pc.pos().x - width / 2)
      .max(0)
      .min((xword.screen_width()).saturating_sub(width as u32) as i32);
    camera_pos.y = (pc.pos().y - height / 2)
      .max(0)
      .min((xword.screen_height()).saturating_sub(height as u32) as i32);

    Ok(())
  })?;

  // if false {
  //   let grid: &InteractiveGrid = ev.scene().entity(grid_uid)?;
  //   let grid_serialized = bitcode::encode(grid.grid());
  //   let mut file = File::create(GRID_PATH)?;
  //   file.write_all(&grid_serialized)?;
  // }

  Ok(())
}

fn main() -> ExitCode {
  if let Err(err) = run() {
    eprintln!("Error: {err}");
    ExitCode::FAILURE
  } else {
    ExitCode::SUCCESS
  }
}
