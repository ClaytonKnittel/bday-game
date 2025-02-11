#![deny(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

mod crossword;
mod interactive_grid;
mod pc;

use std::{
  collections::HashSet,
  fs::{self, File},
  io::Write,
  process::ExitCode,
};

use clap::{Parser, ValueEnum};
use crossword::Crossword;
use interactive_grid::InteractiveGrid;
use pc::Pc;
use serde::Serialize;
use termgame::{color::AnsiValue, event_loop::EventLoop};
use util::{bitcode, error::TermgameResult, grid::Grid, pos::Pos};
use xword_dict::XWordDict;
use xword_gen::xword::{XWord, XWordTile};

const GRID_PATH: &str = "./grid.bin";

#[derive(ValueEnum, Clone, Debug, Serialize)]
#[serde(rename_all = "kebab-case")]
enum RunMode {
  InteractiveGrid,
  Progress,
  Play,
}

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
  #[arg(long, default_value = "progress")]
  mode: RunMode,
}

fn read_grid(path: &str) -> TermgameResult<InteractiveGrid> {
  Ok(InteractiveGrid::from_grid(bitcode::decode(&fs::read(
    path,
  )?)?))
}

fn read_dict() -> TermgameResult<XWordDict> {
  const DICT_PATH: &str = "./xword_gen/dict.bin";
  Ok(bitcode::decode(&fs::read(DICT_PATH)?)?)
}

fn build_dict() -> TermgameResult<HashSet<String>> {
  Ok(
    read_dict()?
      .top_n_words(150000)
      .into_iter()
      .map(|str| str.to_owned())
      .collect(),
  )
}

const fn saturday() -> &'static str {
  "________XX_____
   _________X_____
   _________X_____
   _____XX____X___
   ____X_____X____
   ___X____X______
   X______________
   XXX____X____XXX
   ______________X
   ______X____X___
   ____X_____X____
   ___X____XX_____
   _____X_________
   _____X_________
   _____XX________"
}

const fn sunday() -> &'static str {
  "________X_______X______
   ________X_______X______
   ________X_______X______
   ___X______X____X___X___
   ____XX_____X______X____
   ______X_____XX____X____
   XXX____X____X____X_____
   ___X_____X______X______
   ________X_____X_____XXX
   _____X_______XXX_______
   _____XX_____X______X___
   ____X_____________X____
   ___X______X_____XX_____
   _______XXX_______X_____
   XXX_____X_____X________
   ______X______X_____X___
   _____X____X____X____XXX
   ____X____XX_____X______
   ____X______X_____XX____
   ___X___X____X______X___
   ______X_______X________
   ______X_______X________
   ______X_______X________"
}

const fn partial_sunday() -> &'static str {
  "________Xc______X______
   ________Xl______X______
   ________Xa______X______
   ___X_____yX____X___X___
   ____XX___t_XXXX___X____
   ______X__o__XX____X____
   XXX____X_n__X____X_____
   ___X_____X______X______
   ________X_____X_____XXX
   _____X_______XXX_______
   _____XX_____X______X___
   ____X______X______X____
   ___X______X_____XX_____
   _______XXX_______X_____
   XXX____X______X________
   ______X______X_____X___
   _____X____X____X____XXX
   ____X___XXX_____X______
   ____XsmellyX_____XX____
   ___X___X____XX_____X___
   ______X_______X________
   ______X_______X________
   ______X_______X________"
}

fn mega_grid() -> TermgameResult<Grid<bool>> {
  Ok(bitcode::decode(&fs::read("./grid.bin")?)?)
}

fn interactive_grid() -> TermgameResult {
  let mut ev = EventLoop::new()?;
  let grid = read_grid(GRID_PATH).or_else(|_| -> TermgameResult<_> {
    Ok(InteractiveGrid::from_grid(Grid::from_vec(
      vec![XWordTile::Empty; 50 * 51],
      50,
      51,
    )?))
  })?;
  let grid_uid = ev.scene().add_entity(Box::new(grid));

  let pc_uid = ev
    .scene()
    .add_entity(Box::new(Pc::new(Pos::zero(), AnsiValue::rgb(5, 0, 5))));
  ev.run_event_loop(|scene, window, _| {
    let width = window.width() as i32;
    let height = window.height() as i32;

    let pc: &Pc = scene.entity(pc_uid)?;
    let grid: &InteractiveGrid = scene.entity(grid_uid)?;
    let camera_pos = window.camera_pos_mut();

    camera_pos.x = (pc.pos().x - width / 2)
      .max(0)
      .min((grid.screen_width()).saturating_sub(width as u32) as i32);
    camera_pos.y = (pc.pos().y - height / 2)
      .max(0)
      .min((grid.screen_height()).saturating_sub(height as u32) as i32);

    Ok(())
  })?;

  if false {
    let grid: &InteractiveGrid = ev.scene().entity(grid_uid)?;
    let grid_serialized = bitcode::encode(grid.grid());
    let mut file = File::create(GRID_PATH)?;
    file.write_all(&grid_serialized)?;
  }

  Ok(())
}

fn show_dlx_iters() -> TermgameResult {
  let mut ev = EventLoop::new()?;
  // let grid = bitcode::decode(&fs::read("xword_gen/crossword.bin")?)?;
  let grid = XWord::build_grid(partial_sunday())?;
  // let grid = mega_grid()?;
  let xword_uid = ev
    .scene()
    .add_entity(Box::new(Crossword::from_grid(grid.clone())));

  // const REQUIRED: [&str; 0] = [];
  #[rustfmt::skip]
  const REQUIRED: [&str; 24] = [
    "clayton", "eugenia", "andrew", "jackson","matt", "bchan", "austen", "paul",
    "kevin", "kmoney", "paige", "kyle", "nina", "anne", "ethan", "jonathan",
    "rose", "alex", "cindy", "cooper", "jessica", "kathy", "laney", "sruthi",
    // "christina",
  ];

  let xword_solver = XWord::from_grid_with_required(
    grid.clone(),
    REQUIRED.map(|str| str.to_owned()),
    build_dict()?,
  )?;
  let mut x_iter = xword_solver.stepwise_board_iter();

  let mut done = false;
  let pc_uid = ev
    .scene()
    .add_entity(Box::new(Pc::new(Pos::zero(), AnsiValue::rgb(5, 0, 5))));
  ev.run_event_loop(|scene, window, _| {
    let width = window.width() as i32;
    let height = window.height() as i32;

    if !done {
      if let Some(grid) = x_iter.next() {
        scene.entity_mut::<Crossword>(xword_uid)?.swap_grid(grid);
      } else {
        done = true;
      }
    }

    let pc: &Pc = scene.entity(pc_uid)?;
    let xword: &Crossword = scene.entity(xword_uid)?;

    let camera_pos = window.camera_pos_mut();

    camera_pos.x = (pc.pos().x - width / 2)
      .max(0)
      .min((xword.screen_width()).saturating_sub(width as u32) as i32);
    camera_pos.y = (pc.pos().y - height / 2)
      .max(0)
      .min((xword.screen_height()).saturating_sub(height as u32) as i32);

    Ok(())
  })?;

  Ok(())
}

fn play_puzzle() -> TermgameResult {
  let mut ev = EventLoop::new()?;
  let grid = bitcode::decode(&fs::read("xword_gen/crossword.bin")?)?;
  let xword_uid = ev.scene().add_entity(Box::new(Crossword::from_grid(grid)));

  let pc_uid = ev
    .scene()
    .add_entity(Box::new(Pc::new(Pos::zero(), AnsiValue::rgb(5, 0, 5))));
  ev.run_event_loop(|scene, window, _| {
    let width = window.width() as i32;
    let height = window.height() as i32;

    let pc: &Pc = scene.entity(pc_uid)?;
    let xword: &Crossword = scene.entity(xword_uid)?;

    let camera_pos = window.camera_pos_mut();

    camera_pos.x = (pc.pos().x - width / 2)
      .max(0)
      .min((xword.screen_width()).saturating_sub(width as u32) as i32);
    camera_pos.y = (pc.pos().y - height / 2)
      .max(0)
      .min((xword.screen_height()).saturating_sub(height as u32) as i32);

    Ok(())
  })?;

  Ok(())
}

fn run() -> TermgameResult {
  let args = Args::parse();
  match args.mode {
    RunMode::InteractiveGrid => interactive_grid(),
    RunMode::Progress => show_dlx_iters(),
    RunMode::Play => play_puzzle(),
  }
}

fn main() -> ExitCode {
  if let Err(err) = run() {
    println!("Error: {err}");
    ExitCode::FAILURE
  } else {
    ExitCode::SUCCESS
  }
}
