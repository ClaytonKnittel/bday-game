#![deny(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

mod args;
mod client;
mod crossword;
mod interactive_grid;
mod pc;
mod q_prompt;
mod run_game;
mod screen_manager;
mod textbox;

use std::{
  collections::{HashMap, HashSet},
  fs::{self, File},
  io::Write,
  process::ExitCode,
};

use args::{Args, RunMode};
use clap::Parser;
use common::crossword::{Crossword, XWordTile};
use crossword::CrosswordEntity;
use interactive_grid::{InteractiveGrid, InteractiveGridMode};
use pc::Pc;
use run_game::play_puzzle;
use termgame::{color::AnsiValue, event_loop::EventLoop};
use util::{bitcode, error::TermgameResult, grid::Grid, pos::Pos};
use xword_dict::XWordDict;
use xword_gen::xword::{XWord, XWordTraits, XWordWithRequired};

const GRID_PATH: &str = "./grid.bin";

fn read_dict() -> TermgameResult<XWordDict> {
  const DICT_PATH: &str = "./xword_gen/dict.bin";
  Ok(bitcode::decode(&fs::read(DICT_PATH)?)?)
}

fn build_dict() -> TermgameResult<HashSet<String>> {
  Ok(
    read_dict()?
      .top_n_words(150_000)
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

fn mega_grid() -> TermgameResult<Grid<XWordTile>> {
  Ok(bitcode::decode(&fs::read(GRID_PATH)?)?)
}

fn interactive_grid(mode: InteractiveGridMode) -> TermgameResult {
  let mut ev = EventLoop::new()?;
  let grid = match mega_grid() {
    Ok(grid) => InteractiveGrid::from_grid(grid, mode),
    Err(_) => InteractiveGrid::new(mode, 50, 51)?,
  };
  let grid_uid = ev.scene().add_entity(Box::new(grid));

  ev.run_event_loop(|scene, window, _| {
    let width = window.width() as i32;
    let height = window.height() as i32;

    let grid: &InteractiveGrid = scene.entity(grid_uid)?;
    let cursor_pos = grid.cursor_screen_pos();
    let camera_pos = window.camera_pos_mut();

    camera_pos.x = (cursor_pos.x - width / 2)
      .max(0)
      .min((grid.screen_width()).saturating_sub(width as u32) as i32);
    camera_pos.y = (cursor_pos.y - height / 2)
      .max(0)
      .min((grid.screen_height()).saturating_sub(height as u32) as i32);

    Ok(())
  })?;

  if true {
    let grid: &InteractiveGrid = ev.scene().entity(grid_uid)?;
    let grid_serialized = bitcode::encode(grid.grid());
    let mut file = File::create(GRID_PATH)?;
    file.write_all(&grid_serialized)?;
  }

  Ok(())
}

fn show_dlx_iters() -> TermgameResult {
  // let grid = bitcode::decode(&fs::read("xword_gen/crossword.bin")?)?;
  // let grid = XWord::build_grid(sunday())?;
  let grid = mega_grid()?;

  // const REQUIRED: [&str; 0] = [];
  // #[rustfmt::skip]
  // const REQUIRED: [&str; 25] = [
  //   "clayton", "eugenia", "andrew", "jackson", "matt", "bchan", "austen", "paul",
  //   "kevin", "kmoney", "paige", "kyle", "nina", "anne", "ethan", "jonathan",
  //   "rose", "alex", "cindy", "cooper", "jessica", "kathy", "laney", "sruthi",
  //   "christina",
  // ];
  #[rustfmt::skip]
  const REQUIRED: [&str; 5] = [
    "pizza", "ono", "jazz", "lamp", "windmill",
  ];

  let mut dict = build_dict()?;
  for &required_word in REQUIRED.iter() {
    dict.insert(required_word.to_owned());
  }

  let xword_solver =
    XWordWithRequired::from_grid(grid.clone(), REQUIRED.map(|str| str.to_owned()), dict)?;
  let mut x_iter = xword_solver.stepwise_board_iter();

  let mut ev = EventLoop::new()?;
  let xword_uid = ev.scene().add_entity(Box::new(CrosswordEntity::from_grid(
    grid.clone(),
    0,
    HashMap::new(),
  )));

  let mut done = false;
  let pc_uid = ev
    .scene()
    .add_entity(Box::new(Pc::new(Pos::zero(), AnsiValue::rgb(5, 0, 5))));
  ev.run_event_loop(|scene, window, _| {
    let width = window.width() as i32;
    let height = window.height() as i32;

    if !done {
      if let Some(grid) = x_iter.next() {
        scene
          .entity_mut::<CrosswordEntity>(xword_uid)?
          .swap_for(Crossword::from_grid(grid, HashMap::new()));
      } else {
        done = true;
      }
    }

    let pc: &Pc = scene.entity(pc_uid)?;
    let xword: &CrosswordEntity = scene.entity(xword_uid)?;

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

async fn run() -> TermgameResult {
  let args = Args::parse();
  match args.mode {
    RunMode::InteractiveGrid => interactive_grid(InteractiveGridMode::WordLengthSat),
    RunMode::Progress => show_dlx_iters(),
    RunMode::Play => play_puzzle(&args.host, args.admin).await,
  }
}

#[tokio::main(flavor = "current_thread")]
async fn main() -> ExitCode {
  if let Err(err) = run().await {
    println!("Error: {err}");
    ExitCode::FAILURE
  } else {
    ExitCode::SUCCESS
  }
}
