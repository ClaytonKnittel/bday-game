#![deny(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::{
  fs::{self, File},
  io::{BufRead, BufReader, Write},
};

use dlx::DlxIteratorWithNames;
use util::{bitcode, error::TermgameResult, grid::Grid, time::time_fn};
use xword_dict::XWordDict;
use xword_gen::xword::XWord;

const DICT_PATH: &str = "./dict.bin";

fn build_and_save_dict_from_xd(xd_path: &str) -> TermgameResult {
  let dict = XWordDict::parse_xd_file(
    BufReader::new(File::open(xd_path)?)
      .lines()
      .skip(1)
      .collect::<Result<Vec<_>, _>>()?,
  )?;

  let result = bitcode::encode(&dict);
  let mut file = File::create(DICT_PATH)?;
  file.write_all(&result)?;

  Ok(())
}

fn read_dict() -> TermgameResult<XWordDict> {
  Ok(bitcode::decode(&fs::read(DICT_PATH)?)?)
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

fn mega() -> TermgameResult<Grid<bool>> {
  Ok(bitcode::decode(&fs::read("../grid.bin")?)?)
}

fn find_and_save_solution(grid: Grid<bool>) -> TermgameResult {
  let dict = read_dict()?;
  let words: Vec<_> = dict.top_n_words(1_000_000);

  #[rustfmt::skip]
  const REQUIRED: [&str; 1] = [
    "clayton",
  ];
  // #[rustfmt::skip]
  // const REQUIRED: [&str; 25] = [
  //   "clayton", "eugenia", "andrew", "jackson", "matt", "bchan", "austen", "paul", "kevin",
  //   "kmoney", "paige", "kyle", "nina", "anne", "ethan", "jonathan", "rose", "alex", "cindy",
  //   "cooper", "jessica", "kathy", "laney", "sruthi", "christina",
  // ];

  let xword = XWord::from_grid_with_required(
    grid,
    REQUIRED.into_iter().map(|str| str.to_owned()),
    words.into_iter().map(|str| str.to_owned()),
  )?;

  let (time, solution) = time_fn(|| xword.solve());
  let solution = solution?;
  println!("Took {}s", time.as_secs_f32());

  {
    let result = bitcode::encode(&solution);
    let mut file = File::create("./crossword.bin")?;
    file.write_all(&result)?;
  }

  Ok(())
}

fn show_steps() -> TermgameResult {
  let dict = read_dict()?;

  let words = dict.top_n_words(120000);

  // const REQUIRED: [&str; 2] = ["clayton", "eugenia"];
  // #[rustfmt::skip]
  // const REQUIRED: [&str; 25] = [
  //   "clayton", "eugenia", "andrew", "jackson", "matt", "bchan", "austen", "paul", "kevin",
  //   "kmoney", "paige", "kyle", "nina", "anne", "ethan", "jonathan", "rose", "alex", "cindy",
  //   "cooper", "jessica", "kathy", "laney", "sruthi", "christina",
  // ];

  let xword = XWord::from_grid(
    // mega()?,
    XWord::build_grid(saturday())?,
    // REQUIRED.map(|s| s.to_owned()),
    words.iter().map(|&str| str.to_owned()),
  )?;

  let mut dlx = xword.build_dlx();
  for farthest_vec in dlx
    .find_solutions_stepwise()
    .with_names()
    .scan(Vec::new(), |longest_vec, solution| {
      let solution_vec = solution.take_result();
      Some(if solution_vec.len() > longest_vec.len() {
        *longest_vec = solution_vec.clone();
        Some(longest_vec.clone())
      } else {
        None
      })
    })
    .flatten()
  // .step_by(5)
  {
    println!("{}", xword.build_grid_from_assignments(farthest_vec)?);
  }

  Ok(())
}

fn main() -> TermgameResult {
  if true {
    find_and_save_solution(mega()?)
  } else if false {
    build_and_save_dict_from_xd("./clues.txt")
  } else {
    show_steps()
  }
}
