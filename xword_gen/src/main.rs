#![deny(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::{
  fs::{self, File},
  io::{BufRead, BufReader, Write},
};

use util::{bitcode, error::TermgameResult, grid::Grid, time::time_fn};
use xword_dict::XWordDict;
use xword_gen::xword::{XWord, XWordTile, XWordTraits};

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

const fn partial_sunday() -> &'static str {
  "________Xc______X______
   ________Xl______X______
   ________Xa______X______
   ___X_____yX____X___X___
   ____XX___t_X______X____
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
   ___X___X____X______X___
   ______X_______X________
   ______X_______X________
   ______X_______X________"
}

fn mega() -> TermgameResult<Grid<XWordTile>> {
  Ok(bitcode::decode(&fs::read("../grid.bin")?)?)
}

fn find_and_save_solution(grid: Grid<XWordTile>) -> TermgameResult {
  let dict = read_dict()?;
  let words: Vec<_> = dict.top_n_words(180_000);

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

  let xword = XWord::from_grid(
    grid,
    // REQUIRED.into_iter().map(|str| str.to_owned()),
    words.into_iter().map(|str| str.to_owned()),
  )?;

  let guard = pprof::ProfilerGuardBuilder::default()
    .frequency(1000)
    .blocklist(&["libc", "libgcc", "pthread", "vdso"])
    .build()?;

  let (time, solution) = time_fn(|| xword.solve());

  if let Ok(report) = guard.report().build() {
    let file = std::fs::File::create("xword_gen.svg")?;
    report.flamegraph(file)?;
  };

  let solution = solution?;
  println!("Took {}s", time.as_secs_f32());

  if let Some(solution) = solution {
    let result = bitcode::encode(&solution);
    let mut file = File::create("./crossword.bin")?;
    file.write_all(&result)?;
  } else {
    println!("No solution!");
  }

  Ok(())
}

fn main() -> TermgameResult {
  if false {
    // find_and_save_solution(mega()?)
    find_and_save_solution(XWord::build_grid(partial_sunday())?)
  } else {
    build_and_save_dict_from_xd("./clues.txt")
  }
}
