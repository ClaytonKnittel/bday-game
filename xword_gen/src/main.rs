#![deny(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::{
  collections::{hash_map::Entry, HashMap},
  fs::{self, File},
  io::{BufRead, BufReader, Write},
  iter::once,
};

use dlx::DlxIteratorWithNames;
use itertools::Itertools;
use util::{
  bitcode,
  error::{TermgameError, TermgameResult},
  grid::Grid,
};
use xword_gen::xword::XWord;

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

fn main() -> TermgameResult {
  let dict = read_dict("clues.txt")?;

  let total: u64 = dict.iter().map(|(_, &freq)| freq as u64).sum();
  println!("Total: {total}, size {}", dict.len());

  let words: Vec<_> = dict
    .iter()
    .map(|(str, &freq)| (str.to_owned(), freq))
    .sorted_by_key(|&(_, freq)| !freq)
    .take(120000)
    // .chain(
    //   [
    //     "ingoodconscience",
    //     "icecreamheadache",
    //     "areyouamanoramouse",
    //     "jerusalemartichoke",
    //     "comingoutparty",
    //     "iminbigtrouble",
    //     "waterbuffalo",
    //     "socialsecurity",
    //     "opentoquestion",
    //     "beammeupscotty",
    //     "iwillalwaysloveyou",
    //     "kissingandmakingup",
    //     "aturnfortheworse",
    //     "detectivestories",
    //     "beverlyhills",
    //     "imonyourside",
    //     "onceinawhile",
    //     "statetrooper",
    //     "cruxoftheissue",
    //     "genetherapy",
    //     "thesmithsonian",
    //     "comedownthepike",
    //   ]
    //   .into_iter()
    //   .map(|s| (s.to_owned(), 1)),
    // )
    // .chain((3..=15).map(|len| (once('a').cycle().take(len).collect(), 1)))
    .collect();
  for (word, freq) in words.iter().take(5) {
    println!("{word} occurs {freq} times");
  }

  // let mut hist = HashMap::<usize, u32>::new();
  // for (word, _) in words.iter() {
  //   match hist.entry(word.chars().count()) {
  //     Entry::Occupied(mut entry) => *entry.get_mut() += 1,
  //     Entry::Vacant(entry) => {
  //       entry.insert(1);
  //     }
  //   }
  // }
  // for size in 0..1000 {
  //   if let Some(cnt) = hist.get(&size) {
  //     println!("{size}: {cnt}");
  //   }
  // }

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
    words.iter().map(|(str, _)| (*str).clone()),
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

  // {
  //   let result = bitcode::encode(&solution);
  //   let mut file = File::create("./crossword.bin")?;
  //   file.write_all(&result)?;
  // }

  Ok(())
}
