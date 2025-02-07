#![deny(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::{
  collections::{hash_map::Entry, HashMap},
  fs::File,
  io::{BufRead, BufReader, Write},
};

use itertools::Itertools;
use util::{bitcode, error::TermgameResult};
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
      || answer.chars().all_equal() && answer_len > 3
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

fn main() -> TermgameResult {
  let dict = read_dict("clues.txt")?;

  let total: u64 = dict.iter().map(|(_, &freq)| freq as u64).sum();
  println!("Total: {total}, size {}", dict.len());

  let words: Vec<_> = dict
    .iter()
    .map(|(str, &freq)| (str.to_owned(), freq))
    .sorted_by_key(|&(_, freq)| !freq)
    .take(180000)
    .collect();
  for (word, freq) in words.iter().take(5) {
    println!("{word} occurs {freq} times");
  }

  let xword = XWord::from_layout_with_required(
    sunday(),
    [
      "clayton",
      "eugenia",
      "andrew",
      "jackson",
      "matt",
      "bchan",
      "austen",
      "paul",
      "kevin",
      "kmoney",
      "paige",
      "kyle",
      "nina",
      "anne",
      "ethan",
      "jonathan",
      "rose",
      "alex",
      "cindy",
      "cooper",
      "jessica",
      "kathy",
      "laney",
      "sruthi",
      "christina",
    ]
    .map(|s| s.to_owned())
    .into_iter()
    .collect(),
    words.iter().map(|(str, _)| (*str).clone()).collect(),
  )?;

  let solution = xword.solve()?;

  println!("Solution:\n{}", solution.map(|&tile| tile.unwrap_or('_')));

  {
    let result = bitcode::encode(&solution);
    let mut file = File::create("./crossword.bin")?;
    file.write_all(&result)?;
  }

  Ok(())
}
