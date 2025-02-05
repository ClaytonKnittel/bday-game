#![deny(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::{
  collections::{hash_map::Entry, HashMap},
  fs::File,
  io::{BufRead, BufReader},
};

use itertools::Itertools;
use util::error::TermgameResult;
use xword::XWord;

mod dlx;
mod xword;

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

    if answer.is_empty()
      || !answer.chars().all(|c| c.is_alphabetic())
      || answer.chars().all(|c| c.to_ascii_lowercase() == 'x')
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

fn main() -> TermgameResult {
  let dict = read_dict("clues.txt")?;

  let total: u64 = dict.iter().map(|(_, &freq)| freq as u64).sum();
  println!("Total: {total}, size {}", dict.len());

  let words: Vec<_> = dict
    .iter()
    .filter(|(str, _)| str.len() <= 5)
    .map(|(str, &freq)| (str.to_owned(), freq))
    .sorted_by_key(|&(_, freq)| !freq)
    .take(1268)
    .collect();
  for (word, freq) in words.iter().take(5) {
    println!("{word} occurs {freq} times");
  }

  let xword = XWord::from_layout(
    "X___X
     _____
     _____
     _____
     _____",
    words.iter().map(|(str, _)| (*str).clone()).collect(),
  )?;

  let solution = xword.solve()?.map(|&tile| tile.unwrap_or('_'));

  println!("Solution:\n{solution}");

  Ok(())
}
