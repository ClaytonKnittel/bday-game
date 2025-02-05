#![deny(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use util::error::TermgameResult;
use xword::XWord;

mod dlx;
mod xword;

fn main() -> TermgameResult {
  let xword = XWord::from_layout(
    "X___X
     _____
     _____
     _____
     _____",
    [
      "hug", "korea", "isbns", "snark", "sines", "kiss", "hosni", "urban", "genre", "asks",
    ]
    .into_iter()
    .map(|str| str.to_owned())
    .collect(),
  )?;

  let solution = xword.solve()?.map(|&tile| tile.unwrap_or('_'));

  println!("Solution:\n{solution}");

  Ok(())
}
