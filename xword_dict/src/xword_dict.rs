use std::{borrow::Borrow, collections::HashMap};

use bitcode::{Decode, Encode};
use itertools::Itertools;
use once_cell::sync::Lazy;
use regex::Regex;
use util::error::{TermgameError, TermgameResult};

#[derive(Encode, Decode, PartialEq, Eq, Hash)]
struct DictEntry {
  publisher: String,
  publish_year: u16,
  clue: String,
}

impl DictEntry {
  fn canonicalize_word(word: &str) -> String {
    word.to_ascii_lowercase()
  }

  fn parse_from_xd_txt(line: &str) -> TermgameResult<Option<(String, Self)>> {
    let items: Vec<_> = line.split('\t').collect();
    if items.len() < 4 {
      return Err(TermgameError::Parse(format!("Failed to parse \"{line}\" as DictEntry")).into());
    }

    let word = items[2];
    let word_len = word.chars().count();
    if word_len <= 2
      || !word.chars().all(|c| c.is_ascii_alphabetic())
      || word.chars().all(|c| c.eq_ignore_ascii_case(&'x'))
      || (word.chars().all_equal() && word_len > 3)
    {
      return Ok(None);
    }

    static SELF_REF_CLUE_RE: Lazy<Regex> =
      Lazy::new(|| Regex::new(r"\d+[ -]?([aA]cross|[dD]own)").unwrap());
    let clue = items[3..].join(" ").to_owned();
    if SELF_REF_CLUE_RE.is_match(&clue) {
      return Ok(None);
    }

    let publish_year: u16 = items[1].parse()?;
    if publish_year < 1999 {
      return Ok(None);
    }

    Ok(Some((
      Self::canonicalize_word(word),
      Self {
        publisher: items[0].to_owned(),
        publish_year,
        clue,
      },
    )))
  }
}

#[derive(Encode, Decode)]
pub struct XWordDict {
  dict: HashMap<String, HashMap<DictEntry, u32>>,
}

impl XWordDict {
  pub fn parse_xd_file<S>(clues: impl IntoIterator<Item = S>) -> TermgameResult<Self>
  where
    S: Borrow<String>,
  {
    Ok(Self {
      dict: clues.into_iter().try_fold(
        HashMap::<_, HashMap<_, _>>::new(),
        |mut dict, clue_str| -> TermgameResult<_> {
          if let Some((word, entry)) =
            DictEntry::parse_from_xd_txt(clue_str.borrow()).map_err(|err| {
              TermgameError::Parse(format!("Failed to parse {}: {err}", clue_str.borrow()))
            })?
          {
            *dict.entry(word).or_default().entry(entry).or_default() += 1;
          }
          Ok(dict)
        },
      )?,
    })
  }

  pub fn add_clue(&mut self, word: String, clue_txt: String) {
    *self
      .dict
      .entry(word)
      .or_default()
      .entry(DictEntry {
        publisher: "n/a".to_owned(),
        publish_year: 2025,
        clue: clue_txt,
      })
      // Make the word occur many times so it will alaways be selected in top_n_words
      .or_default() += 100_000;
  }

  pub fn top_n_words(&self, n: usize) -> Vec<&str> {
    self
      .dict
      .iter()
      .map(|(str, freq_map)| (str.as_str(), freq_map.values().sum::<u32>()))
      .sorted_by_key(|&(_, freq)| !freq)
      .map(|(str, _)| str)
      .take(n)
      .collect()
  }
}
