use std::collections::{HashMap, HashSet};

#[derive(Clone, Debug)]
pub struct WordBank {
  word_set: HashSet<String>,
  bank: HashMap<u32, String>,
}

impl WordBank {
  pub fn from_words(words: impl IntoIterator<Item = String>) -> Self {
    let word_set: HashSet<_> = words.into_iter().collect();
    let bank = word_set
      .iter()
      .enumerate()
      .map(|(idx, word)| (idx as u32, word.clone()))
      .collect();
    Self { word_set, bank }
  }

  pub fn has(&self, word: &str) -> bool {
    self.word_set.contains(word)
  }

  pub fn get(&self, id: u32) -> Option<&str> {
    self.bank.get(&id).map(|word| word.as_str())
  }

  pub fn all_words(&self) -> impl Iterator<Item = &str> {
    self.bank.values().map(|word| word.as_str())
  }

  pub fn all_words_with_id(&self) -> impl Iterator<Item = (u32, &str)> {
    self.bank.iter().map(|(&id, word)| (id, word.as_str()))
  }
}

type LetterFrequencyMapEntry<'a> = (HashMap<(char, u32), u32>, HashSet<&'a str>);
pub struct LetterFrequencyMap<'a> {
  /// Map from word_length -> ((letter, index) -> count, (set of words))
  frequencies: HashMap<u32, LetterFrequencyMapEntry<'a>>,
}

impl<'a> LetterFrequencyMap<'a> {
  fn new() -> Self {
    Self { frequencies: HashMap::new() }
  }

  // TODO make private, build in WordBank
  pub fn from_words(words: impl IntoIterator<Item = &'a str>) -> Self {
    let mut map = Self::new();
    for word in words.into_iter() {
      map.insert(word);
    }
    map
  }

  fn insert(&mut self, word: &'a str) {
    let len = word.chars().count() as u32;
    let (char_map, words) = self.frequencies.entry(len).or_default();
    for (idx, letter) in word.chars().enumerate() {
      *char_map.entry((letter, idx as u32)).or_default() += 1;
    }
    words.insert(word);
  }

  pub fn words_with_length(&self, word_length: u32) -> impl Iterator<Item = &'a str> + '_ {
    self
      .frequencies
      .get(&word_length)
      .map(|(_, words)| words.iter().cloned())
      .into_iter()
      .flatten()
  }

  pub fn likelihood(&self, word_length: u32, char_pos: (char, u32)) -> f32 {
    self
      .frequencies
      .get(&word_length)
      .map(|(char_map, words)| {
        char_map.get(&char_pos).cloned().unwrap_or(0) as f32 / words.len() as f32
      })
      .unwrap_or(0f32)
  }
}
