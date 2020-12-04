use regex::{Captures, Regex};
use std::ops::Range;
use std::str::FromStr;

#[derive(Debug)]
pub enum AdventError {
  InvalidRegex,
}

#[derive(Debug)]
pub struct Part1Line {
  pub count_range: Range<i32>,
  pub character: char,
  pub password: String,
}

impl FromStr for Part1Line {
  type Err = AdventError;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    lazy_static! {
      static ref RE: Regex = Regex::new(r"^(\d+)\-(\d+) ([a-z]): ([a-z]+)$").unwrap();
    }
    let captures = RE.captures(s).ok_or(AdventError::InvalidRegex)?;

    let count_start = parse_from_captures(&captures, 1)?;
    let count_end: i32 = parse_from_captures(&captures, 2)?;
    Ok(Part1Line {
      count_range: count_start..(count_end + 1), // add one to count_end to make `.contains` work
      character: parse_from_captures(&captures, 3)?,
      password: parse_from_captures(&captures, 4)?,
    })
  }
}

fn parse_from_captures<T>(captures: &Captures, index: usize) -> Result<T, AdventError>
where
  T: FromStr,
{
  captures
    .get(index)
    .ok_or(AdventError::InvalidRegex)?
    .as_str()
    .parse::<T>()
    .map_err(|_| AdventError::InvalidRegex)
}

#[derive(Debug)]
pub struct Part2Line {
  pub index_1: usize,
  pub index_2: usize,
  pub character: char,
  pub password: String,
}

impl FromStr for Part2Line {
  type Err = AdventError;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    lazy_static! {
      static ref RE: Regex = Regex::new(r"^(\d+)\-(\d+) ([a-z]): ([a-z]+)$").unwrap();
    }
    let captures = RE.captures(s).ok_or(AdventError::InvalidRegex)?;

    let position_1: usize = parse_from_captures(&captures, 1)?;
    let position_2: usize = parse_from_captures(&captures, 2)?;
    Ok(Part2Line {
      index_1: position_1 - 1,
      index_2: position_2 - 1,
      character: parse_from_captures(&captures, 3)?,
      password: parse_from_captures(&captures, 4)?,
    })
  }
}
