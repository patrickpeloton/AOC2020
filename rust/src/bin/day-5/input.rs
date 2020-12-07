use std::str::FromStr;

const NUMBER_OF_ROWS: i32 = 128;
const NUMBER_OF_SEATS: i32 = 8;

#[derive(Debug)]
pub enum AdventError {
  UnrecognizedChar,
  InvalidTree,
}

#[derive(Debug)]
enum RowLetter {
  F,
  B,
}

#[derive(Debug)]

enum SeatLetter {
  L,
  R,
}

impl RowLetter {
  fn from_char(c: char) -> Result<Self, AdventError> {
    match c {
      'F' => Ok(RowLetter::F),
      'B' => Ok(RowLetter::B),
      _ => Err(AdventError::UnrecognizedChar),
    }
  }
}

impl SeatLetter {
  fn from_char(c: char) -> Result<Self, AdventError> {
    match c {
      'L' => Ok(SeatLetter::L),
      'R' => Ok(SeatLetter::R),
      _ => Err(AdventError::UnrecognizedChar),
    }
  }
}

#[derive(Debug)]
pub struct BoardingPass {
  row_code: Vec<RowLetter>,
  seat_code: Vec<SeatLetter>,
}

impl BoardingPass {
  pub fn new(code: &str) -> Result<Self, AdventError> {
    BoardingPass::from_str(code)
  }

  pub fn row(&self) -> Result<i32, AdventError> {
    let mut tree = Tree::new(NUMBER_OF_ROWS);
    for letter in self.row_code.iter() {
      match letter {
        RowLetter::F => tree.lower(),
        RowLetter::B => tree.upper(),
      };
    }

    tree.value()
  }

  pub fn seat(&self) -> Result<i32, AdventError> {
    let mut tree = Tree::new(NUMBER_OF_SEATS);
    for letter in self.seat_code.iter() {
      match letter {
        SeatLetter::L => tree.lower(),
        SeatLetter::R => tree.upper(),
      };
    }

    tree.value()
  }

  pub fn id(&self) -> Result<i32, AdventError> {
    let row = self.row()?;
    let seat = self.seat()?;
    Ok(row * 8 + seat)
  }
}

impl FromStr for BoardingPass {
  type Err = AdventError;

  fn from_str(line: &str) -> Result<Self, Self::Err> {
    let mut row_code = vec![];
    let mut seat_code = vec![];
    for (index, char) in line.chars().enumerate() {
      if index < 7 {
        row_code.push(RowLetter::from_char(char)?);
      } else {
        seat_code.push(SeatLetter::from_char(char)?);
      }
    }

    Ok(BoardingPass {
      row_code,
      seat_code,
    })
  }
}

// Tree Stuff

#[derive(Debug)]
struct Tree {
  start: i32,
  size: i32,
}

impl Tree {
  fn new(size: i32) -> Self {
    Tree { start: 0, size }
  }

  fn done(&self) -> bool {
    self.size == 1
  }

  fn value(&self) -> Result<i32, AdventError> {
    if self.done() {
      Ok(self.start)
    } else {
      Err(AdventError::InvalidTree)
    }
  }

  fn lower(&mut self) -> &Self {
    self.size = self.size / 2;
    self
  }

  fn upper(&mut self) -> &Self {
    self.size = self.size / 2;
    self.start += self.size;
    self
  }
}
