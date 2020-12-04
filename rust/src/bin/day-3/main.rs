use aoc;
use std::{
  cmp::PartialEq,
  io::{Error, Result},
  str::FromStr,
};

#[derive(Debug, PartialEq)]
enum Cell {
  Tree,
  Snow,
}

struct Row {
  cells: Vec<Cell>,
}

impl Row {
  fn get(&self, index: usize) -> Option<&Cell> {
    self.cells.get(index % self.cells.len())
  }

  fn is_tree(&self, index: usize) -> bool {
    self.get(index) == Some(&Cell::Tree)
  }
}

impl FromStr for Row {
  type Err = Error;

  fn from_str(s: &str) -> Result<Self> {
    Ok(Row {
      cells: s
        .chars()
        .map(|c| if c == '.' { Cell::Snow } else { Cell::Tree })
        .collect(),
    })
  }
}

fn trees_for_slope(input: &Vec<Row>, right: usize, down: usize) -> i64 {
  let mut next_row_index = down;
  let mut cell_index = right;
  input.iter().enumerate().fold(0, |mut trees, (index, row)| {
    if index == next_row_index {
      if row.is_tree(cell_index) {
        trees += 1;
      }
      next_row_index += down;
      cell_index += right;
    }

    trees
  })
}

fn main() -> Result<()> {
  let input = aoc::read_input_for_day::<Row>("day-3")?;
  let part = aoc::read_part_for_day();

  let answer = if part == 1 {
    trees_for_slope(&input, 3, 1)
  } else {
    vec![
      trees_for_slope(&input, 1, 1),
      trees_for_slope(&input, 3, 1),
      trees_for_slope(&input, 5, 1),
      trees_for_slope(&input, 7, 1),
      trees_for_slope(&input, 1, 2),
    ]
    .iter()
    .fold(1, |acc, trees| acc * trees)
  };

  println!("The answer for part-{} is: {}", part, answer);
  Ok(())
}
