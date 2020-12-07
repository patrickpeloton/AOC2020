mod input;

use aoc::{read_input_for_day, read_part_for_day};
use input::BoardingPass;

fn part_1(boarding_passes: Vec<BoardingPass>) -> std::io::Result<()> {
  let mut highest_id = 0;
  for pass in boarding_passes.iter() {
    let id = pass.id().unwrap();
    if id > highest_id {
      highest_id = id;
    }
  }

  println!("the highest id is: {}", highest_id);
  Ok(())
}

fn part_2(boarding_passes: Vec<BoardingPass>) -> std::io::Result<()> {
  let mut ids: Vec<i32> = vec![];

  // whatever, let's just double loop it
  for pass in boarding_passes.iter() {
    ids.push(pass.id().unwrap());
  }
  ids.sort();

  let mut expected = ids.first().unwrap().clone();
  let mut missing: Option<&i32> = None;
  for id in ids.iter() {
    if id != &expected {
      missing = Some(&expected);
      break;
    } else {
      expected += 1;
    }
  }

  println!("Seat id: {}", missing.unwrap());
  Ok(())
}

fn main() -> std::io::Result<()> {
  let boarding_passes = read_input_for_day::<BoardingPass>("day-5")?;
  if read_part_for_day() == 1 {
    part_1(boarding_passes)
  } else {
    part_2(boarding_passes)
  }
}
