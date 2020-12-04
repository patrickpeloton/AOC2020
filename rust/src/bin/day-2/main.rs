#[macro_use]
extern crate lazy_static;
mod input;

use aoc;
use input::{Part1Line, Part2Line};
use std::io;

fn part_1() -> io::Result<usize> {
    let input = aoc::read_input_for_day::<Part1Line>("day-2")?;

    let count = input
        .iter()
        .filter(|i| {
            i.count_range
                .contains(&(i.password.chars().filter(|c| c == &(i.character)).count() as i32))
        })
        .count();

    println!("Answer: {}", count);
    Ok(count)
}

fn part_2() -> io::Result<usize> {
    let input = aoc::read_input_for_day::<Part2Line>("day-2")?;

    let count = input
        .iter()
        .filter(|i| {
            (i.password.chars().nth(i.index_1) == Some(i.character))
                ^ (i.password.chars().nth(i.index_2) == Some(i.character))
        })
        .count();

    Ok(count)
}

fn main() -> io::Result<()> {
    let part = aoc::read_part_for_day();
    let count = if part == 1 { part_1() } else { part_2() };
    println!("Part {} answer: {}", part, count.unwrap());
    Ok(())
}
