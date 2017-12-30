extern crate advent_2017;

use advent_2017::*;
use std::collections::HashSet;

fn main() {
    let input = parse_input(&get_input(4));
    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input));
}

fn parse_input(str: &str) -> Vec<Vec<String>> {
    str.lines().map(|line| line.split_whitespace().map(str::to_owned).collect()).collect()
}

fn part1(passphrases: &[Vec<String>]) -> usize {
    passphrases.iter().filter(|x| is_valid_1(&*x)).count()
}

fn part2(passphrases: &[Vec<String>]) -> usize {
    passphrases.iter().filter(|x| is_valid_2(&*x)).count()
}

fn is_valid_1(line: &[String]) -> bool {
    line.iter().cloned().collect::<HashSet<String>>().len() == line.len()
}

fn is_valid_2(line: &[String]) -> bool {
    line.iter().map(|x| {
        let mut v: Vec<char> = x.chars().collect();
        v.sort_unstable();
        v
    }
    ).collect::<HashSet<Vec<char>>>().len() == line.len()
}