extern crate advent_2017;

use std::collections::HashMap;
use advent_2017::*;

fn main() {
    let input = parse_input(&get_input(6));

    let (part1, part2) = simulate(input);
    println!("Part 1: {}", part1);
    println!("Part 2: {}", part2);

}

fn parse_input(str: &str) -> Vec<u64> {
    str.split_whitespace().flat_map(str::parse).collect()
}

fn redistribute(memory: &mut [u64]) {
    let max_elt = *memory.iter().max().unwrap();
    let max_ix = memory.iter().position(|x| *x == max_elt).unwrap();

    let avail = memory[max_ix];
    memory[max_ix] = 0;

    for i in (0 .. memory.len()).cycle().skip(max_ix+1).take(avail as usize) {
        memory[i] += 1;
    }
}

fn simulate(mut memory: Vec<u64>) -> (u64, u64) {
    let mut seen = HashMap::new();
    let mut counter = 0;

    while !seen.contains_key(&memory) {
        seen.insert(memory.clone(), counter);
        counter += 1;
        redistribute(&mut memory)
    }

    (counter, counter - seen[&memory])
}