extern crate advent_2017;

use advent_2017::*;

fn main() {
    let input = parse_input(&get_input(5));

    println!("Part 1: {}", part1(&input));
    println!("Part 2: {}", part2(&input));

}

fn parse_input(str: &str) -> Vec<isize>{
    str.split_whitespace().flat_map(str::parse).collect()
}

fn part1(vals: &Vec<isize>) -> u32 {
    execute(vals, |x| x + 1)
}

fn part2(vals: &Vec<isize>) -> u32 {
    execute(vals, | x| x + if x < 3 { 1 } else { -1 })
}

fn execute<F: Fn(isize) -> isize>(vals: &[isize], update: F) -> u32 {
    let mut counter = 0;
    let mut cursor = 0;
    let mut vals = vals.to_owned();

    while 0 <= cursor && (cursor as usize) < vals.len() {
        let val = vals[cursor as usize];
        vals[cursor as usize] = update(val);
        counter += 1;
        cursor += val;
    }

    counter
}