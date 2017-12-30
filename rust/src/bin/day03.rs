extern crate advent_2017;

use advent_2017::*;
use std::iter;
use std::collections::hash_map;

fn main() {
    let input: usize = parse_input(&get_input(3));
    println!("Part 1: {}", part1(input));
    println!("Part 2: {}", part2(input));

}

fn part1(input: usize) -> i32 {
    manhattan(coords().skip(input-1).next().unwrap())
}

fn part2(input: usize) -> u32 {
    let mut vals: hash_map::HashMap<(i32,i32),u32> = hash_map::HashMap::new();
    vals.insert((0,0), 1);
    for c in coords().skip(1) {
        let z = neighborhood(c).iter().flat_map(|x|vals.get(x)).sum();
        if z > input as u32 {
            return z
        }
        vals.insert(c,z);
    }
    panic!("failure in part 2")
}

fn neighborhood((x,y): (i32,i32)) -> [(i32,i32); 8] {
    [(x-1,y+1),(x,y+1),(x+1,y+1),
     (x-1,y  ),        (x+1,y  ),
     (x-1,y-1),(x,y-1),(x+1,y-1)]
}

fn manhattan((x,y): (i32,i32)) -> i32 {
     x.abs() + y.abs()
}

fn coords() -> Box<Iterator<Item=(i32,i32)>> {
    static DIRECTIONS: [(i32, i32); 4] = [ (1, 0), (0, 1), (-1, 0), (0, -1)];

    let x =
        (1 ..)
            .flat_map(|x| iter::repeat(x).take(2))
            .zip(DIRECTIONS.iter().cycle())
            .flat_map(|(n,dir)| iter::repeat(dir).take(n))
            .scan((0,0), | st, &(dx,dy)| {
                let val = st.clone();
                st.0 += dx;
                st.1 += dy;
                Some(val)
            })
    ;

    Box::new(x)
}

fn parse_input(str: &str) -> usize {
    str.split_whitespace().next().unwrap().parse().unwrap()
}