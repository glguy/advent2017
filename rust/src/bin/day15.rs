extern crate advent_2017;
#[macro_use]
extern crate nom;

use advent_2017::get_input;
use advent_2017::parsing::uint64;

use nom::{alpha, newline};

fn main() {
    let (start_a, start_b) = parse_input(get_input(15).as_bytes()).unwrap().1;

    let gen_a = Gen { seed: start_a, factor: 16807 };
    let gen_b = Gen { seed: start_b, factor: 48271 };

    let part1 = solve(40_000_000, gen_a, gen_b);
    println!("Part 1: {}", part1);

    let part2 = solve(5_000_000,
                      gen_a.filter(|x| x % 4 == 0),
                      gen_b.filter(|x| x % 8 == 0));
    println!("Part 2: {}", part2);
}

/** Run two iterators side-by-side for n iterations and count how many
    pairs produce a match of the lower 16 bits.
    */
fn solve<I1,I2>(n: usize, it1: I1, it2: I2) -> usize
  where I1: Iterator<Item=u64>,
        I2: Iterator<Item=u64>
{
    it1.zip(it2)
        .take(n)
        .filter(|x| x.0 & 0xffff == x.1 & 0xffff)
        .count()
}


#[derive(Clone, Copy, Debug)]
struct Gen {
    seed: u64,
    factor: u64,
}

impl Iterator for Gen {
    type Item = u64;
    fn next(&mut self) -> Option<Self::Item> {
        self.seed *= self.factor;
        self.seed %= 0x7fffffff;
        Some(self.seed)
    }
}

named!(parse_line<u64>,
    do_parse!(
        tag_s!("Generator ") >> alpha >> tag_s!(" starts with ") >>
        start: uint64 >> newline >>
        (start)
    )
);

named!(parse_input<(u64,u64)>,
    do_parse!(
        start_a: parse_line >>
        start_b: parse_line >>
    (start_a, start_b)
    )
);