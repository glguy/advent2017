extern crate advent_2017;

use advent_2017::*;

fn main() {
    let input = get_input(1);
    let digits = parse_input(&input);

    println!("Part 1: {}", part1(&digits));
    println!("Part 2: {}", part2(&digits));
}

fn part1(digits: &[u32]) -> u32 {
    captcha(digits, 1)
}

fn part2(digits: &[u32]) -> u32 {
    captcha(digits, digits.len() / 2)
}

fn parse_input(str: &str) -> Vec<u32> {
    str.chars().flat_map(|x| x.to_digit(10)).collect()
}

fn captcha(digits: &[u32], offset: usize) -> u32 {
    let lookahead = digits.iter().cycle().skip(offset);
    digits
        .iter()
        .zip(lookahead)
        .filter(|&(x,y)| x==y)
        .map(|(x,_)| x)
        .sum()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn examples1() {
        assert_eq!(3, part1(&parse_input("1122")));
        assert_eq!(4, part1(&parse_input("1111")));
        assert_eq!(0, part1(&parse_input("1234")));
        assert_eq!(9, part1(&parse_input("91212129")));
    }
    #[test]
    fn examples2() {
        assert_eq!( 6, part2(&parse_input("1212")));
        assert_eq!( 0, part2(&parse_input("1221")));
        assert_eq!( 4, part2(&parse_input("123425")));
        assert_eq!(12, part2(&parse_input("123123")));
        assert_eq!( 4, part2(&parse_input("12131415")));
    }
}