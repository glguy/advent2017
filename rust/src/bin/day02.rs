extern crate advent_2017;
#[macro_use]
extern crate indoc;

use advent_2017::*;

fn main() {
    let input = get_input(2);
    let ss = parse_input(&input);

    println!("Part 1: {}", part1(&ss));
    println!("Part 2: {}", part2(&ss));
}

fn parse_input(str: &str) -> Vec<Vec<u32>> {
    str.lines().map(|line| line.split_whitespace().map(|x| x.parse().unwrap()).collect()).collect()
}

fn part1(ss: &Vec<Vec<u32>>) -> u32 {
    ss.iter().map(|x|part1_row(&*x)).sum()
}

fn part1_row(row: &[u32]) -> u32 {
    row.iter().max().unwrap() - row.iter().min().unwrap()
}

fn part2(ss: &Vec<Vec<u32>>) -> u32 {
    ss.iter().map(|x| part2_row(&*x)).sum()
}

fn part2_row(row: &[u32]) -> u32 {
    for x in 0 .. row.len() {
        for y in 0 .. row.len() {
            if x != y {
                let v1 = row[x];
                let v2 = row[y];
                if v1 % v2 == 0 {
                    return v1 / v2
                }
            }
        }
    }
    panic!("Bad row")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn examples1() {
        let input = indoc!(
            "5 1 9 5
             7 5 3
             2 4 6 8");
        let ss = parse_input(input);

        assert_eq!(8, part1_row(&ss[0]));
        assert_eq!(4, part1_row(&ss[1]));
        assert_eq!(6, part1_row(&ss[2]));

        assert_eq!(8+4+6, part1(&ss));
    }
    #[test]
    fn examples2() {
        let input = indoc!(
            "5 9 2 8
             9 4 7 3
             3 8 6 5");
        let ss = parse_input(input);

        assert_eq!(4, part2_row(&ss[0]));
        assert_eq!(3, part2_row(&ss[1]));
        assert_eq!(2, part2_row(&ss[2]));

        assert_eq!(4+3+2, part2(&ss));
    }
}