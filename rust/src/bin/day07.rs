extern crate advent_2017;
#[macro_use]
extern crate nom;

use advent_2017::get_input;
use advent_2017::parsing::uint64;
use nom::{alpha,newline};
use std::str;
use std::collections::{HashSet,HashMap};

fn main() {
    let input = get_input(7);
    let entries = parse_input(input.as_bytes()).unwrap().1;
    let top = part1(&entries);
    println!("Part 1: {}", top);
    println!("Part 2: {}", part2(&entries, top));
}

fn part1<'a>(entries: &[Entry<'a>]) -> &'a str {

    let children: HashSet<&str> =
        entries.iter().flat_map(|x| &x.children).map(|x| *x).collect();

    entries.iter().map(|x| x.name).find(|x| !children.contains(x)).unwrap()
}


fn part2(entries: &[Entry], top: &str) -> u64 {

    let entry_map =
        entries.iter().map(|e| (e.name, e)).collect::<HashMap<_,_>>();

    let mut weight_map = HashMap::<&str,u64>::new();

    let mut i = 0;
    let mut dfs = vec![top];
    while i < dfs.len() {
        for x in &entry_map[dfs[i]].children {
            dfs.push(x)
        }
        i += 1;
    }

    for &x in dfs.iter().rev() {
        let me: &Entry = entry_map[x];
        let weight = me.weight + me.children.iter().map(|&x| weight_map[x]).sum::<u64>();
        weight_map.insert(x,weight);
    }

    println!("{:?}", dfs);

    0
}

#[derive(Debug, Clone)]
struct Entry<'a> {
    name: &'a str,
    weight: u64,
    children: Vec<&'a str>
}

named!(node_name<&str>, map_res!(alpha, str::from_utf8));

named!(parse_input<Vec<Entry>>,
  many0!(
    do_parse!(
      name: node_name >>
      weight: delimited!(tag_s!(" ("), uint64, tag_s!(")")) >>
      opt!(tag_s!(" -> ")) >>
      children: separated_list!(tag_s!(", "), node_name) >>
      newline >>
      (Entry {name, weight, children})
    )
  )
);