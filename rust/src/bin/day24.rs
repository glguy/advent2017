extern crate advent_2017;

use advent_2017::*;

type Pins = u32;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct Segment { pins1: Pins, pins2: Pins }

impl Segment {
    fn new(pins1: Pins, pins2: Pins) -> Self {
        Segment { pins1, pins2 }
    }

    fn pair(&self, goal: Pins) -> Option<Pins> {
        if self.pins1 == goal {
            Some(self.pins2)
        } else if self.pins2 == goal {
            Some(self.pins1)
        } else {
            None
        }
    }

    fn weight(&self) -> Pins {
        self.pins1 + self.pins2
    }
}

fn main() {
    let segments = parse_input(&get_input(24));
    let (p1,p2) = part_1_2(segments);
    println!("Part 1: {}", p1);
    println!("Part 2: {}", p2);
}

fn parse_input(str: &str) -> Vec<Segment> {
    str.lines().map(|x| {
        let ns: Vec<u32> = x.split('/').flat_map(str::parse).collect();
        assert_eq!(2, ns.len());
        Segment::new(ns[0], ns[1])
    }).collect()
}

fn swap_insert<A>(vector: &mut Vec<A>, index: usize, value: A) {
    vector.push(value);
    let n = vector.len();
    vector.swap(index, n - 1)
}

fn longest_bridge(segments: &mut Vec<Segment>, start: Pins, weight: Pins, depth: usize, weights: &mut [Pins]) {
    let depth = depth + 1;
    for i in 0 .. segments.len() {
        if let Some(next) = segments[i].pair(start) {

            let segment = segments.swap_remove(i);

            let weight = weight + segment.weight();
            if weights[depth] < weight { weights[depth] = weight }

            longest_bridge(segments, next,  weight, depth, weights);

            swap_insert(segments, i, segment); // revert swap_remove above
        }
    }
}

fn part_1_2(mut segments: Vec<Segment>) -> (Pins,Pins) {
    let mut weights = vec![0; segments.len() + 1];
    longest_bridge(&mut segments, 0, 0, 0, &mut weights);
    let part_1 = *weights.iter().max().unwrap();
    let part_2 = *weights.iter().filter(|x| x > &&0).last().unwrap_or(&0);
    (part_1, part_2)
}