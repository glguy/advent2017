use std::io::prelude::*;
use std::fs::File;

pub fn get_input(day: u32) -> String {
    let path = format!("../inputs/input{:02}.txt",day);
    let mut file = File::open(path).unwrap();
    let mut result = String::new();
    file.read_to_string(&mut result).unwrap();
    result
}
