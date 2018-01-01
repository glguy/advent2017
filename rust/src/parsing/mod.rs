use nom::digit;
use std::str;

named!(pub uint64<u64>, map_res!(map_res!(digit, str::from_utf8), str::parse));