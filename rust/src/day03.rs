use std::collections::hash_map::RandomState;
use std::collections::HashSet;
//use itertools::Itertools;

fn pt1(input: &str) -> u32 {
    input.lines()
        .map(handle_line)
        .sum()
}

fn handle_line(line: &str) -> u32 {
    let (l1, l2) = line.split_at(line.len() / 2);
    let set1: HashSet<char, RandomState> = HashSet::from_iter(l1.chars());
    let set2: HashSet<char, RandomState> = HashSet::from_iter(l2.chars());
    let char = set1.intersection(&set2).next().unwrap();
    score(char)
}

fn score(char: &char) -> u32 {
    if char.is_ascii_lowercase() {
        *char as u32 - 'a' as u32 + 1
    } else {
        *char as u32 - 'A' as u32 + 27
    }
}

pub fn day03() {
    let input = include_str!("../input/day03.txt");
    println!("Part 1: {}", pt1(input));
}


#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = "
vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw";

    #[test]
    fn test_input_pt1() {
        assert_eq!(pt1(INPUT.trim()), 157)
    }
}