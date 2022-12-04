use im::HashSet;
use itertools::Itertools;

fn pt1(input: &str) -> u32 {
    input.lines()
        .map(|line| line.split_at(line.len() / 2))
        .map(|(l1, l2)| (create_hash_set(l1), create_hash_set(l2)))
        .map(|(set1, set2)|
            set1.intersection(set2).iter().next().unwrap().clone())
        .map(score)
        .sum()
}

fn pt2(input: &str) -> u32 {
    input
        .lines()
        .map(create_hash_set)
        .chunks(3)
        .into_iter()
        .map(|chunk| {
            chunk
                .reduce(|set1, set2| set1.intersection(set2))
                .unwrap()
                .iter().next().unwrap().clone()
        })
        .map(score)
        .sum()
}

fn create_hash_set(l1: &str) -> HashSet<char> {
    l1.chars().collect::<HashSet<char>>()
}

fn score(char: char) -> u32 {
    if char.is_ascii_lowercase() {
        char as u32 - 'a' as u32 + 1
    } else {
        char as u32 - 'A' as u32 + 27
    }
}

pub fn day03() {
    let input = include_str!("../input/day03.txt");
    println!("Part 1: {}", pt1(input));
    println!("Part 2: {}", pt2(input));
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

    #[test]
    fn test_input_pt2() {
        assert_eq!(pt2(INPUT.trim()), 70)
    }
}