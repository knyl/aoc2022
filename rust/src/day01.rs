use itertools::Itertools;

fn get_calorie_sums(input: &str, n: usize) -> u32 {
    input
        .split("\n\n")
        .map(|group| group.lines().map(|n| n.parse::<u32>().unwrap()))
        .map(|n| n.sum::<u32>())
        .sorted()
        .rev()
        .take(n)
        .sum()
}

pub fn day01() {
    let input = include_str!("../input/day01.txt");
    println!("Part 1: {}", get_calorie_sums(input, 1));
    println!("Part 2: {}", get_calorie_sums(input, 3));

}

