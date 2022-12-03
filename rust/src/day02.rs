enum Play { ROCK, PAPER, SCISSORS }

enum Result { WIN, DRAW, LOSE }

impl Result {
    fn score(&self) -> u32 {
        match *self {
            Result::WIN => 0,
            Result::DRAW => 3,
            Result::LOSE => 6,
        }
    }
}

impl Play {
    fn score(&self) -> u32 {
        match *self {
            Play::ROCK => 1,
            Play::PAPER => 2,
            Play::SCISSORS => 3,
        }
    }
}

fn result(opponent: &Play, me: &Play) -> Result {
    match opponent {
        Play::ROCK =>
            match me {
                Play::ROCK => Result::DRAW,
                Play::PAPER => Result::LOSE,
                Play::SCISSORS => Result::WIN,
            }
        Play::PAPER =>
            match me {
                Play::ROCK => Result::WIN,
                Play::PAPER => Result::DRAW,
                Play::SCISSORS => Result::LOSE,
            }
        Play::SCISSORS =>
            match me {
                Play::ROCK => Result::LOSE,
                Play::PAPER => Result::WIN,
                Play::SCISSORS => Result::DRAW,
            }
    }
}

fn pt1(strategy_guide: &str) -> u32 {
    strategy_guide
        .lines()
        .map(|l| parse_play(l))
        .map(|g| score_game(g))
        .sum()
}

fn score_game((opponent, me): (Play, Play)) -> u32 {
    me.score() + result(&opponent, &me).score()
}

fn parse_play(line: &str) -> (Play, Play) {
    line
        .trim()
        .split_once(' ')
        .map(|(s1, s2)| (string_to_play(&s1), string_to_play(&s2)))
        .unwrap()
}

fn string_to_play(s: &str) -> Play {
    match s {
        "A" => Play::ROCK,
        "B" => Play::PAPER,
        "C" => Play::SCISSORS,
        "X" => Play::ROCK,
        "Y" => Play::PAPER,
        "Z" => Play::SCISSORS,
        _ => panic!("No matching play!")
    }
}

pub fn day02() {
    let input = include_str!("../input/day02.txt");
    println!("Day 2, part 1: {}", pt1(input));
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_input_pt1() {
        let input = "A Y
                           B X
                           C Z";

        assert_eq!(pt1(input), 15)
    }
}

