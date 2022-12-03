use Shape::{PAPER, ROCK, SCISSORS};


#[derive(PartialEq, Debug, Copy, Clone)]
enum Shape { ROCK = 0, PAPER = 1, SCISSORS = 2 }

impl TryFrom<i32> for Shape {
    type Error = ();

    fn try_from(v: i32) -> std::result::Result<Self, Self::Error> {
        match v {
            x if x == ROCK as i32 => Ok(ROCK),
            x if x == PAPER as i32 => Ok(PAPER),
            x if x == SCISSORS as i32 => Ok(SCISSORS),
            _ => Err(()),
        }
    }
}

impl Shape {
    const fn score(&self) -> u32 {
        *self as u32 + 1
    }
    fn play(&self, opponent: Shape) -> Result {
        ((*self as i32 - opponent as i32 + 3) % 3).try_into().unwrap()
    }
}

#[derive(PartialEq, Debug, Copy, Clone)]
enum Result { DRAW = 0, WIN = 1, LOSE = 2 }

impl TryFrom<i32> for Result {
    type Error = ();

    fn try_from(v: i32) -> std::result::Result<Self, Self::Error> {
        match v {
            x if x == Result::DRAW as i32 => Ok(Result::DRAW),
            x if x == Result::WIN as i32 => Ok(Result::WIN),
            x if x == Result::LOSE as i32 => Ok(Result::LOSE),
            _ => Err(()),
        }
    }
}

impl Result {
    const fn score(&self) -> u32 {
        match *self {
            Result::LOSE => 0,
            Result::DRAW => 3,
            Result::WIN => 6,
        }
    }
}


fn pt1(strategy_guide: &str) -> u32 {
    evaluate_strategy_guide(strategy_guide, parse_play)
}

fn pt2(strategy_guide: &str) -> u32 {
    evaluate_strategy_guide(strategy_guide, parse_play2)
}

fn evaluate_strategy_guide(strategy_guide: &str, parse_fn: fn(&str) -> (Shape, Shape)) -> u32 {
    strategy_guide
        .lines()
        .map(parse_fn)
        .map(calculate_score)
        .sum()
}

fn calculate_score((opponent, me): (Shape, Shape)) -> u32 {
    me.score() + me.play(opponent).score()
}

fn parse_play(line: &str) -> (Shape, Shape) {
    line
        .trim()
        .split_once(' ')
        .map(|(s1, s2)| (string_to_play(&s1), string_to_play(&s2)))
        .unwrap()
}

fn parse_play2(line: &str) -> (Shape, Shape) {
    line
        .trim()
        .split_once(' ')
        .map(|(s1, s2)| (string_to_play(&s1), string_to_result(&s2)))
        .map(play_from_result2)
        .unwrap()
}

fn play_from_result2((opponent, result): (Shape, Result)) -> (Shape, Shape) {
    let me: Shape = ((opponent as i32 + result as i32) % 3).try_into().unwrap();
    (opponent, me)
}

fn string_to_play(s: &str) -> Shape {
    match s {
        "A" => ROCK,
        "B" => PAPER,
        "C" => SCISSORS,
        "X" => ROCK,
        "Y" => PAPER,
        "Z" => SCISSORS,
        _ => panic!("No matching play for {}!", s)
    }
}

fn string_to_result(s: &str) -> Result {
    match s {
        "X" => Result::LOSE,
        "Y" => Result::DRAW,
        "Z" => Result::WIN,
        _ => panic!("No matching result for {}!", s)
    }
}

pub fn day02() {
    let input = include_str!("../input/day02.txt");
    println!("Day 2, part 1: {}", pt1(input));
    println!("Day 2, part 2: {}", pt2(input));
}


#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = "A Y\nB X\nC Z";

    #[test]
    fn test_input_pt1() {
        assert_eq!(pt1(INPUT), 15)
    }

    #[test]
    fn test_input_pt2() {
        assert_eq!(pt2(INPUT), 12)
    }

    #[test]
    fn gameplay_is_working() {
        assert_eq!(ROCK.play(SCISSORS), Result::WIN);
        assert_eq!(PAPER.play(ROCK), Result::WIN);
        assert_eq!(SCISSORS.play(PAPER), Result::WIN);
        assert_eq!(ROCK.play(ROCK), Result::DRAW);
        assert_eq!(PAPER.play(PAPER), Result::DRAW);
        assert_eq!(SCISSORS.play(SCISSORS), Result::DRAW);
        assert_eq!(ROCK.play(PAPER), Result::LOSE);
        assert_eq!(PAPER.play(SCISSORS), Result::LOSE);
        assert_eq!(SCISSORS.play(ROCK), Result::LOSE);
    }
}

