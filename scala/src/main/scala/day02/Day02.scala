package day02

import scala.annotation.tailrec
import scala.io.Source

enum Play:
  case ROCK, PAPER, SCISSORS

enum Result:
  case LOSE, DRAW, WIN

def toPlay(char: String): Play  =
  char match {
    case "X" => Play.ROCK
    case "A" => Play.ROCK
    case "Y" => Play.PAPER
    case "B" => Play.PAPER
    case "Z" => Play.SCISSORS
    case "C" => Play.SCISSORS
    case _ =>  throw new RuntimeException("Should never happen")
  }
def toResult(char: String): Result  =
  char match
    case "X" => Result.LOSE
    case "Y" => Result.DRAW
    case "Z" => Result.WIN
    case _ =>  throw new RuntimeException("Should never happen")

def score(game: (Play, Play)) : Int =
  game match
    case (Play.PAPER, Play.ROCK) => 1
    case (Play.SCISSORS, Play.PAPER) => 2
    case (Play.ROCK, Play.SCISSORS) => 3

    case (Play.ROCK, Play.ROCK) => 1 + 3
    case (Play.PAPER, Play.PAPER) => 2 + 3
    case (Play.SCISSORS, Play.SCISSORS) => 3 + 3

    case (Play.SCISSORS, Play.ROCK) => 1 + 6
    case (Play.ROCK, Play.PAPER) => 2 + 6
    case (Play.PAPER, Play.SCISSORS) => 3 + 6

def score2(game: (Play, Result)): Int =
  game match
    case (Play.PAPER, Result.LOSE) => 1
    case (Play.PAPER, Result.DRAW) => 5
    case (Play.PAPER, Result.WIN) => 9
    case (Play.ROCK, Result.LOSE) => 3
    case (Play.ROCK, Result.DRAW) => 4
    case (Play.ROCK, Result.WIN) => 8
    case (Play.SCISSORS, Result.LOSE) => 2
    case (Play.SCISSORS, Result.DRAW) => 6
    case (Play.SCISSORS, Result.WIN) => 7

def parseInput(line: String): (Play, Play) =
  line match {
    case s"$opponent $me" => (toPlay(opponent), toPlay(me))
    case _ => throw new RuntimeException(s"Not matching line: $line")
  }
def parseInput2(line: String): (Play, Result) =
  line match {
    case s"$opponent $me" => (toPlay(opponent), toResult(me))
    case _ => throw new RuntimeException(s"Not matching line: $line")
  }


@main
def main(): Unit =
  val input = Source.fromResource("day02.txt").getLines().toList
  val rounds = input
    .map(parseInput)
    .map(score)
    .sum

  println("Pt1: " + rounds)

  val rounds2 = input
    .map(parseInput2)
    .map(score2)
    .sum
  println("Pt2: " + rounds2)
