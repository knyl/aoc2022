package day05

import scala.annotation.tailrec
import scala.io.Source

case class Instruction(num: Int, from: Int, to: Int)

type Crates = Map[Int, List[Char]]
type Crane = List[Char] => List[Char]

def parseInstructions(line: String): Instruction = line match {
  case s"move $num from $from to $to" => Instruction(num.toInt, from.toInt, to.toInt)
  case _ => throw new RuntimeException(s"No match for $line")
}

def parseStartConfig(line: List[Char]): (Int, List[Char]) =
  val key = line.head.asDigit
  key -> line.tail.filter(_ != ' ').reverse

@tailrec
def move_crates(instructions: List[Instruction], craneFn: Crane, crates: Crates): Crates = instructions match
  case Nil => crates
  case ::(instruction, next) =>
    val (cratesToMove, cratesLeft) = crates(instruction.from).splitAt(instruction.num)
    val cratesAddedTo = craneFn(cratesToMove) ++ crates(instruction.to)
    val updatedCrates = crates + (instruction.from -> cratesLeft) + (instruction.to -> cratesAddedTo)
    move_crates(next, craneFn, updatedCrates)

def crane9000(list: List[Char]): List[Char] = list.reverse
def crane9001(list: List[Char]): List[Char] = list

def getTopCrates(crates: Crates) = crates.toList.sortBy((key, _) => key).map((_, list) => list.head).mkString

@main
def main(): Unit =
  val input = Source.fromResource("day05.txt").getLines().toList
  //val input = Source.fromResource("test-day05.txt").getLines().toList
  val (startConfigLines, instructionsLines) = input.span(!_.isBlank)
  val instructions = instructionsLines.tail.map(parseInstructions)
  val startConfig = startConfigLines
    .transpose
    .map(_.reverse)
    .filter(_.head != ' ')
    .map(parseStartConfig)
    .toMap

  val pt1 = move_crates(instructions, crane9000, startConfig)
  val pt2 = move_crates(instructions, crane9001, startConfig)
  println(s"pt1: ${getTopCrates(pt1)}")
  println(s"pt2: ${getTopCrates(pt2)}")

