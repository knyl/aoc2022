package day10

import day05.Instruction

import scala.annotation.tailrec
import scala.io.Source

case class State(cycle: Int, xReg: Int)

trait Instruction:
  def Op(state: State): State

object Noop extends Instruction:
  override def Op(state: State): State = state.copy(cycle = state.cycle + 1)

case class AddX(x: Int) extends Instruction:
  override def Op(state: State): State = state.copy(cycle = state.cycle + 2, xReg = state.xReg + x)

def parseLine(str: String): Instruction = str match
  case s"noop" => Noop
  case s"addx $x" => AddX(x.toInt)
  case _ => throw new RuntimeException(s"Invalid input: $str")

def pt1(instructions: List[Instruction]): Int =
  val states = processInstructions(instructions)

  states
    .zipWithIndex
    .flatMap((state: State, ind: Int) =>
      if state.cycle % 40 == 20 then Some(states(ind - 1))
      else if state.cycle % 40 == 19 then Some(states(ind))
      else None)
    .distinct
    .map(getScore)
    .sum

def pt2(instructions: List[Instruction]): Unit =
  val states = processInstructions(instructions)
  val stateMap = states.map(s => s.cycle -> s.xReg).toMap
  val pixels = (0 to states.last.cycle).map(getPixel(_, stateMap)).toList
  val pixelRows = pixels.sliding(40, 40)
  pixelRows.map(_.mkString("")).foreach(println)


def getPixel(pos: Int, stateMap: Map[Int, Int]): String =
  val spritePos = stateMap.getOrElse(pos, stateMap(pos - 1))
  if spritePos >= (pos - 1) % 40 && spritePos <= (pos + 1) % 40 then "#"
  else " "

def getScore(state: State): Int =
  (state.cycle + 2) / 10 * 10 * state.xReg

@tailrec
def processInstructions(instructions: List[Instruction], results: List[State] = List(State(cycle = 0, xReg = 1))): List[State] = instructions match
  case Nil => results.reverse
  case head :: tail => processInstructions(tail, head.Op(results.head) :: results)

@main
def main(): Unit =
  val input = Source.fromResource("day10.txt").getLines().toList
  val instructions = input.map(parseLine)
  println(s"pt1: ${pt1(instructions)}")
  pt2(instructions)


val INPUT =
  """
    |addx 15
    |addx -11
    |addx 6
    |addx -3
    |addx 5
    |addx -1
    |addx -8
    |addx 13
    |addx 4
    |noop
    |addx -1
    |addx 5
    |addx -1
    |addx 5
    |addx -1
    |addx 5
    |addx -1
    |addx 5
    |addx -1
    |addx -35
    |addx 1
    |addx 24
    |addx -19
    |addx 1
    |addx 16
    |addx -11
    |noop
    |noop
    |addx 21
    |addx -15
    |noop
    |noop
    |addx -3
    |addx 9
    |addx 1
    |addx -3
    |addx 8
    |addx 1
    |addx 5
    |noop
    |noop
    |noop
    |noop
    |noop
    |addx -36
    |noop
    |addx 1
    |addx 7
    |noop
    |noop
    |noop
    |addx 2
    |addx 6
    |noop
    |noop
    |noop
    |noop
    |noop
    |addx 1
    |noop
    |noop
    |addx 7
    |addx 1
    |noop
    |addx -13
    |addx 13
    |addx 7
    |noop
    |addx 1
    |addx -33
    |noop
    |noop
    |noop
    |addx 2
    |noop
    |noop
    |noop
    |addx 8
    |noop
    |addx -1
    |addx 2
    |addx 1
    |noop
    |addx 17
    |addx -9
    |addx 1
    |addx 1
    |addx -3
    |addx 11
    |noop
    |noop
    |addx 1
    |noop
    |addx 1
    |noop
    |noop
    |addx -13
    |addx -19
    |addx 1
    |addx 3
    |addx 26
    |addx -30
    |addx 12
    |addx -1
    |addx 3
    |addx 1
    |noop
    |noop
    |noop
    |addx -9
    |addx 18
    |addx 1
    |addx 2
    |noop
    |noop
    |addx 9
    |noop
    |noop
    |noop
    |addx -1
    |addx 2
    |addx -37
    |addx 1
    |addx 3
    |noop
    |addx 15
    |addx -21
    |addx 22
    |addx -6
    |addx 1
    |noop
    |addx 2
    |addx 1
    |noop
    |addx -10
    |noop
    |noop
    |addx 20
    |addx 1
    |addx 2
    |addx 2
    |addx -6
    |addx -11
    |noop
    |noop
    |noop
    |""".stripMargin.trim.split('\n').toList