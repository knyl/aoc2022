package day14

import scala.annotation.tailrec
import scala.io.Source

case class Position(x: Int, y: Int)

def getPaths(string: String) =
  val paths = string.split(" -> ").map(str => str.span(_ != ',')).map((x, y) => (x.toInt, y.tail.toInt)).toList
  makeCoordinates(paths)

@tailrec
def makeCoordinates(tuples: List[(Int, Int)], coords: List[Position] = List()): List[Position] = tuples match
  case Nil => coords
  case _ :: Nil => coords
  case (x1, y1) :: (x2, y2) :: _ if x1 == x2 && y1 < y2 => makeCoordinates(tuples.tail, coords ++ (y1 to y2).map(y => Position(x1, y)))
  case (x1, y1) :: (x2, y2) :: _ if x1 == x2 => makeCoordinates(tuples.tail, coords ++ (y1 to y2 by -1).map(y => Position(x1, y)))
  case (x1, y1) :: (x2, y2) :: _ if y1 == y2 && x1 < x2 => makeCoordinates(tuples.tail, coords ++ (x1 to x2).map(x => Position(x, y1)))
  case (x1, y1) :: (x2, y2) :: _ if y1 == y2 => makeCoordinates(tuples.tail, coords ++ (x1 to x2 by -1).map(x => Position(x, y1)))
  case _ => throw new RuntimeException(s"Invalid input $tuples")

def pt1(rocks: Set[Position], sand: Set[Position] = Set()): Int =
  val finalSand = moveSand(rocks, sand)
  finalSand.size

def pt2(rocks: Set[Position], sand: Set[Position] = Set()): Int =
  val floorLevel = rocks.map(_.y).max + 1
  val finalSand = moveSand2(floorLevel, rocks, sand)
  finalSand.size

@tailrec
def moveSand(rocks: Set[Position], sand: Set[Position], fallingSand: Position = Position(500, 0)): Set[Position] = sandMovement(fallingSand, rocks, sand) match
  case None if fallingSand == Position(500, 0) => sand + fallingSand
  case None => moveSand(rocks, sand + fallingSand)
  case Some(newSand) if isInside(newSand, rocks) => moveSand(rocks, sand, newSand)
  case Some(_) => sand

@tailrec
def moveSand2(floorLevel: Int, rocks: Set[Position], sand: Set[Position], fallingSand: Position = Position(500, 0)): Set[Position] = sandMovement(fallingSand, rocks, sand) match
  case None if fallingSand == Position(500, 0) => sand + fallingSand
  case None => moveSand2(floorLevel, rocks, sand + fallingSand)
  case Some(newSand) if newSand.y < floorLevel => moveSand2(floorLevel, rocks, sand, newSand)
  case Some(newSand) => moveSand2(floorLevel, rocks, sand + newSand)

def isInside(pos: Position, rocks: Set[Position]) = pos.y <= rocks.map(_.y).max

def sandMovement(fallingSand: Position, rocks: Set[Position], sand: Set[Position]): Option[Position] =
  val newSand = List(fallingSand.copy(y = fallingSand.y + 1), fallingSand.copy(x = fallingSand.x - 1, y = fallingSand.y + 1), fallingSand.copy(x = fallingSand.x + 1, y = fallingSand.y + 1))
    .find(pos => !rocks.contains(pos) && !sand.contains(pos))
  newSand

@main
def main(): Unit =
  val input = Source.fromResource("day14.txt").getLines().toList
  val rockPaths = input.flatMap(getPaths).toSet

  println(s"Part 1: ${pt1(rockPaths)}")
  println(s"Part 2: ${pt2(rockPaths)}")

val TEST =
  """
    |498,4 -> 498,6 -> 496,6
    |503,4 -> 502,4 -> 502,9 -> 494,9
    |""".stripMargin.trim.split('\n').toList