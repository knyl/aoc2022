package day08

import scala.annotation.tailrec
import scala.io.Source

case class Position(x: Int, y: Int)

object Position:
  def up(p: Position): Position = Position(p.x, p.y - 1)

  def down(p: Position): Position = Position(p.x, p.y + 1)

  def left(p: Position): Position = Position(p.x - 1, p.y)

  def right(p: Position): Position = Position(p.x + 1, p.y)

  def allDirections(): List[Position => Position] = List(Position.up, Position.down, Position.left, Position.right)

case class Trees(graph: Graph, width: Int, height: Int)

type Graph = Map[Position, Int]
type Step = Position => Position

def pt1(trees: Trees): Int =
  trees.graph.map(getVisibility(_, trees)).count(p => p)

def getVisibility(tuple: (Position, Int), trees: Trees): Boolean =
  val (position, height) = tuple
  Position.allDirections().map(isVisible(_, position, height, trees)).reduce(_ || _)

@tailrec
def isVisible(stepFn: Step, tree: Position, height: Int, trees: Trees): Boolean =
  val neighbor = stepFn(tree)
  if isOutside(neighbor, trees) then true
  else if trees.graph(neighbor) >= height then false
  else isVisible(stepFn, neighbor, height, trees)

def pt2(trees: Trees): Int =
  trees.graph.map(calculateScenicScore(_, trees)).max

def calculateScenicScore(tuple: (Position, Int), trees: Trees): Int =
  val (position, height) = tuple
  Position.allDirections().map(getScenicScore(_, position, height, trees)).product

def isOutside(position: Position, trees: Trees) =
  position.x < 0 || position.y < 0 || position.x > trees.width || position.y > trees.height

@tailrec
def getScenicScore(stepFn: Step, tree: Position, height: Int, trees: Trees, score: Int = 0): Int =
  val neighbor = stepFn(tree)
  if isOutside(neighbor, trees) then score
  else if trees.graph(neighbor) >= height then score + 1
  else getScenicScore(stepFn, neighbor, height, trees, score + 1)

def parseInput(lines: List[String]): Graph =
  lines
    .zipWithIndex
    .flatMap((line: String, y: Int) =>
      line.toCharArray
        .map(_.asDigit)
        .zipWithIndex
        .map((cost: Int, x: Int) => (Position(x, y), cost)))
    .toMap

@main
def main(): Unit =
  val input = Source.fromResource("day08.txt").getLines().toList
  val graph = parseInput(input)
  val width = graph.keys.map(_.x).max
  val height = graph.keys.map(_.y).max
  val trees = Trees(graph, width, height)

  println(s"Pt1: ${pt1(trees)}")
  println(s"Pt2: ${pt2(trees)}")
