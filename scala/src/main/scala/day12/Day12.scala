package day12

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

case class Position(x: Int, y: Int)

case class Area(graph: Graph, goal: Position, bottomRight: Position)

type Graph = Map[Position, Int]

@tailrec
def dijkstra(area: Area, toVisit: mutable.PriorityQueue[(Position, Int)], distances: Graph, visited: Set[Position] = Set()): Int =
  val maybeNext = getNext(toVisit, visited)
  if maybeNext.isEmpty then
    Int.MaxValue
  else
    val (nextToVisit, _) = maybeNext.get
    if nextToVisit == area.goal then
      distances(nextToVisit)
    else
      val neighbours = getNeighbourPositions(area, nextToVisit).filterNot(visited.contains).filter(canVisit(nextToVisit, area.graph))
      val neighboursUpdatedDistance = neighbours.map(updateDistance(distances, nextToVisit))
      val updatedDistances = distances ++ neighboursUpdatedDistance
      neighboursUpdatedDistance.foreach(toVisit.enqueue(_))
      dijkstra(area, toVisit, updatedDistances, visited + nextToVisit)

def getNext(toVisit: mutable.PriorityQueue[(Position, Int)], visited: Set[Position]): Option[(Position, Int)] =
  if toVisit.isEmpty then None
  else
    var next = toVisit.dequeue()
    while (toVisit.nonEmpty && visited.contains(next._1))
      next = toVisit.dequeue()
    if visited.contains(next._1) then None
    else Some(next)

def canVisit(position: Position, graph: Graph) =
  (neighbourPosition: Position) => graph(position) >= graph(neighbourPosition) || (graph(position) + 1) == graph(neighbourPosition)

def updateDistance(distances: Graph, currentPosition: Position)(position: Position): (Position, Int) =
  val distance = distances.getOrElse(position, Int.MaxValue)
  val newDistance = List(distance, 1 + distances(currentPosition)).min
  (position, newDistance)

def getNeighbourPositions(cave: Area, p: Position): List[Position] =
  List(
    Position(p.x - 1, p.y),
    Position(p.x, p.y - 1), Position(p.x, p.y + 1),
    Position(p.x + 1, p.y),
  ).filter(p => p.x >= 0 && p.y >= 0 && p.x <= cave.bottomRight.x && p.y <= cave.bottomRight.y)

def parseInput(lines: List[String]): Graph =
  lines
    .zipWithIndex
    .flatMap((line: String, y: Int) =>
      line.toCharArray
        .map(_.toInt)
        .zipWithIndex
        .map((cost: Int, x: Int) => (Position(x, y), cost)))
    .toMap


@main
def main(): Unit =
  val input = Source.fromResource("day12.txt").getLines().toList

  val graph = parseInput(input)
  val start = graph.filter(_._2 == 'S'.toInt).head._1
  val end = graph.filter(_._2 == 'E'.toInt).head._1
  val bottomRight = graph.keys.toList.sortBy(_.x).reverse.maxBy(_.y)
  val updatedGraph = graph + (start -> 'a'.toInt) + (end -> 'z'.toInt)
  val area = Area(updatedGraph, end, bottomRight)
  val toVisit = mutable.PriorityQueue.empty[(Position, Int)](
    Ordering.by((_: (Position, Int))._2).reverse
  )
  toVisit.enqueue((start, 0))
  val res = dijkstra(area, toVisit, Map((start, 0)))

  println(s"Pt1: $res")

  val pt2 = graph.filter(_._2 == 'a'.toInt).keys.map(p => {
    val toVisit2 = mutable.PriorityQueue.empty[(Position, Int)](
      Ordering.by((_: (Position, Int))._2).reverse
    )
    toVisit2.enqueue((p, 0))
    dijkstra(area, toVisit2, Map((p, 0)))
  }).min
  println(s"Pt2: $pt2")


val TEST =
  """
    |Sabqponm
    |abcryxxl
    |accszExk
    |acctuvwj
    |abdefghi
    |""".stripMargin.trim.split('\n').toList