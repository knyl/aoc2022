package day04

import scala.io.Source

case class Assignment(from: Int, to: Int):
  val range: Set[Int] = Set.from(from to to)

  def fullyContains(other: Assignment): Boolean =
    range.contains(other.to) && range.contains(other.from)

  def anyOverlap(other: Assignment): Boolean =
    range.contains(other.to) || range.contains(other.from)


def fullyContains(p1: Assignment, p2: Assignment): Boolean =
  p1.fullyContains(p2) || p2.fullyContains(p1)

def anyOverlap(p1: Assignment, p2: Assignment): Boolean =
  p1.anyOverlap(p2) || p2.anyOverlap(p1)

def parseInput(line: String): (Assignment, Assignment) = line match {
  case s"$p1-$p2,$p3-$p4" => (Assignment(p1.toInt, p2.toInt), Assignment(p3.toInt, p4.toInt))
  case _ => throw new RuntimeException(s"No match for $line")
}

@main
def main(): Unit =
  val input = Source.fromResource("day04.txt").getLines().toList
  val assignments = input.map(parseInput)

  val pt1 = assignments.count(fullyContains)
  val pt2 = assignments.count(anyOverlap)

  println("Pt1: " + pt1)
  println("Pt2: " + pt2)
