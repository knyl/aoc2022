package day03

import scala.annotation.tailrec
import scala.io.Source

def findDuplicate(line: String): Char =
  val (rucksack1, rucksack2) = line.splitAt(line.length/2)
  rucksack1.toSet.intersect(rucksack2.toSet).head

def getScore(char: Char): Int =
  if char.isLower then
    char - 'a' + 1
  else
    char - 'A' + 27

def findGroupBadge(rucksacks: List[String]): Char =
  rucksacks
    .map(line => line.toSet)
    .reduce((items1, items2) => items1.intersect(items2))
    .head

@main
def main(): Unit =
  val lines = Source.fromResource("day03.txt").getLines().toList

  val pt1 = lines
    .map(findDuplicate)
    .map(getScore)
    .sum

  val pt2 = lines
      .sliding(3, 3)
      .map(findGroupBadge)
      .map(getScore)
      .sum

  println("Pt1: " + pt1)
  println("Pt2: " + pt2)
