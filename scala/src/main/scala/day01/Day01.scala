package day01
import scala.annotation.tailrec
import scala.io.Source

@tailrec
def calorie_sums(numbers: List[String], currSum: Int = 0, acc: List[Int] = Nil) : List[Int] =
  numbers match {
    case Nil => acc
    case n1 :: tail if n1 == "" => calorie_sums(tail, 0, currSum :: acc)
    case _ => calorie_sums(numbers.tail, numbers.head.toInt + currSum, acc)
  }


@main
def main(): Unit =
  val lines = Source.fromResource("day01.txt").getLines().toList

  println("Pt1: " + calorie_sums(lines).max)
  println("Pt2: " + calorie_sums(lines).sorted.reverse.take(3).sum)
