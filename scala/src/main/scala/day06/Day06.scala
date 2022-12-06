package day06

import scala.annotation.tailrec
import scala.io.Source

def findFirstMarker(str: String, markerSize: Int): Int = {
  str.sliding(markerSize).takeWhile(_.distinct.length != markerSize).length + markerSize
}

@main
def main(): Unit =
  val input = Source.fromResource("day06.txt").getLines().toList.head

  println(s"7 == ${findFirstMarker("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 4)}")
  println(s"5 ==  ${findFirstMarker("bvwbjplbgvbhsrlpgdmjqwftvncz", 4)}")
  println(s"6 ==  ${findFirstMarker("nppdvjthqldpwncqszvftbrmjlhg", 4)}")
  println(s"10 == ${findFirstMarker("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 4)}")
  println(s"11 == ${findFirstMarker("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 4)}")

  println(s"19 == ${findFirstMarker("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 14)}")
  println(s"23 ==  ${findFirstMarker("bvwbjplbgvbhsrlpgdmjqwftvncz", 14)}")
  println(s"23 ==  ${findFirstMarker("nppdvjthqldpwncqszvftbrmjlhg", 14)}")
  println(s"29 == ${findFirstMarker("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 14)}")
  println(s"26 == ${findFirstMarker("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 14)}")

  println(s"Pt1: ${findFirstMarker(input, 4)}")
  println(s"Pt2: ${findFirstMarker(input, 14)}")
