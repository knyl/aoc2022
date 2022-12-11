package day11

import scala.annotation.tailrec
import scala.io.Source

type WorryLevel = Long
type Inspections = Long
type Name = Int
type MonkeyMap = Map[Name, Monkey]
type MonkeyInspections = Map[Name, Inspections]
type WorryDecreaseFn = WorryLevel => WorryLevel

case class Monkey(name: Name, items: List[WorryLevel], operation: Operation, test: ThrowTest)

case class ThrowTest(divisibleBy: Int, ifTrue: Name, ifFalse: Name)

trait Operation:
  def op(oldValue: WorryLevel): WorryLevel

case class Square() extends Operation:
  override def op(oldValue: WorryLevel): WorryLevel = oldValue * oldValue

case class Addition(rhs: Int) extends Operation:
  override def op(oldValue: WorryLevel): WorryLevel = oldValue + rhs

case class Multiplication(rhs: Int) extends Operation:
  override def op(oldValue: WorryLevel): WorryLevel = oldValue * rhs

def parseInput(input: List[String]): List[Monkey] =
  input.grouped(7).toList.map(parseMonkey)

def parseMonkey(input: List[String]): Monkey =
  val name = parseName(input.head)
  val items = parseItems(input(1))
  val operation = parseOperation(input(2))
  val test = parseTest(input(3))
  val ifTrue = cond(input(4))
  val ifFalse = cond(input(5))
  Monkey(name, items, operation, ThrowTest(test, ifTrue, ifFalse))


def parseName(string: String) = string.trim match
  case s"Monkey $num:" => num.toInt
  case _ => throw new RuntimeException(s"Unexpected string: $string")

def parseItems(string: String) = string.trim match
  case s"Starting items: $items" => items.split(", ").map(_.toLong).toList
  case _ => throw new RuntimeException(s"Unexpected string: $string")

def parseOperation(string: String) = string.trim match
  case s"Operation: new = old * old" => Square()
  case s"Operation: new = old + $rhs" => Addition(rhs.toInt)
  case s"Operation: new = old * $rhs" => Multiplication(rhs.toInt)
  case _ => throw new RuntimeException(s"Unexpected string: $string")

def parseTest(string: String) = string.trim match
  case s"Test: divisible by $num" => num.toInt
  case _ => throw new RuntimeException(s"Unexpected string: $string")

def cond(string: String) = string.trim match
  case s"If true: throw to monkey $num" => num.toInt
  case s"If false: throw to monkey $num" => num.toInt
  case _ => throw new RuntimeException(s"Unexpected string: $string")

@tailrec
def doRounds(maxRounds: Int, worryDecrease: WorryDecreaseFn, monkeys: MonkeyMap, inspections: MonkeyInspections = Map(), round: Int = 0): MonkeyInspections = round match
  case _ if maxRounds == round => inspections
  case _ =>
    val (updatedMonkeys, interactions) = doTurns(monkeys, worryDecrease)
    val updatedInspections = interactions.foldLeft(inspections)((acc, v) => updateInspections((acc, v)))
    doRounds(maxRounds, worryDecrease, updatedMonkeys, updatedInspections, round + 1)

@tailrec
def doTurns(monkeys: Map[Name, Monkey], worryDecrease: WorryDecreaseFn, interactions: List[(Name, Inspections)] = List(), turn: Int = 0): (Map[Name, Monkey], List[(Name, Inspections)]) = turn match
  case _ if turn >= monkeys.size => (monkeys, interactions)
  case m =>
    val monkeyInteractions = monkeys(m).items.size
    val updatedItems = monkeys(m).items.map(i => inspectItem(worryDecrease, i, monkeys(m)))
    val updatedMonkeys = updatedItems.foldLeft(monkeys)((acc, v) => updateMonkeyItems(acc, v)) + (m -> monkeys(m).copy(items = List()))
    doTurns(updatedMonkeys, worryDecrease, (m, monkeyInteractions) :: interactions, turn + 1)

def inspectItem(worryDecrease: WorryDecreaseFn, worryLevel: WorryLevel, monkey: Monkey) =
  val inspectedWorryLevel = worryDecrease(monkey.operation.op(worryLevel))
  if inspectedWorryLevel % monkey.test.divisibleBy == 0 then (monkey.test.ifTrue, inspectedWorryLevel)
  else (monkey.test.ifFalse, inspectedWorryLevel)

def updateMonkeyItems(tuple: (Map[Name, Monkey], (Name, WorryLevel))): Map[Name, Monkey] =
  val (monkeys, (name, item)) = tuple
  monkeys + (name -> monkeys(name).copy(items = monkeys(name).items ++ List(item)))

def updateInspections(tuple: (Map[Name, Inspections], (Name, Inspections))) =
  val (monkeys, (name, inspections)) = tuple
  monkeys + (name -> (monkeys.getOrElse(name, 0L) + inspections))

def pt1(monkeys: MonkeyMap): WorryLevel =
  val worryDecreaseFn = (v: WorryLevel) => v / 3L
  val inspections = doRounds(20, worryDecreaseFn, monkeys)
  inspections.values.toList.sorted.reverse.take(2).product

def pt2(monkeys: MonkeyMap): WorryLevel =
  val worryDecrease = monkeys.values.map(_.test.divisibleBy).product
  val worryDecreaseFn = (v: WorryLevel) => v % worryDecrease
  val inspections = doRounds(10000, worryDecreaseFn, monkeys)
  inspections.values.toList.sorted.reverse.take(2).product

@main
def main(): Unit =
  val input = Source.fromResource("day11.txt").getLines().toList
  val monkeys = parseInput(input).map(m => (m.name, m)).toMap
  println(s"Pt1: ${pt1(monkeys)}")
  println(s"Pt2: ${pt2(monkeys)}")

val TEST =
  """
    |Monkey 0:
    |  Starting items: 79, 98
    |  Operation: new = old * 19
    |  Test: divisible by 23
    |    If true: throw to monkey 2
    |    If false: throw to monkey 3
    |
    |Monkey 1:
    |  Starting items: 54, 65, 75, 74
    |  Operation: new = old + 6
    |  Test: divisible by 19
    |    If true: throw to monkey 2
    |    If false: throw to monkey 0
    |
    |Monkey 2:
    |  Starting items: 79, 60, 97
    |  Operation: new = old * old
    |  Test: divisible by 13
    |    If true: throw to monkey 1
    |    If false: throw to monkey 3
    |
    |Monkey 3:
    |  Starting items: 74
    |  Operation: new = old + 3
    |  Test: divisible by 17
    |    If true: throw to monkey 0
    |    If false: throw to monkey 1
    |
    |""".stripMargin.trim.split('\n').toList
