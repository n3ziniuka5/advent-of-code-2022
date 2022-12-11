package aoc

import aoc.Common.timed

import scala.annotation.tailrec
import scala.io.Source

object Day11:
  def main(args: Array[String]): Unit =
    val lines = Source.fromResource("day11.txt").getLines().toList
    timed("Part 1", part1(lines))
    timed("Part 2", part2(lines))

  case class Monkey(items: Vector[Long], operation: Long => Long, test: Long => Boolean, ifTrue: Int, ifFalse: Int)

  def parse(lines: List[String]): Vector[Monkey] = {
    lines
      .sliding(6, 7)
      .map { monkeyInput =>
        val items           = monkeyInput(1).drop("  Starting items: ".length).split(", ").map(_.toLong).toVector
        val operationString = monkeyInput(2).drop("  Operation: new = ".length)
        val operation: Long => Long =
          if (operationString == "old * old") n => n * n
          else if (operationString.startsWith("old *"))
            val mult = operationString.drop("old * ".length).toLong
            n => n * mult
          else if (operationString.startsWith("old +"))
            val add = operationString.drop("old + ".length).toLong
            n => n + add
          else n => n

        val divisibleBy = monkeyInput(3).drop("  Test: divisible by ".length).toInt
        val ifTrue      = monkeyInput(4).drop("    If true: throw to monkey ".length).toInt
        val ifFalse     = monkeyInput(5).drop("    If false: throw to monkey ".length).toInt

        Monkey(items, operation, n => n % divisibleBy == 0, ifTrue, ifFalse)
      }
      .toVector
  }

  @tailrec
  def play(rounds: Int, divideBy: Int, monkeys: Vector[Monkey], timesInspected: Map[Int, Long]): Map[Int, Long] = {
    if (rounds == 0) timesInspected
    else
      val (newMonkeys, newInspected) = monkeys.indices.foldLeft((monkeys, timesInspected)) {
        case ((monkeys, inspected), i) =>
          val newMap = inspected + (i -> (inspected.getOrElse(i, 0L) + monkeys(i).items.size))
          val newMonkeys = monkeys(i).items.foldLeft(monkeys) { (accm, item) =>
            val newWorry = (monkeys(i).operation(item) / divideBy) % 9699690
            val passTo   = if (monkeys(i).test(newWorry)) monkeys(i).ifTrue else monkeys(i).ifFalse
            accm
              .updated(passTo, accm(passTo).copy(items = accm(passTo).items :+ newWorry))
              .updated(i, accm(i).copy(items = Vector.empty))
          }
          (newMonkeys, newMap)
      }
      play(rounds - 1, divideBy, newMonkeys, newInspected)
  }

  def part1(lines: List[String]): Long =
    val timesInspected = play(20, 3, parse(lines), Map.empty)
    println(timesInspected)
    timesInspected.toList.map(_._2).sorted.reverse.take(2).product

  def part2(lines: List[String]): Long =
    val timesInspected = play(10000, 1, parse(lines), Map.empty)
    println(timesInspected)
    timesInspected.toList.map(_._2).sorted.reverse.take(2).product
