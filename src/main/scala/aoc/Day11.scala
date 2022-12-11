package aoc

import aoc.Common.timed

import scala.annotation.tailrec
import scala.io.Source

object Day11:
  def main(args: Array[String]): Unit =
    val lines = Source.fromResource("day11.txt").getLines().toList
    timed("Part 1", part1(lines))
    timed("Part 2", part2(lines))

  case class Monkey(items: Vector[Long], operation: Long => Long, test: Long => Boolean, ifTrue: Int, ifFalse: Int):
    def withItem(item: Long): Monkey = copy(items = items :+ item)

  def parseMonkeys(lines: List[String]): Vector[Monkey] =
    lines
      .sliding(6, 7)
      .map { monkeyInput =>
        val items           = monkeyInput(1).drop("  Starting items: ".length).split(", ").map(_.toLong).toVector
        val operationString = monkeyInput(2).drop("  Operation: new = ".length)
        val operation: Long => Long =
          if (operationString == "old * old") n => n * n
          else if (operationString.startsWith("old *"))
            val multiplier = operationString.drop("old * ".length).toLong
            worry => worry * multiplier
          else
            val add = operationString.drop("old + ".length).toLong
            worry => worry + add

        val divisibleBy = monkeyInput(3).drop("  Test: divisible by ".length).toInt
        val ifTrue      = monkeyInput(4).drop("    If true: throw to monkey ".length).toInt
        val ifFalse     = monkeyInput(5).drop("    If false: throw to monkey ".length).toInt

        Monkey(items, operation, n => n % divisibleBy == 0, ifTrue, ifFalse)
      }
      .toVector

  def lcm(n1: Int, n2: Int): Int =
    (math.max(n1, n2) to Int.MaxValue).find { i =>
      i % n1 == 0 && i % n2 == 0
    }.get

  def findLcm(lines: List[String]): Int =
    lines
      .sliding(6, 7)
      .map { monkeyInput =>
        monkeyInput(3).drop("  Test: divisible by ".length).toInt
      }
      .reduce(lcm)

  @tailrec
  def play(
    rounds: Int,
    reliefDivision: Int,
    divisorLcm: Int,
    monkeys: Vector[Monkey],
    timesInspected: Map[Int, Long]
  ): Map[Int, Long] =
    if (rounds == 0) timesInspected
    else
      val (monkeysAfterRound, newInspected) = monkeys.indices.foldLeft((monkeys, timesInspected)) {
        case ((monkeysBeforeTurn, inspected), i) =>
          val monkeysAfterTurn = monkeysBeforeTurn(i).items.foldLeft(monkeysBeforeTurn) { (monkeys, worryLevel) =>
            val newWorry = (monkeys(i).operation(worryLevel) / reliefDivision) % divisorLcm
            val passTo   = if (monkeys(i).test(newWorry)) monkeys(i).ifTrue else monkeys(i).ifFalse
            monkeys
              .updated(passTo, monkeys(passTo).withItem(newWorry))
              .updated(i, monkeys(i).copy(items = Vector.empty))
          }
          val newMap = inspected + (i -> (inspected.getOrElse(i, 0L) + monkeysBeforeTurn(i).items.size))
          (monkeysAfterTurn, newMap)
      }
      play(rounds - 1, reliefDivision, divisorLcm, monkeysAfterRound, newInspected)

  def part1(lines: List[String]): Long =
    val timesInspected = play(20, 3, findLcm(lines), parseMonkeys(lines), Map.empty)
    timesInspected.toList.map(_._2).sorted.reverse.take(2).product

  def part2(lines: List[String]): Long =
    val timesInspected = play(10000, 1, findLcm(lines), parseMonkeys(lines), Map.empty)
    timesInspected.toList.map(_._2).sorted.reverse.take(2).product
