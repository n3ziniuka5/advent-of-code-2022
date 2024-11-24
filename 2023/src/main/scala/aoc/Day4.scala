package aoc

import aoc.Common.timed
import zio.prelude.ZSet

import scala.io.Source

object Day4:
    case class Line(cardNumber: Int, winningNumbers: Set[Int], yourNumbers: Set[Int])

    def main(args: Array[String]): Unit =
        val lines = Source.fromResource("day4.txt").getLines().toList
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    def parse(lines: List[String]): List[Line] =
        def parseNumbers(s: String): Set[Int] =
            s.split(" ").flatMap(_.trim.toIntOption).toSet

        lines.map { case s"Card $n: $winningLine | $yourNumbers" =>
            Line(n.trim.toInt, parseNumbers(winningLine), parseNumbers(yourNumbers))
        }

    def part1(lines: List[String]): Int =
        parse(lines).map { case Line(n, a, b) =>
            val matchCount = b.count(a.contains)
            if (matchCount > 0) math.pow(2, matchCount - 1).toInt else 0
        }.sum

    def part2(lines: List[String]): Int =
        parse(lines)
            .foldLeft(ZSet[Int]()) { case (acc, Line(n, a, b)) =>
                val matchCount = b.count(a.contains)
                (1 to matchCount).foldLeft(acc.combine(ZSet(n))) { case (acc, i) =>
                    acc.combine(ZSet.fromMap(Map(n + i -> acc(n))))
                }
            }
            .toMap
            .values
            .sum
