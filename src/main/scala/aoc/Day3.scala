package aoc

import aoc.Common.timed
import scala.io.Source

object Day3:
  def main(args: Array[String]): Unit =
    val lines = Source.fromResource("day3.txt").getLines().toList
    timed("Part 1", part1(lines))
    timed("Part 2", part2(lines))

  def charPriority(c: Char): Int =
    if (c.isUpper) c.toInt - 38
    else c.toInt - 96

  def part1(lines: List[String]): Int =
    lines
      .map { line =>
        val (r1, r2)  = line.splitAt(line.size / 2)
        val commonSet = r1.toSet & r2.toSet
        commonSet.head
      }
      .map(charPriority)
      .sum

  def part2(lines: List[String]): Int =
    lines
      .sliding(3, 3)
      .map { group =>
        group
          .foldLeft(('a' to 'z').toSet ++ ('A' to 'Z').toSet)((commonItems, currentBag) =>
            commonItems & currentBag.toSet
          )
          .head
      }
      .map(charPriority)
      .sum
