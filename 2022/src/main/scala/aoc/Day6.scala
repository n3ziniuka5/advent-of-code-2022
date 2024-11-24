package aoc

import aoc.Common.timed

import scala.io.Source

object Day6:
  def main(args: Array[String]): Unit =
    val lines = Source.fromResource("day6.txt").getLines().toList
    timed("Part 1", part1(lines.head))
    timed("Part 2", part2(lines.head))

  def markerStart(line: String, n: Int): Int =
    line.zipWithIndex
      .sliding(n)
      .find { l =>
        l.map(_._1).distinct.size == n
      }
      .get
      .head
      ._2 + n

  def part1(line: String): Int =
    markerStart(line, 4)

  def part2(line: String): Int =
    markerStart(line, 14)
