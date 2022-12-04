package aoc

import aoc.Common.{enqueueAndKeepMaxSize, timed}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object Day4 {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day4.txt").getLines().toList
    timed("Part 1", part1(lines))
    timed("Part 2", part2(lines))
  }

  def parseRanges(line: String): (Range, Range) = {
    val Array(r1, r2) = line
      .split(',')
      .map { rangeString =>
        val Array(start, end) = rangeString.split('-').map(_.toInt)
        start to end
      }
      .sortBy(r => (r.start, -r.end))

    (r1, r2)
  }

  def fullyOverlap(r1: Range, r2: Range): Boolean = {
    r1.start <= r2.start && r1.end >= r2.end
  }

  def overlap(r1: Range, r2: Range): Boolean = {
    r1.end >= r2.start
  }

  def part1(lines: List[String]): Int = {
    lines.map(parseRanges).count(fullyOverlap)
  }

  def part2(lines: List[String]): Int = {
    lines.map(parseRanges).count(overlap)
  }
}
