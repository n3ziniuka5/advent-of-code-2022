package aoc

import aoc.Common.timed

import scala.collection.mutable
import scala.io.Source
import scala.annotation.tailrec

object Day1:
    def main(args: Array[String]): Unit =
        val lines = Source.fromResource("day1.txt").getLines().toList
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    def part1(lines: List[String]): Long =
        val firstList  = lines.map(line => line.split("\\s+")(0).toLong).sorted
        val secondList = lines.map(line => line.split("\\s+")(1).toLong).sorted
        firstList.zip(secondList).map((a, b) => math.abs(a - b)).sum

    def part2(lines: List[String]): Long =
        val firstList  = lines.map(line => line.split("\\s+")(0).toLong).sorted
        val secondList = lines.map(line => line.split("\\s+")(1).toLong).sorted

        val rightListCount = secondList.groupBy(identity).mapValues(_.length)
        firstList.map(a => rightListCount.getOrElse(a, 0) * a).sum
