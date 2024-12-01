package aoc

import aoc.Common.timed

import scala.collection.mutable
import scala.io.Source
import scala.annotation.tailrec

object Day1:
    def main(args: Array[String]): Unit =
        val lines = InputUtils.fetchInput(2024, 1)
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    def parseLines(lines: List[String]): List[List[Long]] =
        lines.map(line => line.split("\\s+").map(_.toLong)).transpose

    def part1(lines: List[String]): Long =
        val List(firstList, secondList) = parseLines(lines)
        firstList.sorted.zip(secondList.sorted).map((a, b) => math.abs(a - b)).sum

    def part2(lines: List[String]): Long =
        val List(firstList, secondList) = parseLines(lines)
        val rightListCount              = secondList.groupBy(identity).mapValues(_.length)
        firstList.map(a => rightListCount.getOrElse(a, 0) * a).sum
