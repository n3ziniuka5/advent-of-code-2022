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

    def part1(lines: List[String]): Int =
        0

    def part2(lines: List[String]): Int =
        0
