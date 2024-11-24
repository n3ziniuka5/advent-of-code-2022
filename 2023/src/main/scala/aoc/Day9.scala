package aoc

import aoc.Common.timed

import scala.io.Source

object Day9:
    def main(args: Array[String]): Unit =
        val lines = Source.fromResource("day9.txt").getLines().toList
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    def parse(lines: List[String]): List[Vector[Long]] =
        lines.map(_.split(" ").map(_.toLong).toVector)

    extension (seq: Vector[Long])
        def diffs: Vector[Long] =
            seq.sliding(2, 1).map { a => a(1) - a(0) }.toVector

    def part1(lines: List[String]): Long =
        def solve(seq: Vector[Long]): Long =
            if (seq.forall(_ == 0)) 0
            else seq.last + solve(seq.diffs)

        parse(lines).map(solve).sum

    def part2(lines: List[String]): Long =
        def solve(seq: Vector[Long]): Long =
            if (seq.forall(_ == 0)) 0
            else seq.head - solve(seq.diffs)

        parse(lines).map(solve).sum
