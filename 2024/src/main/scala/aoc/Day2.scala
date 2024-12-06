package aoc

import aoc.Common.timed

object Day2:
    def main(args: Array[String]): Unit =
        val lines = InputUtils.fetchInput(2024, 2)
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    def part1(lines: List[String]): Long =
        lines.map(parseLine).count(lineSafe)

    def part2(lines: List[String]): Long =
        lines.map(parseLine).count(safeWithDampener)

    def lineSafe(line: Vector[Int]): Boolean =
        val sortedAsc  = line.sorted
        val sortedDesc = line.sorted(Ordering[Int].reverse)
        val gradual    = line == sortedAsc || line == sortedDesc
        gradual && line
            .sliding(2)
            .forall: pair =>
                val a    = pair(0)
                val b    = pair(1)
                val diff = math.abs(a - b)
                diff >= 1 && diff <= 3

    def safeWithDampener(line: Vector[Int]): Boolean =
        line.indices.exists: removeNth =>
            val withoutNth = line.patch(removeNth, Nil, 1)
            lineSafe(withoutNth)

    def parseLine(line: String): Vector[Int] =
        line.split(" ").map(_.toInt).toVector
