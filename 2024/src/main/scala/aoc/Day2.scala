package aoc

import aoc.Common.timed

import scala.collection.mutable
import scala.io.Source
import scala.annotation.tailrec

object Day2:
    def main(args: Array[String]): Unit =
        val lines = InputUtils.fetchInput(2024, 2)
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    def part1(lines: List[String]): Long =
        lines.count(lineSafe)

    def part2(lines: List[String]): Long =
        lines.count(safeWithDampener)

    def lineSafe(line: String): Boolean =
        val originalNumbers = line.split(" ").map(_.toInt).toList
        val sortedAsc       = originalNumbers.sorted
        val sortedDesc      = originalNumbers.sorted(Ordering[Int].reverse)
        val gradual         = originalNumbers == sortedAsc || originalNumbers == sortedDesc
        gradual && originalNumbers
            .sliding(2)
            .forall: pair =>
                val a    = pair(0)
                val b    = pair(1)
                val diff = math.abs(a - b)
                diff >= 1 && diff <= 3

    def safeWithDampener(line: String): Boolean =
        val numberStrings = line.split(" ").toVector
        numberStrings.indices.exists: removeNth =>
            val withoutNth = numberStrings.zipWithIndex.filterNot((_, i) => i == removeNth)
            lineSafe(withoutNth.map(_._1).mkString(" "))
