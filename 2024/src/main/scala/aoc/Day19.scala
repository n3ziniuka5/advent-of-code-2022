package aoc

import aoc.Common.timed
import language.experimental.namedTuples

object Day19:
    def main(args: Array[String]): Unit =
        val lines = InputUtils.fetchInput(2024, 19)
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    def part1(lines: List[String]): Long =
        val (available, desired) = parse(lines)
        desired.map(waysToBuild(available, _)).count(_ > 0)

    def part2(lines: List[String]): Long =
        val (available, desired) = parse(lines)
        desired.map(waysToBuild(available, _)).sum

    val cache = collection.mutable.Map("" -> 1L)

    def waysToBuild(available: List[String], desired: String): Long =
        if cache.contains(desired) then cache(desired)
        else
            val waysToBuildForAllPrefixes = available
                .filter(desired.startsWith)
                .map: prefix =>
                    waysToBuild(available, desired.stripPrefix(prefix))
                .sum
            cache.addOne(desired -> waysToBuildForAllPrefixes)
            waysToBuildForAllPrefixes

    def parse(lines: List[String]): (available: List[String], desired: List[String]) =
        val available = lines.head.split(", ").toList
        val desired   = lines.drop(2)
        (available, desired)
