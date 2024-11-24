package aoc

import aoc.Common.timed

import scala.io.Source

object Day6:
    def main(args: Array[String]): Unit =
        val lines = Source.fromResource("day6.txt").getLines().toList
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    def waysToWin(time: Long, distance: Long): Long =
        val targetDistance = distance + 1
        val discriminant   = time * time - 4 * targetDistance
        val start          = ((time - math.sqrt(discriminant)) / 2).ceil.toLong

        time - start * 2 + 1

    def part1(lines: List[String]): Long =
        val time        = lines.head.drop("Time:".length).trim.split(" ").flatMap(_.trim.toIntOption).toList
        val distance    = lines(1).drop("Distance:".length).trim.split(" ").flatMap(_.trim.toIntOption).toList
        val timeAndDist = time.zip(distance)

        timeAndDist.map { case (time, dist) =>
            waysToWin(time, dist)
        }.product

    def part2(lines: List[String]): Long =
        val time     = lines.head.drop("Time:".length).replace(" ", "").toLong
        val distance = lines(1).drop("Distance:".length).replace(" ", "").toLong

        waysToWin(time, distance)
