package aoc

import aoc.Common.timed
import scala.annotation.tailrec

object Day8:
    def main(args: Array[String]): Unit =
        val lines = InputUtils.fetchInput(2024, 8)
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    def part1(lines: List[String]): Long =
        findAntinodes(Map2d.fromLines(lines), restrictDistance = true).size

    def part2(lines: List[String]): Long =
        findAntinodes(Map2d.fromLines(lines), restrictDistance = false).size

    def findAntinodes(map: Map2d[Char], restrictDistance: Boolean): Set[Point] =
        val frequencyMap = map.underlying.groupBy(_._2).removed('.').view.mapValues(_.keys.toList).toMap
        frequencyMap.toSet
            .flatMap: (_, points) =>
                points
                    .combinations(2)
                    .toSet
                    .flatMap: comb =>
                        val List(p1, p2) = comb
                        antinodesBetweenPoints(p1, p2, map, restrictDistance)

    def antinodesBetweenPoints(p1: Point, p2: Point, map: Map2d[Char], restrictDistance: Boolean): Set[Point] =
        val dX = p1.x - p2.x
        val dY = p1.y - p2.y

        @tailrec
        def directSightPoints(start: Point, dx: Int, dy: Int, results: List[Point]): Set[Point] =
            val next = Point(start.x + dx, start.y + dy)
            if next.inBounds(map) then
                if restrictDistance then Set(next)
                else directSightPoints(next, dx, dy, next +: results)
            else results.toSet

        val antinodes = directSightPoints(p1, dX, dY, Nil) ++ directSightPoints(p2, -dX, -dY, Nil)

        if restrictDistance then antinodes
        else antinodes + p1 + p2
