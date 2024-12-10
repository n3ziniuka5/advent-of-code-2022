package aoc

import aoc.Common.timed
import scala.annotation.tailrec

object Day10:
    def main(args: Array[String]): Unit =
        val lines = InputUtils.fetchInput(2024, 10)
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    def part1(lines: List[String]): Long =
        val (map, trailHeads) = parseMap(lines)
        trailHeads.map(summitsReached(_, map)).map(_.toSet.size).sum

    def part2(lines: List[String]): Long =
        val (map, trailHeads) = parseMap(lines)
        trailHeads.map(summitsReached(_, map)).map(_.size).sum

    def parseMap(lines: List[String]): (Map2d[Char], List[Point]) =
        val map        = Map2d.fromLines(lines)
        val trailHeads = map.underlying.filter(_._2 == '0').map(_._1)
        (map, trailHeads.toList)

    def summitsReached(trailHead: Point, map: Map2d[Char]): List[Point] =
        @tailrec
        def loop(searches: List[(Point, Int)], summits: List[Point]): List[Point] =
            searches match
                case (point, height) :: tail =>
                    if height == 9 then loop(tail, point +: summits)
                    else
                        val newSearches =
                            point.adjacent
                                .filter(p => map.get(p).map(_.toString.toInt).contains(height + 1))
                                .map(p => (p, height + 1))
                        loop(newSearches ++ tail, summits)
                case Nil => summits

        loop(List((trailHead, 0)), Nil)
