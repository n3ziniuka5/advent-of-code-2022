package aoc

import aoc.Common.timed
import scala.collection.mutable.PriorityQueue
import scala.annotation.tailrec

object Day20:
    def main(args: Array[String]): Unit =
        val lines = InputUtils.fetchInput(2024, 20)
        timed("Part 1", part1(lines, 100))
        timed("Part 2", part2(lines, 100))

    def part1(lines: List[String], minTimeSaved: Long): Long =
        val (map, start, end) = parseInput(lines)
        numberOfCheats(shortestPath(map, start, end), minTimeSaved, 2)

    def part2(lines: List[String], minTimeSaved: Long): Long =
        val (map, start, end) = parseInput(lines)
        numberOfCheats(shortestPath(map, start, end), minTimeSaved, 20)

    def shortestPath(map: Map2d[Char], start: Point, end: Point): List[Point] =
        @tailrec
        def loop(searches: PriorityQueue[(Point, Long, List[Point])], visited: Set[Point]): List[Point] =
            val (current, distance, currentPath) = searches.dequeue()
            if current == end then (current +: currentPath).reverse
            else
                val newSearches = current.adjacent
                    .filter(p => !visited.contains(p) && map.get(p).contains('.'))
                    .map(p => (p, distance + 1, current +: currentPath))
                searches.enqueue(newSearches*)
                loop(searches, visited + current)

        loop(PriorityQueue((start, 0L, List.empty[Point]))(Ordering.by(-_._2)), Set.empty)

    def numberOfCheats(path: List[Point], minimalTimeSaved: Long, maxShortcutAllowed: Long): Long =
        @tailrec
        def loop(searchFrom: List[(Point, Int)], cheatCount: Long): Long =
            searchFrom match
                case (currentPoint, currentIndex) :: tail =>
                    val cheatDistances = tail
                        .map: (targetPoint, targetIndex) =>
                            val distance = currentPoint.manhattanDistance(targetPoint)
                            if distance <= maxShortcutAllowed then targetIndex - currentIndex - distance
                            else -1
                    val goodCheats = cheatDistances.count(_ >= minimalTimeSaved)

                    loop(tail, cheatCount + goodCheats)

                case Nil => cheatCount

        loop(path.zipWithIndex, 0L)

    def parseInput(lines: List[String]): (map: Map2d[Char], start: Point, end: Point) =
        val map   = Map2d.fromLines(lines)
        val start = map.underlying.find(_._2 == 'S').get._1
        val end   = map.underlying.find(_._2 == 'E').get._1
        (map.updated(end, '.'), start, end)
