package aoc

import aoc.Common.timed

object Day8:
    def main(args: Array[String]): Unit =
        val lines = InputUtils.fetchInput(2024, 8)
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    def part1(lines: List[String]): Long =
        val map = Map2d.fromLines(lines)
        println(map)
        val frequencies = map.underlying.groupBy(_._2).view.mapValues(_.keySet).toMap.removed('.')
        println(frequencies)
        // println(frequencies.view.mapValues(_.size).toList.sortBy(_._2).reverse)
        val antinodes = frequencies
            .flatMap: (_, points) =>
                points.toList
                    .combinations(2)
                    .toSet
                    .flatMap: comb =>
                        val List(p1, p2) = comb
                        println(s"comparing $p1 and $p2")
                        val distance = p1.manhattanDistance(p2)

                        val perimeter = for
                            x <- -distance to distance
                            y <- -distance to distance
                            if math.abs(x) + math.abs(y) == distance
                        yield List(Point(p1.x + x, p1.y + y), Point(p2.x + x, p2.y + y))

                        perimeter.flatten
                            .filter(_.inBounds(map))
                            .filter: p =>
                                (p.manhattanDistance(p1) == distance && p.manhattanDistance(p2) == distance * 2) ||
                                    (p.manhattanDistance(p2) == distance && p.manhattanDistance(p1) == distance * 2)
                            .filter: p =>
                                ((math.abs(p.x - p1.x) == math.abs(p1.x - p2.x)) && (math
                                    .abs(p.y - p1.y) == math.abs(p1.y - p2.y))) || (math
                                    .abs(p.x - p2.x) == math.abs(p1.x - p2.x)) && (math
                                    .abs(p.y - p2.y) == math.abs(p1.y - p2.y))
                            .toSet

        val updatedMap = antinodes.foldLeft(map.underlying): (map, p) =>
            map.updated(p, '#')
        println(Map2d(updatedMap))
        println(antinodes)
        antinodes.toSet.size

    def part2(lines: List[String]): Long =
        val map = Map2d.fromLines(lines)
        println(map)
        val frequencies = map.underlying.groupBy(_._2).view.mapValues(_.keySet).toMap.removed('.')
        println(frequencies)
        // println(frequencies.view.mapValues(_.size).toList.sortBy(_._2).reverse)
        val antinodes = frequencies
            .flatMap: (_, points) =>
                points.toList
                    .combinations(2)
                    .toSet
                    .flatMap: comb =>
                        val List(p1, p2) = comb
                        println(s"comparing $p1 and $p2")

                        val dx1 = p1.x - p2.x
                        val dy1 = p1.y - p2.y

                        val dx2 = p2.x - p1.x
                        val dy2 = p2.y - p1.y

                        def findPoints(start: Point, dx: Int, dy: Int, results: List[Point]): List[Point] =
                            val next = Point(start.x + dx, start.y + dy)
                            if next.inBounds(map) then findPoints(next, dx, dy, next +: results)
                            else results

                        val allPoints = findPoints(p1, dx1, dy1, Nil) ++ findPoints(p2, dx2, dy2, Nil)

                        allPoints.filter(_.inBounds(map)).filter(p => map.get(p).contains('.')).toSet

        /* val distance = p1.manhattanDistance(p2)

                        val perimeter = for
                            x <- -distance to distance
                            y <- -distance to distance
                            if math.abs(x) + math.abs(y) == distance
                        yield List(Point(p1.x + x, p1.y + y), Point(p2.x + x, p2.y + y))

                        perimeter.flatten
                            .filter(_.inBounds(map))
                            .filter: p =>
                                (p.manhattanDistance(p1) == distance && p.manhattanDistance(p2) == distance * 2) ||
                                    (p.manhattanDistance(p2) == distance && p.manhattanDistance(p1) == distance * 2)
                            .filter: p =>
                                ((math.abs(p.x - p1.x) == math.abs(p1.x - p2.x)) && (math
                                    .abs(p.y - p1.y) == math.abs(p1.y - p2.y))) || (math
                                    .abs(p.x - p2.x) == math.abs(p1.x - p2.x)) && (math
                                    .abs(p.y - p2.y) == math.abs(p1.y - p2.y))
                            .toSet */

        val updatedMap = antinodes.foldLeft(map.underlying): (map, p) =>
            map.updated(p, '#')
        println(Map2d(updatedMap))
        println(antinodes)
        val antinodesSize = antinodes.toSet.size
        val antenas       = frequencies.view.mapValues(_.size).toList.filter(_._2 > 1).map(_._2).sum
        antinodesSize + antenas
