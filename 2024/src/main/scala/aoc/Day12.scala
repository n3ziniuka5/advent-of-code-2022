package aoc

import aoc.Common.timed
import scala.annotation.tailrec

object Day12:
    def main(args: Array[String]): Unit =
        val lines = InputUtils.fetchInput(2024, 12)
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    case class Region(area: Long, perimeter: Long, sides: Long)

    def part1(lines: List[String]): Long =
        val map = Map2d.fromLines(lines)
        findRegions(map, map.underlying.toList, Nil, Set.empty).map((_, r) => r.area * r.perimeter).sum

    def part2(lines: List[String]): Long =
        val map = Map2d.fromLines(lines)
        findRegions(map, map.underlying.toList.sortBy((p, _) => (p.x, p.y)), Nil, Set.empty)
            .map((_, r) => r.area * r.sides)
            .sum

    @tailrec
    def findRegions(
        map: Map2d[Char],
        remainingPoints: List[(Point, Char)],
        results: List[(Char, Region)],
        visited: Set[Point]
    ): List[(Char, Region)] =
        remainingPoints match
            case (point, plant) :: tail =>
                if visited.contains(point) then findRegions(map, tail, results, visited)
                else
                    val (region, addedVisited) = searchRegion(map, point, plant)
                    findRegions(map, tail, (plant, region) +: results, visited ++ addedVisited)
            case Nil => results

    enum Direction:
        case Up, Down, Left, Right

    def searchRegion(map: Map2d[Char], startAt: Point, plant: Char): (Region, Set[Point]) =
        @tailrec
        def loop(
            perimeterSearches: Vector[Point],
            nonPerimeterSearches: Vector[Point],
            visited: Set[Point],
            area: Int,
            perimeter: Int,
            horizontalSides: Set[(Direction, Point)],
            verticalSides: Set[(Direction, Point)],
            totalSides: Int
        ): (Region, Set[Point]) =
            val searches = if perimeterSearches.isEmpty then nonPerimeterSearches else perimeterSearches
            searches.headOption match
                case Some(current) =>
                    if visited.contains(current) then
                        val newPerimeterSearches =
                            if perimeterSearches.nonEmpty then perimeterSearches.tail else perimeterSearches
                        val newNonPerimeterSearches =
                            if perimeterSearches.isEmpty then nonPerimeterSearches.tail else nonPerimeterSearches
                        loop(
                          newPerimeterSearches,
                          newNonPerimeterSearches,
                          visited,
                          area,
                          perimeter,
                          horizontalSides,
                          verticalSides,
                          totalSides
                        )
                    else
                        val adjacentSamePlant = current.adjacent.filter(p => map.get(p).contains(plant))
                        val plantPerimeter    = 4 - adjacentSamePlant.size
                        val toVisitNext       = adjacentSamePlant.filter(p => !visited.contains(p))

                        val (addTop, topSide) =
                            if !map.get(current.up).contains(plant) then
                                val toAdd =
                                    if horizontalSides.contains((Direction.Up, current.up.left)) || horizontalSides
                                            .contains((Direction.Up, current.up.right))
                                    then 0
                                    else 1

                                (toAdd, Set((Direction.Up, current.up)))
                            else (0, Set.empty)

                        val (addBottom, bottomSide) =
                            if !map.get(current.down).contains(plant) then
                                val toAdd =
                                    if horizontalSides.contains((Direction.Down, current.down.left)) || horizontalSides
                                            .contains((Direction.Down, current.down.right))
                                    then 0
                                    else 1

                                (toAdd, Set((Direction.Down, current.down)))
                            else (0, Set.empty)

                        val (addLeft, leftSide) =
                            if !map.get(current.left).contains(plant) then
                                val toAdd =
                                    if verticalSides.contains((Direction.Left, current.left.up)) || verticalSides
                                            .contains((Direction.Left, current.left.down))
                                    then 0
                                    else 1

                                (toAdd, Set((Direction.Left, current.left)))
                            else (0, Set.empty)

                        val (addRight, rightSide) =
                            if !map.get(current.right).contains(plant) then
                                val toAdd =
                                    if verticalSides.contains((Direction.Right, current.right.up)) || verticalSides
                                            .contains((Direction.Right, current.right.down))
                                    then 0
                                    else 1
                                (toAdd, Set((Direction.Right, current.right)))
                            else (0, Set.empty)

                        val newPerimeterSearches =
                            if plantPerimeter > 0 then perimeterSearches ++ toVisitNext else perimeterSearches
                        val newNonPerimeterSearches =
                            if plantPerimeter == 0 then nonPerimeterSearches ++ toVisitNext else nonPerimeterSearches

                        loop(
                          newPerimeterSearches,
                          newNonPerimeterSearches,
                          visited + current,
                          area + 1,
                          perimeter + plantPerimeter,
                          horizontalSides ++ topSide ++ bottomSide,
                          verticalSides ++ leftSide ++ rightSide,
                          totalSides + addTop + addBottom + addLeft + addRight
                        )
                case None => (Region(area, perimeter, totalSides), visited)

        loop(Vector(startAt), Vector.empty, Set.empty, 0, 0, Set.empty, Set.empty, 0)
