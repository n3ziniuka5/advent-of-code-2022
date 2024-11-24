package aoc

import aoc.Common.timed

import scala.collection.parallel.CollectionConverters.*
import scala.io.Source
import collection.mutable.PriorityQueue
import scala.annotation.tailrec
import scala.collection.mutable

object Day17:
    def main(args: Array[String]): Unit =
        val lines = Source.fromResource("day17.txt").getLines().toList
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    def part1(lines: List[String]): Long =
        solve(parse(lines), 3, 1)

    def part2(lines: List[String]): Long =
        solve(parse(lines), 10, 4)

    def parse(lines: List[String]): Map2d[Int] =
        Map2d.fromLines(lines).map { case (k, v) =>
            k -> v.toString.toInt
        }

    case class SearchState(
        currentPos: Point,
        previousPos: Point,
        movesInSameDirection: Int,
        totalHeatLoss: Long,
        targetX: Int,
        targetY: Int
    ):
        def toKey = SearchStateKey(currentPos, previousPos)

    object SearchState:
        given Ordering[SearchState] = Ordering.by { s =>
            val bestCaseDistance =
                Math.abs(s.currentPos.x - s.targetX) + Math.abs(s.currentPos.y - s.targetY) + s.totalHeatLoss
            (-bestCaseDistance, -s.totalHeatLoss)
        }

    case class SearchStateKey(
        currentPos: Point,
        previousPos: Point
    )

    def solve(map: Map2d[Int], maxMovesInSameDirection: Int, minMovesOnTurn: Int): Long =
        @tailrec
        def loop(
            searches: PriorityQueue[SearchState],
            map: Map2d[Int],
            visited: Map[SearchStateKey, (Int, Long)]
        ): Long =
            def alreadySeen(search: SearchState) =
                visited.get(search.toKey).exists { case (existingSteps, existingHeat) =>
                    existingSteps <= search.movesInSameDirection && existingHeat <= search.totalHeatLoss
                }

            val head = searches.dequeue()

            if (head.currentPos.x == map.maxX && head.currentPos.y == map.maxY) head.totalHeatLoss
            else if (alreadySeen(head)) loop(searches, map, visited)
            else
                val (sameDirection: (Point => Point), turns: List[Point => Point]) =
                    if (head.currentPos.up == head.previousPos)
                        ((p: Point) => p.down, List((p: Point) => p.left, (p: Point) => p.right))
                    else if (head.currentPos.down == head.previousPos)
                        ((p: Point) => p.up, List((p: Point) => p.left, (p: Point) => p.right))
                    else if (head.currentPos.left == head.previousPos)
                        ((p: Point) => p.right, List((p: Point) => p.up, (p: Point) => p.down))
                    else if (head.currentPos.right == head.previousPos)
                        ((p: Point) => p.left, List((p: Point) => p.up, (p: Point) => p.down))
                    else throw new RuntimeException("Invalid state")

                val sameDirectionSearch =
                    if (head.movesInSameDirection < maxMovesInSameDirection)
                        List(
                          head.copy(
                            sameDirection(head.currentPos),
                            head.currentPos,
                            head.movesInSameDirection + 1,
                            head.totalHeatLoss + map.get(sameDirection(head.currentPos)).getOrElse(0)
                          )
                        )
                    else Nil

                val turnSearches = turns.map { turn =>
                    head.copy(
                      (1 to minMovesOnTurn).foldLeft(head.currentPos)((p, _) => turn(p)),
                      (1 until minMovesOnTurn).foldLeft(head.currentPos)((p, _) => turn(p)),
                      minMovesOnTurn,
                      head.totalHeatLoss + (1 to minMovesOnTurn)
                          .scanLeft(head.currentPos)((p, _) => turn(p))
                          .tail // scanLeft includes the starting point
                          .map(map.get(_).getOrElse(0))
                          .sum
                    )
                }
                val newSearches = sameDirectionSearch ++ turnSearches
                newSearches.filter(_.currentPos.inBounds(map)).filterNot(alreadySeen).foreach(searches.enqueue(_))
                loop(searches, map, visited + (head.toKey -> (head.movesInSameDirection, head.totalHeatLoss)))

        val topLeft = Point(0, 0)
        val startingPoints = mutable.PriorityQueue[SearchState](
          SearchState(topLeft.right, topLeft, 1, map(topLeft.right), map.maxX, map.maxY),
          SearchState(topLeft.down, topLeft, 1, map(topLeft.down), map.maxX, map.maxY),
        )
        loop(startingPoints, map, Map.empty)
