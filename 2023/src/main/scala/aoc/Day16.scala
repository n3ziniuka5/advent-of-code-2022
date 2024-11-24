package aoc

import aoc.Common.timed

import scala.io.Source
import scala.collection.parallel.CollectionConverters._

object Day16:
    def main(args: Array[String]): Unit =
        val lines = Source.fromResource("day16.txt").getLines().toList
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    def part1(lines: List[String]): Long =
        val map = Map2d.fromLines(lines)
        energized(List((Point(0, 0), Point(-1, 0))), Set.empty, map)

    def part2(lines: List[String]): Long =
        val map = Map2d.fromLines(lines)

        val downwards = (0 to map.maxX).map { x =>
            val point = Point(x, 0)
            List((point, point.up))
        }
        val upwards = (0 to map.maxX).map { x =>
            val point = Point(x, map.maxY)
            List((point, point.down))
        }

        val fromLeft = (0 to map.maxY).map { y =>
            val point = Point(0, y)
            List((point, point.left))
        }

        val fromRight = (0 to map.maxY).map { y =>
            val point = Point(map.maxX, y)
            List((point, point.right))
        }

        val allStarts = downwards ++ upwards ++ fromLeft ++ fromRight
        allStarts.par.map { start =>
            energized(start, Set.empty, map)
        }.max

    def energized(
        search: List[(Point, Point)],
        visited: Set[(Point, Point)],
        map: Map2d[Char]
    ): Int =
        if (search.isEmpty) visited.map(_._1).size
        else if (visited.contains(search.head) || visited.contains(search.head.swap))
            energized(search.tail, visited, map)
        else
            val (current, previous) = search.head
            val newSearches = map(current) match
                case '.' =>
                    if (previous == current.up) List((current.down, current))
                    else if (previous == current.down) List((current.up, current))
                    else if (previous == current.left) List((current.right, current))
                    else List((current.left, current))
                case '|' =>
                    if (previous == current.up) List((current.down, current))
                    else if (previous == current.down) List((current.up, current))
                    else
                        List((current.up, current), (current.down, current))
                case '-' =>
                    if (previous == current.left) List((current.right, current))
                    else if (previous == current.right) List((current.left, current))
                    else
                        List((current.left, current), (current.right, current))
                case '\\' =>
                    if (previous == current.up) List((current.right, current))
                    else if (previous == current.down) List((current.left, current))
                    else if (previous == current.right) List((current.up, current))
                    else List((current.down, current))
                case '/' =>
                    if (previous == current.up) List((current.left, current))
                    else if (previous == current.down) List((current.right, current))
                    else if (previous == current.left) List((current.up, current))
                    else List((current.down, current))

            energized(
              newSearches.filter(_._1.inBounds(map)) ++ search.tail,
              visited + search.head,
              map
            )
