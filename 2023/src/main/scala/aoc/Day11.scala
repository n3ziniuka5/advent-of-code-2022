package aoc

import aoc.Common.timed

import scala.io.Source

object Day11:
    def main(args: Array[String]): Unit =
        val lines = Source.fromResource("day11.txt").getLines().toList
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    case class Point(x: Long, y: Long)

    def parse(lines: List[String]): (Map[Point, Char], List[(Point, Point)]) =
        val map = lines.zipWithIndex
            .flatMap { (line, y) =>
                line.zipWithIndex.map { (c, x) =>
                    Point(x, y) -> c
                }
            }
            .toMap
            .filter(_._2 == '#')
        val pairs = map.keySet.toList.combinations(2).map(p => (p.head, p(1))).toList
        (map, pairs)

    def solve(map: Map[Point, Char], pairs: List[(Point, Point)], galaxySize: Int): Long =
        val maxX = map.maxBy(_._1.x)._1.x
        val maxy = map.maxBy(_._1.y)._1.y

        val emptyColumns = (0L to maxX).filter { x =>
            (0L to maxy).forall { y =>
                !map.contains(Point(x, y))
            }
        }
        val emptyRows = (0L to maxy).filter { y =>
            (0L to maxX).forall { x =>
                !map.contains(Point(x, y))
            }
        }

        pairs
            .map(p => (p.head, p(1)))
            .map { case (p1, p2) =>
                val numEmptyCols   = (math.min(p1.x, p2.x) to math.max(p1.x, p2.x)).count(emptyColumns.contains)
                val numEmptyRows   = (math.min(p1.y, p2.y) to math.max(p1.y, p2.y)).count(emptyRows.contains)
                val totalExpansion = (numEmptyCols + numEmptyRows) * (galaxySize - 1)

                val distX = math.abs(p1.x - p2.x)
                val distY = math.abs(p1.y - p2.y)
                distX + distY + totalExpansion
            }
            .sum

    def part1(lines: List[String]): Long =
        val (map, pairs) = parse(lines)
        solve(map, pairs, 2)

    def part2(lines: List[String]): Long =
        val (map, pairs) = parse(lines)
        solve(map, pairs, 1000000)
