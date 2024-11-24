package aoc

import aoc.Common.timed

import scala.io.Source

object Day13:
    def main(args: Array[String]): Unit =
        val lines = Source.fromResource("day13.txt").getLines().toList
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    case class Point(x: Int, y: Int)
    enum ReflectionType:
        case Vertical, Horizontal

    def part1(lines: List[String]): Long =
        parse(lines, Nil).map(summarizeMap(_, 0)).sum

    def part2(lines: List[String]): Long =
        parse(lines, Nil).map(summarizeMap(_, 1)).sum

    def parse(lines: List[String], maps: List[Map[Point, Char]]): List[Map[Point, Char]] =
        val nonEmpty = lines.dropWhile(_.isEmpty).takeWhile(_.nonEmpty)
        if (nonEmpty.isEmpty) maps.reverse
        else
            val newRes = nonEmpty.zipWithIndex.flatMap { (line, y) =>
                line.zipWithIndex.map { (char, x) =>
                    Point(x, y) -> char
                }
            }.toMap
            parse(lines.drop(nonEmpty.length + 1), newRes +: maps)

    def summarizeMap(map: Map[Point, Char], reflectionTolerance: Int): Int =
        val maxX = map.keys.maxBy(_.x).x
        val maxY = map.keys.maxBy(_.y).y

        findReflection(map, reflectionTolerance, ReflectionType.Vertical).orElse {
            findReflection(map, reflectionTolerance, ReflectionType.Horizontal).map {
                _ * 100
            }
        }.get

    def findReflection(map: Map[Point, Char], reflectionTolerance: Int, reflectionType: ReflectionType): Option[Int] =
        val maxX = map.keys.maxBy(_.x).x
        val maxY = map.keys.maxBy(_.y).y

        val (lineUntil, oppositeLines, point) = reflectionType match
            case ReflectionType.Vertical   => (maxX, maxY, (a: Int, b: Int) => Point(a, b))
            case ReflectionType.Horizontal => (maxY, maxX, (a: Int, b: Int) => Point(b, a))

        (1 to lineUntil).find { mirrorAt =>
            val toTheLeft  = ((mirrorAt - 1) to 0 by -1).toList
            val toTheRight = (mirrorAt to lineUntil).toList

            val zipped = toTheLeft.zip(toTheRight)

            val nonMatch = (0 to oppositeLines).map { y =>
                zipped.count((left, right) => map(point(left, y)) != map(point(right, y)))
            }.sum
            nonMatch == reflectionTolerance
        }
