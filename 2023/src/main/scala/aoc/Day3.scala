package aoc

import aoc.Common.timed

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object Day3:
    case class Pos(x: Int, y: Int):
        def neighbors: List[Pos] = (for {
            dX <- -1 to 1
            dY <- -1 to 1
            if dX != 0 || dY != 0
        } yield Pos(x + dX, y + dY)).toList

    case class Data(raw: Map[Pos, Char], numbers: List[(Int, Set[Pos])])
    object Data:
        val empty = Data(Map.empty, Nil)

    def main(args: Array[String]): Unit =
        val lines = Source.fromResource("day3.txt").getLines().toList
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    def parse(lines: List[String]): Data =
        @tailrec
        def parseLine(line: String, x: Int, y: Int, data: Data): Data =
            if (line.isEmpty) data
            else if (line.head.isDigit)
                val numberString    = line.takeWhile(_.isDigit)
                val numberPositions = numberString.zipWithIndex.map { case (c, dX) => Pos(x + dX, y) -> c }.toList
                val newData = data.copy(
                  raw = data.raw ++ numberPositions.toMap,
                  numbers = (numberString.toInt, numberPositions.map(_._1).toSet) +: data.numbers
                )
                parseLine(line.drop(numberString.length), x + numberString.length, y, newData)
            else if (line.head != '.')
                parseLine(line.tail, x + 1, y, data.copy(raw = data.raw + (Pos(x, y) -> line.head)))
            else
                parseLine(line.tail, x + 1, y, data)

        lines.zipWithIndex.foldLeft(Data.empty) { case (data, (line, y)) =>
            val parsed = parseLine(line, 0, y, Data.empty)
            Data(data.raw ++ parsed.raw, parsed.numbers ++ data.numbers)
        }

    def part1(lines: List[String]): Int =
        val data = parse(lines)
        data.numbers
            .filter { case (_, positions) =>
                positions.flatMap(_.neighbors).exists { pos =>
                    !data.raw.getOrElse(pos, '0').isDigit
                }
            }
            .map(_._1)
            .sum

    def part2(lines: List[String]): Int =
        val data = parse(lines)
        val gears = data.numbers.foldLeft(Map.empty[Pos, List[Int]]) { case (gears, (num, positions)) =>
            val newGears = positions.flatMap(_.neighbors).flatMap { pos =>
                if (data.raw.getOrElse(pos, '0') == '*') Some(pos)
                else None
            }

            newGears.foldLeft(gears) { case (gears, pos) =>
                gears + (pos -> (num +: gears.getOrElse(pos, Nil)))
            }
        }
        gears.collect { case (_, nums) if nums.sizeIs == 2 => nums.product }.sum
