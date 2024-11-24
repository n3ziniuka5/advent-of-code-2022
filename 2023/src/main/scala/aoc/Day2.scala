package aoc

import aoc.Common.timed

import scala.io.Source

object Day2:
    def main(args: Array[String]): Unit =
        val lines = Source.fromResource("day2.txt").getLines().toList
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    private def parse(lines: List[String]): List[(Int, List[(String, Int)])] =
        lines.map { case s"Game $game: $drawLine" =>
            val draws = drawLine.split(";").toList.map(_.trim).flatMap { draw =>
                draw.split(",").toList.map(_.trim).map { case s"$number $color" =>
                    (color, number.toInt)
                }
            }
            (game.toInt, draws)
        }

    def part1(lines: List[String]): Int =
        parse(lines)
            .filter { (game, draws) =>
                draws.forall { (color, number) =>
                    color match {
                        case "red"   => number <= 12
                        case "green" => number <= 13
                        case "blue"  => number <= 14
                    }
                }
            }
            .map(_._1)
            .sum

    def part2(lines: List[String]): Int =
        parse(lines).map { (game, draws) =>
            def findMax(color: String): Int =
                draws
                    .collect { case (`color`, n) => n }
                    .maxOption
                    .getOrElse(0)

            val red   = findMax("red")
            val green = findMax("green")
            val blue  = findMax("blue")

            red * green * blue
        }.sum
