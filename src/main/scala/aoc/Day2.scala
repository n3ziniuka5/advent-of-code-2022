package aoc

import aoc.Common.timed
import aoc.Day2.Shape.{PAPER, ROCK, SCISSORS}

import scala.io.Source
import scala.math.floorMod

object Day2 {
  enum Shape {
    case ROCK, PAPER, SCISSORS
  }

  val toShape = Map(
    "A" -> ROCK,
    "B" -> PAPER,
    "C" -> SCISSORS,
    "X" -> ROCK,
    "Y" -> PAPER,
    "Z" -> SCISSORS
  )

  val toPoints = Map(
    ROCK     -> 0,
    PAPER    -> 1,
    SCISSORS -> 2
  )

  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day2.txt").getLines().toList
    timed("Part 1", part1(lines))
    timed("Part 2", part2(lines))
  }

  def part1(lines: List[String]): Int = {
    def lineToPoints(line: String): Int = {
      val Array(opponent, you) = line.split(' ').map(toShape)
      val victoryScore = (opponent, you) match {
        case (ROCK, PAPER)        => 6
        case (PAPER, SCISSORS)    => 6
        case (SCISSORS, ROCK)     => 6
        case _ if opponent == you => 3
        case _                    => 0
      }

      victoryScore + toPoints(you) + 1
    }

    lines.map(lineToPoints).sum
  }

  def part2(lines: List[String]): Int = {
    def lineToPoints(line: String): Int = {
      val Array(opponent, you) = line.split(' ')
      val opponentPoints       = toPoints(toShape(opponent))

      val roundScore = you match {
        case "X" => floorMod(opponentPoints - 1, 3)
        case "Y" => 3 + opponentPoints
        case "Z" => 6 + floorMod(opponentPoints + 1, 3)
      }

      roundScore + 1
    }

    lines.map(lineToPoints).sum
  }

}
