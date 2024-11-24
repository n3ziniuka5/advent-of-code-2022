package aoc

import aoc.Common.timed
import aoc.Day2.Shape.{Paper, Rock, Scissors}

import scala.io.Source
import scala.math.floorMod

object Day2 {
  enum Shape {
    case Rock, Paper, Scissors
  }

  val toShape = Map(
    "A" -> Rock,
    "B" -> Paper,
    "C" -> Scissors,
    "X" -> Rock,
    "Y" -> Paper,
    "Z" -> Scissors
  )

  val toPoints = Map(
    Rock     -> 0,
    Paper    -> 1,
    Scissors -> 2
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
        case (Rock, Paper)        => 6
        case (Paper, Scissors)    => 6
        case (Scissors, Rock)     => 6
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
