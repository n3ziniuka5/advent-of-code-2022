package aoc

import aoc.Common.timed

import scala.annotation.tailrec
import scala.io.Source

object Day10:
  def main(args: Array[String]): Unit =
    val lines = Source.fromResource("day10.txt").getLines().toList
    timed("Part 1", part1(lines))
    timed("Part 2", part2(lines))

  def part1(lines: List[String]): Int =
    @tailrec
    def loop(lines: List[String], startCycle: Int, x: Int, signalStrengths: List[Int]): List[Int] =
      lines match
        case head :: tail =>
          val (newCycle, newX) =
            if (head == "noop") (startCycle + 1, x)
            else
              val add = head.drop(5).toInt
              (startCycle + 2, x + add)

          val addedSignalStrengths = (startCycle until newCycle).toList.filter(c => c % 40 == 20).map(c => c * x)
          loop(tail, newCycle, newX, addedSignalStrengths ++ signalStrengths)

        case Nil => signalStrengths

    loop(lines, 1, 1, List.empty).sum

  def part2(lines: List[String]): String =
    @tailrec
    def loop(lines: List[String], startCycle: Int, x: Int, crtLines: Vector[Vector[Char]]): Vector[Vector[Char]] =
      lines match
        case head :: tail =>
          val (newCycle, newX) =
            if (head == "noop") (startCycle + 1, x)
            else
              val add = head.drop(5).toInt
              (startCycle + 2, x + add)

          val newCrt = (startCycle until newCycle).foldLeft(crtLines) { (acc, c) =>
            val row    = (c - 1) / 40
            val col    = (c - 1) % 40
            val toDraw = if (((x - 1) to (x + 1)).contains(col)) '#' else ' '
            acc.updated(row, acc(row).updated(col, toDraw))
          }

          loop(tail, newCycle, newX, newCrt)

        case Nil => crtLines

    val crtLines = loop(lines, 1, 1, Vector.fill(6)(Vector.fill(40)('!')))
    "\n" + crtLines.map(_.mkString).mkString("\n")
