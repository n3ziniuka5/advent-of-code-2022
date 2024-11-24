package aoc

import aoc.Common.timed

import scala.annotation.tailrec
import scala.io.Source

object Day10:
  def main(args: Array[String]): Unit =
    val lines = Source.fromResource("day10.txt").getLines().toList
    timed("Part 1", part1(lines))
    timed("Part 2", part2(lines))

  @tailrec
  def cycle[A](lines: List[String], startCycle: Int, x: Int, acc: A, f: (Range, Int, A) => A): A =
    lines match
      case head :: tail =>
        val (newCycle, newX) =
          if (head == "noop") (startCycle + 1, x)
          else (startCycle + 2, x + head.drop(5).toInt)

        val newAcc = f(startCycle until newCycle, x, acc)
        cycle(tail, newCycle, newX, newAcc, f)

      case Nil => acc

  def part1(lines: List[String]): Int =
    cycle(
      lines,
      1,
      1,
      List.empty[Int],
      { (cycles, x, signalStrengths) =>
        val addedSignalStrengths = cycles.toList.filter(c => c % 40 == 20).map(c => c * x)
        addedSignalStrengths ++ signalStrengths
      }
    ).sum

  def part2(lines: List[String]): String =
    val crtLines = cycle(
      lines,
      1,
      1,
      Vector.fill(6)(Vector.fill(40)('!')),
      { (cycles, x, crtLines) =>
        cycles.foldLeft(crtLines) { (crtLines, c) =>
          val row    = (c - 1) / 40
          val col    = (c - 1) % 40
          val toDraw = if (((x - 1) to (x + 1)).contains(col)) '#' else ' '
          crtLines.updated(row, crtLines(row).updated(col, toDraw))
        }
      }
    )
    "\n" + crtLines.map(_.mkString).mkString("\n")
