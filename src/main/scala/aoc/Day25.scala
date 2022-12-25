package aoc

import aoc.Common.timed

import scala.annotation.tailrec
import scala.io.Source

object Day25:
  def main(args: Array[String]): Unit =
    val lines = Source.fromResource("day25.txt").getLines().toList
    timed("Part 1", part1(lines))

  def snafuToDec(str: String): Long =
    val positives = str.map {
      case '-'   => '0'
      case '='   => '0'
      case other => other
    }

    val negatives = str.map {
      case '-' => '1'
      case '=' => '2'
      case _   => '0'
    }

    java.lang.Long.parseLong(positives, 5) - java.lang.Long.parseLong(negatives, 5)

  def decToSnafu(int: Long): String =
    val initial = java.lang.Long.toString(int, 5)

    @tailrec
    def add(str: String, i: Int): String =
      if (i >= str.length) str + "1"
      else
        str(i) match
          case '0' => str.updated(i, '1')
          case '1' => str.updated(i, '2')
          case '2' => str.updated(i, '3')
          case '3' => str.updated(i, '4')
          case '4' => add(str.updated(i, '0'), i + 1)

    @tailrec
    def loop(i: Int, acc: String): String =
      if (i >= acc.length) acc.reverse
      else
        val newAcc = acc(i) match
          case ('0' | '1' | '2') => acc
          case '3'               => add(acc.updated(i, '='), i + 1)
          case '4'               => add(acc.updated(i, '-'), i + 1)

        loop(i + 1, newAcc)

    loop(0, initial.reverse)

  def part1(lines: List[String]): String =
    decToSnafu(lines.map(snafuToDec).sum)
