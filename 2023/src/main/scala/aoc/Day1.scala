package aoc

import aoc.Common.timed

import scala.collection.mutable
import scala.io.Source
import scala.annotation.tailrec

object Day1:
    def main(args: Array[String]): Unit =
        val lines = Source.fromResource("day1.txt").getLines().toList
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    def part1(lines: List[String]): Int =
        lines.map { l =>
            val num = l.find(_.isDigit).map(_.toString).getOrElse("")
                ++ l.findLast(_.isDigit).map(_.toString).getOrElse("")
            num.toInt
        }.sum

    def part2(lines: List[String]): Int =
        @tailrec
        def findDigits(line: String, first: Option[Int], last: Int): (Int, Int) =
            if (line.isEmpty) (first.getOrElse(0), last)
            else if (line.head.isDigit)
                val num = line.head.toString.toInt
                findDigits(line.tail, first.orElse(Some(num)), num)
            else if (line.startsWith("one"))
                findDigits(line.tail, first.orElse(Some(1)), 1)
            else if (line.startsWith("two"))
                findDigits(line.tail, first.orElse(Some(2)), 2)
            else if (line.startsWith("three"))
                findDigits(line.tail, first.orElse(Some(3)), 3)
            else if (line.startsWith("four"))
                findDigits(line.tail, first.orElse(Some(4)), 4)
            else if (line.startsWith("five"))
                findDigits(line.tail, first.orElse(Some(5)), 5)
            else if (line.startsWith("six"))
                findDigits(line.tail, first.orElse(Some(6)), 6)
            else if (line.startsWith("seven"))
                findDigits(line.tail, first.orElse(Some(7)), 7)
            else if (line.startsWith("eight"))
                findDigits(line.tail, first.orElse(Some(8)), 8)
            else if (line.startsWith("nine"))
                findDigits(line.tail, first.orElse(Some(9)), 9)
            else
                findDigits(line.tail, first, last)

        lines.map { l =>
            val (n1, n2) = findDigits(l, None, 0)
            (n1.toString ++ n2.toString).toInt
        }.sum
