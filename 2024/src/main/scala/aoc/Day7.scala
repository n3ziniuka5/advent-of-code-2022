package aoc

import aoc.Common.timed
import scala.language.experimental.namedTuples
import scala.annotation.tailrec

object Day7:
    def main(args: Array[String]): Unit =
        val lines = InputUtils.fetchInput(2024, 7)
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    def part1(lines: List[String]): Long =
        parse(lines).filter(l => solutionExists(l.target, l.numbers, withConcatenation = false)).map(_.target).sum

    def part2(lines: List[String]): Long =
        parse(lines).filter(l => solutionExists(l.target, l.numbers, withConcatenation = true)).map(_.target).sum

    def parse(lines: List[String]): List[(target: Long, numbers: List[Long])] =
        lines.map: line =>
            val Array(target, numbersString) = line.split(": ")
            val numbers                      = numbersString.split(" ").map(_.toLong).toList
            (target.toLong, numbers)

    def solutionExists(target: Long, numbers: List[Long], withConcatenation: Boolean): Boolean =
        @tailrec
        def loop(searches: List[(currentAmount: Long, numbers: List[Long])]): Boolean =
            searches match
                case search :: remainingSearches =>
                    if search.currentAmount > target then loop(remainingSearches)
                    else
                        search.numbers match
                            case number :: remainingNumbers =>
                                val newSearches = List(
                                  Some((search.currentAmount + number, remainingNumbers)),
                                  Some((search.currentAmount * number, remainingNumbers)),
                                  Option.when(withConcatenation)(
                                    (s"${search.currentAmount}${number}".toLong, remainingNumbers)
                                  ),
                                )
                                loop(newSearches.flatten ++ remainingSearches)
                            case Nil =>
                                if search.currentAmount == target then true
                                else loop(remainingSearches)
                case Nil => false
        loop(List((numbers.head, numbers.tail)))
