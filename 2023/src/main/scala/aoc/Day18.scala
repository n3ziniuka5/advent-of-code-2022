package aoc

import aoc.Common.timed

import scala.annotation.tailrec
import scala.io.Source

object Day18:
    def main(args: Array[String]): Unit =
        val lines = Source.fromResource("day18.txt").getLines().toList
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    case class Instruction(direction: Char, count: Int, colorCode: String)

    def part1(lines: List[String]): Long =
        solve(parse(lines), 0, 0)

    def part2(lines: List[String]): Long =
        val insructions = parse(lines).map { i =>
            val direction = i.colorCode.last match
                case '0' => 'R'
                case '1' => 'D'
                case '2' => 'L'
                case '3' => 'U'
            i.copy(count = Integer.parseInt(i.colorCode.take(5), 16), direction = direction)
        }
        solve(insructions, 0, 0)

    def parse(lines: List[String]): List[Instruction] =
        lines.map { case s"$direction $count (#$colorCode)" =>
            Instruction(direction.head, count.toInt, colorCode)
        }

    @tailrec
    def solve(instructions: List[Instruction], rightOffset: Long, count: Long): Long =
        if (instructions.isEmpty) count + 1
        else
            val head = instructions.head

            val (newOffset, newCount) = head.direction match
                case 'R' => (rightOffset + head.count, count)
                case 'L' => (rightOffset - head.count, count + head.count)
                case 'U' => (rightOffset, count - (head.count * (rightOffset - 1)))
                case 'D' =>
                    (rightOffset, count + (head.count * rightOffset))

            solve(instructions.tail, newOffset, newCount)
