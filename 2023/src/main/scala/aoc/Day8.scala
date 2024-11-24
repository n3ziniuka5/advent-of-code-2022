package aoc

import aoc.Common.timed

import scala.annotation.tailrec
import scala.io.Source

object Day8:
    def main(args: Array[String]): Unit =
        val lines = Source.fromResource("day8.txt").getLines().toList
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    def parse(lines: List[String]): (LazyList[Char], Map[String, (String, String)]) =
        val instructions                          = lines.head.toList
        def instructionsRepeating: LazyList[Char] = LazyList.from(instructions) lazyAppendedAll instructionsRepeating
        val destinations = lines
            .drop(2)
            .map { case s"$src = ($left, $right)" =>
                src -> (left, right)
            }
            .toMap

        (instructionsRepeating, destinations)

    @tailrec
    def find(
        instructions: LazyList[Char],
        destinations: Map[String, (String, String)],
        target: String => Boolean,
        currentPos: String,
        steps: Long
    ): Long =
        if target(currentPos) then steps
        else
            val instruction = instructions.head
            val nextPos     = if (instruction == 'L') destinations(currentPos)._1 else destinations(currentPos)._2
            find(instructions.tail, destinations, target, nextPos, steps + 1)

    def part1(lines: List[String]): Long =
        val (instructions, destinations) = parse(lines)
        find(instructions, destinations, _ == "ZZZ", "AAA", 0)

    def part2(lines: List[String]): Long =
        val (instructions, destinations) = parse(lines)

        val startingPoints = destinations.filter(_._1.endsWith("A")).keySet
        val distances = startingPoints.map { point =>
            find(instructions, destinations, _.endsWith("Z"), point, 0)
        }

        @tailrec
        def gcd(a: Long, b: Long): Long = {
            if (b == 0) a else gcd(b, a % b)
        }

        def lcm(a: Long, b: Long): Long = {
            (a / gcd(a, b)) * b
        }

        distances.reduceLeft(lcm)
