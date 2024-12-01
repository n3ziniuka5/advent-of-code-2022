package aoc

import zio.test.ZIOSpecDefault
import zio.test.assertTrue

object Day1Test extends ZIOSpecDefault:
    val input = List(
      "3   4",
      "4   3",
      "2   5",
      "1   3",
      "3   9",
      "3   3"
    )

    override def spec = suite("Day 1")(
      test("Part 1")(assertTrue(Day1.part1(input) == 11)),
      test("Part 2")(assertTrue(Day1.part2(input) == 31))
    )
