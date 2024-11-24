package aoc

import zio.test.{ZIOSpecDefault, assertTrue}

object Day9Test extends ZIOSpecDefault:
    val input = List(
      "0 3 6 9 12 15",
      "1 3 6 10 15 21",
      "10 13 16 21 30 45"
    )

    override def spec = suite("Day 9")(
      test("Part 1")(assertTrue(Day9.part1(input) == 114)),
      test("Part 2")(assertTrue(Day9.part2(input) == 2))
    )
