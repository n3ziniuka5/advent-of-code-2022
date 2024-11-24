package aoc

import zio.test.{ZIOSpecDefault, assertTrue}

object Day6Test extends ZIOSpecDefault:
    val input = List(
      "Time:      7  15   30",
      "Distance:  9  40  200",
    )

    override def spec = suite("Day 6")(
      test("Part 1")(assertTrue(Day6.part1(input) == 288)),
      test("Part 2")(assertTrue(Day6.part2(input) == 71503))
    )
