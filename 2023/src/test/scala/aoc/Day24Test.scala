package aoc

import zio.test.{ZIOSpecDefault, assertTrue}

object Day24Test extends ZIOSpecDefault:
    val input = List(
      "19, 13, 30 @ -2, 1, -2",
      "18, 19, 22 @ -1, -1, -2",
      "20, 25, 34 @ -2, -2, -4",
      "12, 31, 28 @ -1, -2, -1",
      "20, 19, 15 @ 1, -5, -3",
    )

    override def spec = suite("Day 24")(
      test("Part 1")(assertTrue(Day24.part1(input, 7, 27) == 2)),
      test("Part 2")(assertTrue(Day24.part2(input) == 47)),
    )
