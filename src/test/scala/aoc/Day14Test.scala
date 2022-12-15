package aoc

import zio.test.{ZIOSpecDefault, assertTrue}

object Day14Test extends ZIOSpecDefault {
  val input = List(
    "498,4 -> 498,6 -> 496,6",
    "503,4 -> 502,4 -> 502,9 -> 494,9",
  )

  override def spec = suite("Day 14")(
    test("Part 1")(assertTrue(Day14.part1(input) == 24)),
    test("Part 2")(assertTrue(Day14.part2(input) == 93))
  )
}
