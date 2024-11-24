package aoc

import zio.test.{ZIOSpecDefault, assertTrue}

object Day2Test extends ZIOSpecDefault {
  val input = List(
    "A Y",
    "B X",
    "C Z"
  )

  override def spec = suite("Day 2")(
    test("Part 1")(assertTrue(Day2.part1(input) == 15)),
    test("Part 2")(assertTrue(Day2.part2(input) == 12))
  )
}
