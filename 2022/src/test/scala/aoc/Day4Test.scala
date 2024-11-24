package aoc

import zio.test.{ZIOSpecDefault, assertTrue}

object Day4Test extends ZIOSpecDefault {
  val input = List(
    "2-4,6-8",
    "2-3,4-5",
    "5-7,7-9",
    "2-8,3-7",
    "6-6,4-6",
    "2-6,4-8"
  )

  override def spec = suite("Day 4")(
    test("Part 1")(assertTrue(Day4.part1(input) == 2)),
    test("Part 2")(assertTrue(Day4.part2(input) == 4))
  )
}
