package aoc

import zio.test.{ZIOSpecDefault, assertTrue}

object Day8Test extends ZIOSpecDefault {
  val input = List(
    "30373",
    "25512",
    "65332",
    "33549",
    "35390"
  )

  override def spec = suite("Day 8")(
    test("Part 1")(assertTrue(Day8.part1(input) == 21)),
    test("Part 2")(assertTrue(Day8.part2(input) == 8))
  )
}
