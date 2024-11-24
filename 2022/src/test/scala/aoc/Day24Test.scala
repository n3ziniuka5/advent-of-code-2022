package aoc

import zio.test.{ZIOSpecDefault, assertTrue}

object Day24Test extends ZIOSpecDefault {
  val input = List(
    "#.######",
    "#>>.<^<#",
    "#.<..<<#",
    "#>v.><>#",
    "#<^v^^>#",
    "######.#"
  )

  override def spec = suite("Day 4")(
    test("Part 1")(assertTrue(Day24.part1(input) == 18)),
    test("Part 2")(assertTrue(Day24.part2(input) == 54))
  )
}
