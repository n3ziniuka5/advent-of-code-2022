package aoc

import zio.test.{ZIOSpecDefault, assertTrue}

object Day17Test extends ZIOSpecDefault {
  val input = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"

  override def spec = suite("Day 17")(
    test("Part 1")(assertTrue(Day17.part1(input) == 3068L)),
    test("Part 2")(assertTrue(Day17.part2(input) == 1514285714288L))
  )
}
