package aoc

import zio.test.ZIOSpecDefault
import zio.test.assertTrue

object Day11Test extends ZIOSpecDefault:
    override def spec = suite("Day 11")(
      test("Part 1")(assertTrue(Day11.part1(List("125 17")) == 55312L)),
    )
