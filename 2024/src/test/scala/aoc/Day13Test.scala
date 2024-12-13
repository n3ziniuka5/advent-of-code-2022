package aoc

import zio.test.ZIOSpecDefault
import zio.test.assertTrue

object Day13Test extends ZIOSpecDefault:
    override def spec = suite("Day 13")(
      test("Part 1")(assertTrue(Day13.part1(InputUtils.fetchSample(2024, 13)) == 480))
    )
