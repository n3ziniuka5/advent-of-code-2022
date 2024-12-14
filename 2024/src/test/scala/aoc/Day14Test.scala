package aoc

import zio.test.ZIOSpecDefault
import zio.test.assertTrue

object Day14Test extends ZIOSpecDefault:
    override def spec = suite("Day 14")(
      test("Part 1")(assertTrue(Day14.part1(InputUtils.fetchSample(2024, 14)) == 12))
    )
