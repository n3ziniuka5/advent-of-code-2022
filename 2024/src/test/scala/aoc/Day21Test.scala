package aoc

import zio.test.ZIOSpecDefault
import zio.test.assertTrue

object Day21Test extends ZIOSpecDefault:
    override def spec = suite("Day 21")(
      test("Part 1")(assertTrue(Day21.part1(InputUtils.fetchSample(2024, 21, 4)) == 126384))
    )
