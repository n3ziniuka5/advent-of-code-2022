package aoc

import zio.test.ZIOSpecDefault
import zio.test.assertTrue

object Day1Test extends ZIOSpecDefault:
    override def spec = suite("Day 1")(
      test("Part 1")(assertTrue(Day1.part1(InputUtils.fetchSample(2024, 1)) == 11)),
      test("Part 2")(assertTrue(Day1.part2(InputUtils.fetchSample(2024, 1)) == 31))
    )
