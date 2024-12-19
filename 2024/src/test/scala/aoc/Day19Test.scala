package aoc

import zio.test.ZIOSpecDefault
import zio.test.assertTrue

object Day19Test extends ZIOSpecDefault:
    override def spec = suite("Day 19")(
      test("Part 1")(assertTrue(Day19.part1(InputUtils.fetchSample(2024, 19)) == 6)),
      test("Part 2")(assertTrue(Day19.part2(InputUtils.fetchSample(2024, 19)) == 16))
    )
