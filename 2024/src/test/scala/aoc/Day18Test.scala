package aoc

import zio.test.ZIOSpecDefault
import zio.test.assertTrue

object Day18Test extends ZIOSpecDefault:
    override def spec = suite("Day 18")(
      test("Part 1")(assertTrue(Day18.part1(InputUtils.fetchSample(2024, 18)) == 22)),
      test("Part 2")(assertTrue(Day18.part2(InputUtils.fetchSample(2024, 18)) == "6,1"))
    )
