package aoc

import zio.test.ZIOSpecDefault
import zio.test.assertTrue

object Day20Test extends ZIOSpecDefault:
    override def spec = suite("Day 20")(
      test("Part 1")(assertTrue(Day20.part1(InputUtils.fetchSample(2024, 20), 20) == 5)),
      test("Part 2")(assertTrue(Day20.part2(InputUtils.fetchSample(2024, 20), 50) == 285))
    )
