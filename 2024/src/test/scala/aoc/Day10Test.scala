package aoc

import zio.test.ZIOSpecDefault
import zio.test.assertTrue

object Day10Test extends ZIOSpecDefault:
    override def spec = suite("Day 10")(
      test("Part 1")(assertTrue(Day10.part1(InputUtils.fetchSample(2024, 10, 5)) == 36)),
      test("Part 2")(assertTrue(Day10.part2(InputUtils.fetchSample(2024, 10, 5)) == 81))
    )
