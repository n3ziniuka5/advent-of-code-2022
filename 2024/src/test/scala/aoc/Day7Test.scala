package aoc

import zio.test.ZIOSpecDefault
import zio.test.assertTrue

object Day7Test extends ZIOSpecDefault:
    override def spec = suite("Day 7")(
      test("Part 1")(assertTrue(Day7.part1(InputUtils.fetchSample(2024, 7)) == 3749)),
      test("Part 2")(assertTrue(Day7.part2(InputUtils.fetchSample(2024, 7)) == 11387))
    )
