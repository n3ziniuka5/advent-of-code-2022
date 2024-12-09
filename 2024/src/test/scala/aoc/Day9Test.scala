package aoc

import zio.test.ZIOSpecDefault
import zio.test.assertTrue

object Day9Test extends ZIOSpecDefault:
    override def spec = suite("Day 9")(
      test("Part 1")(assertTrue(Day9.part1(InputUtils.fetchSample(2024, 9)) == 1928)),
      test("Part 2")(assertTrue(Day9.part2(InputUtils.fetchSample(2024, 9)) == 2858L))
    )
