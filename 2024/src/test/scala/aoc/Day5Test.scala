package aoc

import zio.test.ZIOSpecDefault
import zio.test.assertTrue

object Day5Test extends ZIOSpecDefault:
    override def spec = suite("Day 5")(
      test("Part 1")(assertTrue(Day5.part1(InputUtils.fetchSample(2024, 5)) == 143L)),
      test("Part 2")(assertTrue(Day5.part2(InputUtils.fetchSample(2024, 5)) == 123L))
    )
