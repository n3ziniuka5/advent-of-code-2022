package aoc

import zio.test.ZIOSpecDefault
import zio.test.assertTrue

object Day4Test extends ZIOSpecDefault:
    override def spec = suite("Day 4")(
      test("Part 1")(assertTrue(Day4.part1(InputUtils.fetchSample(2024, 4, 2)) == 18)),
      test("Part 2")(assertTrue(Day4.part2(InputUtils.fetchSample(2024, 4, 5)) == 9))
    )
