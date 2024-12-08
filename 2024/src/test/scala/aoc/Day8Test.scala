package aoc

import zio.test.ZIOSpecDefault
import zio.test.assertTrue

object Day8Test extends ZIOSpecDefault:
    override def spec = suite("Day 8")(
      test("Part 1")(assertTrue(Day8.part1(InputUtils.fetchSample(2024, 8)) == 14)),
      test("Part 1-2")(assertTrue(Day8.part1(InputUtils.fetchSample(2024, 8, 3)) == 4)),
      test("Part 2")(assertTrue(Day8.part2(InputUtils.fetchSample(2024, 8)) == 34)),
      test("Part 2-2")(assertTrue(Day8.part2(InputUtils.fetchSample(2024, 8, 6)) == 9))
    )
