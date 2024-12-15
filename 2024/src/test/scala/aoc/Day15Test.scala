package aoc

import zio.test.ZIOSpecDefault
import zio.test.assertTrue

object Day15Test extends ZIOSpecDefault:
    override def spec = suite("Day 15")(
      test("Part 1")(assertTrue(Day15.part1(InputUtils.fetchSample(2024, 15)) == 10092)),
      test("Part 2")(assertTrue(Day15.part2(InputUtils.fetchSample(2024, 15)) == 9021))
    )
