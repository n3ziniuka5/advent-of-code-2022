package aoc

import zio.test.ZIOSpecDefault
import zio.test.assertTrue

object Day17Test extends ZIOSpecDefault:
    override def spec = suite("Day 17")(
      test("Part 1")(assertTrue(Day17.part1(InputUtils.fetchSample(2024, 17)) == "4,6,3,5,6,3,5,2,1,0")),
      test("Part 2")(assertTrue(Day17.part2(InputUtils.fetchSample(2024, 17, 2)) == 117440))
    )
