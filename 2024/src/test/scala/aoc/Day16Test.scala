package aoc

import zio.test.ZIOSpecDefault
import zio.test.assertTrue

object Day16Test extends ZIOSpecDefault:
    override def spec = suite("Day 16")(
      test("Part 1-1")(assertTrue(Day16.part1(InputUtils.fetchSample(2024, 16)) == 7036)),
      test("Part 1-2")(assertTrue(Day16.part1(InputUtils.fetchSample(2024, 16, 3)) == 11048)),
      test("Part 2-1")(assertTrue(Day16.part2(InputUtils.fetchSample(2024, 16)) == 45)),
      test("Part 2-2")(assertTrue(Day16.part2(InputUtils.fetchSample(2024, 16, 3)) == 64)),
    )
