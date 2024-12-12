package aoc

import zio.test.ZIOSpecDefault
import zio.test.assertTrue

object Day12Test extends ZIOSpecDefault:
    override def spec = suite("Day 12")(
      test("Part 1-1")(assertTrue(Day12.part1(InputUtils.fetchSample(2024, 12)) == 140)),
      test("Part 1-2")(assertTrue(Day12.part1(InputUtils.fetchSample(2024, 12, 3)) == 772)),
      test("Part 1-3")(assertTrue(Day12.part1(InputUtils.fetchSample(2024, 12, 4)) == 1930)),
      test("Part 2-1")(assertTrue(Day12.part2(InputUtils.fetchSample(2024, 12)) == 80)),
      test("Part 2-2")(assertTrue(Day12.part2(InputUtils.fetchSample(2024, 12, 3)) == 436)),
      test("Part 2-3")(assertTrue(Day12.part2(InputUtils.fetchSample(2024, 12, 4)) == 1206)),
      test("Part 2-7")(assertTrue(Day12.part2(InputUtils.fetchSample(2024, 12, 7)) == 368)),
    )
