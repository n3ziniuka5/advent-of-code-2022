package aoc

import zio.test.{ZIOSpecDefault, assertTrue}

object Day12Test extends ZIOSpecDefault:
    val input = List(
      "???.### 1,1,3",
      ".??..??...?##. 1,1,3",
      "?#?#?#?#?#?#?#? 1,3,1,6",
      "????.#...#... 4,1,1",
      "????.######..#####. 1,6,5",
      "?###???????? 3,2,1",
    )

    override def spec = suite("Day 12")(
      test("Part 1")(assertTrue(Day12.part1(input) == 21)),
      test("Part 2")(assertTrue(Day12.part2(input) == 525152))
    )
