package aoc

import zio.test.{ZIOSpecDefault, assertTrue}

object Day19Test extends ZIOSpecDefault {
  val input = List(
    "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.",
    "Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.",
  )

  override def spec = suite("Day 19")(
    test("Part 1")(assertTrue(Day19.part1(input) == 33)),
    test("Part 2")(assertTrue(Day19.part2(input) == 56 * 62))
  )
}
