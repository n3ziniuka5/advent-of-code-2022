package aoc

import zio.test.{ZIOSpecDefault, assertTrue}

object Day7Test extends ZIOSpecDefault:
    val input = List(
      "32T3K 765",
      "T55J5 684",
      "KK677 28",
      "KTJJT 220",
      "QQQJA 483",
    )

    override def spec = suite("Day 7")(
      test("Part 1")(assertTrue(Day7.part1(input) == 6440)),
      test("Part 2")(assertTrue(Day7.part2(input) == 5905))
    )
