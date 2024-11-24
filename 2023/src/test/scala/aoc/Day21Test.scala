package aoc

import zio.test.{ZIOSpecDefault, assertTrue}

object Day21Test extends ZIOSpecDefault:
    val input = List(
      "...........",
      ".....###.#.",
      ".###.##..#.",
      "..#.#...#..",
      "....#.#....",
      ".##..S####.",
      ".##..#...#.",
      ".......##..",
      ".##.#.####.",
      ".##..##.##.",
      "...........",
    )

    override def spec = suite("Day 21")(
      test("Part 1")(assertTrue(Day21Mess.part1(input, 6) == 165)),
      // test input for part 2 is useless, real data has a distinct pattern
    )
