package aoc

import zio.test.{ZIOSpecDefault, assertTrue}

object Day13Test extends ZIOSpecDefault:
    val input = List(
      "#.##..##.",
      "..#.##.#.",
      "##......#",
      "##......#",
      "..#.##.#.",
      "..##..##.",
      "#.#.##.#.",
      "",
      "#...##..#",
      "#....#..#",
      "..##..###",
      "#####.##.",
      "#####.##.",
      "..##..###",
      "#....#..#",
    )

    override def spec = suite("Day 13")(
      test("Part 1")(assertTrue(Day13.part1(input) == 405)),
      test("Part 2")(assertTrue(Day13.part2(input) == 400))
    )
