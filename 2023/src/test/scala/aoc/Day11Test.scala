package aoc

import zio.test.{ZIOSpecDefault, assertTrue}

object Day11Test extends ZIOSpecDefault:
    val input = List(
      "...#......",
      ".......#..",
      "#.........",
      "..........",
      "......#...",
      ".#........",
      ".........#",
      "..........",
      ".......#..",
      "#...#....."
    )

    override def spec = suite("Day 11")(
      test("Part 1")(assertTrue(Day11.part1(input) == 374)),
      test("Part 2")(assertTrue(Day11.part2(input) == 82000210))
    )
