package aoc

import zio.test.{ZIOSpecDefault, assertTrue}

object Day14Test extends ZIOSpecDefault:
    val input = List(
      "O....#....",
      "O.OO#....#",
      ".....##...",
      "OO.#O....O",
      ".O.....O#.",
      "O.#..O.#.#",
      "..O..#O..O",
      ".......O..",
      "#....###..",
      "#OO..#....",
    )

    override def spec = suite("Day 14")(
      test("Part 1")(assertTrue(Day14.part1(input) == 136)),
      test("Part 2")(assertTrue(Day14.part2(input) == 64))
    )
