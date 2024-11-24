package aoc

import zio.test.{ZIOSpecDefault, assertTrue}

object Day3Test extends ZIOSpecDefault:
    val input = List(
      "467..114..",
      "...*......",
      "..35..633.",
      "......#...",
      "617*......",
      ".....+.58.",
      "..592.....",
      "......755.",
      "...$.*....",
      ".664.598..",
    )

    override def spec = suite("Day 3")(
      test("Part 1")(assertTrue(Day3.part1(input) == 4361)),
      test("Part 2")(assertTrue(Day3.part2(input) == 467835))
    )
