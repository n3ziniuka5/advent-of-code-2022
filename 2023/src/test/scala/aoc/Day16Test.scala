package aoc

import zio.test.{ZIOSpecDefault, assertTrue}

object Day16Test extends ZIOSpecDefault:
    val input = List(
      ".|...\\....",
      "|.-.\\.....",
      ".....|-...",
      "........|.",
      "..........",
      ".........\\",
      "..../.\\\\..",
      ".-.-/..|..",
      ".|....-|.\\",
      "..//.|....",
    )

    override def spec = suite("Day 16")(
      test("Part 1")(assertTrue(Day16.part1(input) == 46)),
      test("Part 2")(assertTrue(Day16.part2(input) == 51))
    )
