package aoc

import zio.test.{ZIOSpecDefault, assertTrue}

object Day8Test extends ZIOSpecDefault:
    val input = List(
      "RL",
      "",
      "AAA = (BBB, CCC)",
      "BBB = (DDD, EEE)",
      "CCC = (ZZZ, GGG)",
      "DDD = (DDD, DDD)",
      "EEE = (EEE, EEE)",
      "GGG = (GGG, GGG)",
      "ZZZ = (ZZZ, ZZZ)",
    )

    val inputPart2 = List(
      "LR",
      "",
      "11A = (11B, XXX)",
      "11B = (XXX, 11Z)",
      "11Z = (11B, XXX)",
      "22A = (22B, XXX)",
      "22B = (22C, 22C)",
      "22C = (22Z, 22Z)",
      "22Z = (22B, 22B)",
      "XXX = (XXX, XXX)",
    )

    override def spec = suite("Day 8")(
      test("Part 1")(assertTrue(Day8.part1(input) == 2)),
      test("Part 2")(assertTrue(Day8.part2(inputPart2) == 6))
    )
