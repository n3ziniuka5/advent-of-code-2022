package aoc

import zio.test.ZIOSpecDefault
import zio.test.assertTrue

object Day1Test extends ZIOSpecDefault:
    val input = List(
      "1abc2",
      "pqr3stu8vwx",
      "a1b2c3d4e5f",
      "treb7uchet",
    )

    val part2Input = List(
      "two1nine",
      "eightwothree",
      "abcone2threexyz",
      "xtwone3four",
      "4nineeightseven2",
      "zoneight234",
      "7pqrstsixteen"
    )

    override def spec = suite("Day 1")(
      test("Part 1")(assertTrue(Day1.part1(input) == 142)),
      test("Part 2")(assertTrue(Day1.part2(part2Input) == 281))
    )
