package aoc

import zio.test.{ZIOSpecDefault, assertTrue}

object Day3Test extends ZIOSpecDefault {
  val input = List(
    "vJrwpWtwJgWrhcsFMMfFFhFp",
    "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
    "PmmdzqPrVvPwwTWBwg",
    "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
    "ttgJtRGJQctTZtZT",
    "CrZsJsPPZsGzwwsLwLmpwMDw"
  )

  override def spec = suite("Day 3")(
    test("Part 1")(assertTrue(Day3.part1(input) == 157)),
    test("Part 2")(assertTrue(Day3.part2(input) == 70))
  )
}
