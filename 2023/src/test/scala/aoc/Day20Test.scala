package aoc

import zio.test.{ZIOSpecDefault, assertTrue}

object Day20Test extends ZIOSpecDefault:
    val input1 = List(
      "broadcaster -> a, b, c",
      "%a -> b",
      "%b -> c",
      "%c -> inv",
      "&inv -> a",
    )

    val input2 = List(
      "broadcaster -> a",
      "%a -> inv, con",
      "&inv -> b",
      "%b -> con",
      "&con -> output"
    )

    override def spec = suite("Day 20")(
      test("Part 1-1")(assertTrue(Day20.part1(input1) == 32000000)),
      test("Part 1-2")(assertTrue(Day20.part1(input2) == 11687500)),
      // test("Part 2")(assertTrue(Day20.part2(input) == 0))
    )
