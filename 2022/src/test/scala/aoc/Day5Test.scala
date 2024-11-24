package aoc

import zio.test.{ZIOSpecDefault, assertTrue}

object Day5Test extends ZIOSpecDefault {
  val input = List(
    "    [D]    ",
    "[N] [C]    ",
    "[Z] [M] [P]",
    " 1   2   3 ",
    "",
    "move 1 from 2 to 1",
    "move 3 from 1 to 3",
    "move 2 from 2 to 1",
    "move 1 from 1 to 2",
  )

  override def spec = suite("Day 5")(
    test("Part 1")(assertTrue(Day5.part1(input) == "CMZ")),
    test("Part 2")(assertTrue(Day5.part2(input) == "MCD"))
  )
}
