package aoc

import zio.test.{ZIOSpecDefault, assertTrue}

object Day13Test extends ZIOSpecDefault {
  val input = List(
    "[1,1,3,1,1]",
    "[1,1,5,1,1]",
    "",
    "[[1],[2,3,4]]",
    "[[1],4]",
    "",
    "[9]",
    "[[8,7,6]]",
    "",
    "[[4,4],4,4]",
    "[[4,4],4,4,4]",
    "",
    "[7,7,7,7]",
    "[7,7,7]",
    "",
    "[]",
    "[3]",
    "",
    "[[[]]]",
    "[[]]",
    "",
    "[1,[2,[3,[4,[5,6,7]]]],8,9]",
    "[1,[2,[3,[4,[5,6,0]]]],8,9]",
  )

  override def spec = suite("Day 13")(
    test("Part 1")(assertTrue(Day13.part1(input) == 13)),
    test("Part 2")(assertTrue(Day13.part2(input) == 140))
  )
}
