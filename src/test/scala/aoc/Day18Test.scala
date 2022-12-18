package aoc

import zio.test.{ZIOSpecDefault, assertTrue}

object Day18Test extends ZIOSpecDefault {
  val input = List(
    "2,2,2",
    "1,2,2",
    "3,2,2",
    "2,1,2",
    "2,3,2",
    "2,2,1",
    "2,2,3",
    "2,2,4",
    "2,2,6",
    "1,2,5",
    "3,2,5",
    "2,1,5",
    "2,3,5",
  )

  override def spec = suite("Day 18")(
    test("Part 1")(assertTrue(Day18.part1(input) == 64)),
    test("Part 2")(assertTrue(Day18.part2(input) == 58))
  )
}
