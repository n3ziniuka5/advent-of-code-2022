package aoc

import zio.test.{ZIOSpecDefault, assertTrue}

object Day12Test extends ZIOSpecDefault {
  val input = List(
    "Sabqponm",
    "abcryxxl",
    "accszExk",
    "acctuvwj",
    "abdefghi"
  )

  override def spec = suite("Day 12")(
    test("Part 1")(assertTrue(Day12.part1(input) == 31)),
    test("Part 2")(assertTrue(Day12.part2(input) == 29))
  )
}
