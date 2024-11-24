package aoc

import zio.test.{ZIOSpecDefault, assertTrue}

object Day23Test extends ZIOSpecDefault {
  val input = List(
    "....#..",
    "..###.#",
    "#...#.#",
    ".#...##",
    "#.###..",
    "##.#.##",
    ".#..#..",
  )

  override def spec = suite("Day 23")(
    test("Part 1")(assertTrue(Day23.part1(input) == 110)),
    test("Part 2")(assertTrue(Day23.part2(input) == 20))
  )
}
