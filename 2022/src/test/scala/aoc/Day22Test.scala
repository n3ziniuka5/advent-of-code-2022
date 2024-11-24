package aoc

import zio.test.{ZIOSpecDefault, assertTrue}

object Day22Test extends ZIOSpecDefault {
  val input = List(
    "        ...#    ",
    "        .#..    ",
    "        #...    ",
    "        ....    ",
    "...#.......#    ",
    "........#...    ",
    "..#....#....    ",
    "..........#.    ",
    "        ...#....",
    "        .....#..",
    "        .#......",
    "        ......#.",
    "",
    "10R5L5R10L4R5L5",
  )

  override def spec = suite("Day 22")(
    test("Part 1")(assertTrue(Day22.part1(input) == 6032)),
    test("Part 2")(assertTrue(Day22.part2(input) == 5031))
  )
}
