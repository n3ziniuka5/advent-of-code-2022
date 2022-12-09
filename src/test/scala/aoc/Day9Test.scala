package aoc

import zio.test.{ZIOSpecDefault, assertTrue}

object Day9Test extends ZIOSpecDefault {
  val input = List(
    "R 4",
    "U 4",
    "L 3",
    "D 1",
    "R 4",
    "D 1",
    "L 5",
    "R 2"
  )

  val largerExample = List(
    "R 5",
    "U 8",
    "L 8",
    "D 3",
    "R 17",
    "D 10",
    "L 25",
    "U 20"
  )

  override def spec = suite("Day 9")(
    // test("Part 1")(assertTrue(Day9.part1(input) == 13)),
    test("Part 2")(assertTrue(Day9.part2(input) == 1)),
    // test("Part 2")(assertTrue(Day9.part2(largerExample) == 36))
  )
}
