package aoc

import zio.test.ZIOSpecDefault
import zio.test.assertTrue

object Day1Test extends ZIOSpecDefault {
  val input = List(
    "1000",
    "2000",
    "3000",
    "",
    "4000",
    "",
    "5000",
    "6000",
    "",
    "7000",
    "8000",
    "9000",
    "",
    "10000"
  )

  override def spec = suite("Day 1")(
    test("Part 1")(assertTrue(Day1.part1(input) == 24000L)),
    test("Part 2")(assertTrue(Day1.part2(input) == 45000L))
  )
}
