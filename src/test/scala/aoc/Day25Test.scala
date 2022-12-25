package aoc

import zio.test.{ZIOSpecDefault, assertTrue}

object Day25Test extends ZIOSpecDefault {
  val input = List(
    "1=-0-2",
    "12111",
    "2=0=",
    "21",
    "2=01",
    "111",
    "20012",
    "112",
    "1=-1=",
    "1-12",
    "12",
    "1=",
    "122",
  )

  override def spec = suite("Day 25")(
    test("Part 1")(assertTrue(Day25.part1(input) == "2=-1=0"))
  )
}
