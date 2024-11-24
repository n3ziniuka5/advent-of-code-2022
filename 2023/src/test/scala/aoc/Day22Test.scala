package aoc

import zio.test.{ZIOSpecDefault, assertTrue}

object Day22Test extends ZIOSpecDefault:
    val input = List(
      "1,0,1~1,2,1",
      "0,0,2~2,0,2",
      "0,2,3~2,2,3",
      "0,0,4~0,2,4",
      "2,0,5~2,2,5",
      "0,1,6~2,1,6",
      "1,1,8~1,1,9",
    )

    override def spec = suite("Day 22")(
      test("Part 1")(assertTrue(Day22.part1(input) == 5)),
      test("Part 2")(assertTrue(Day22.part2(input) == 7)),
    )
