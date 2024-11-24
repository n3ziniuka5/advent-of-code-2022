package aoc

import zio.test.{ZIOSpecDefault, assertTrue}

object Day15Test extends ZIOSpecDefault:
    val input = List(
      "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7",
    )

    override def spec = suite("Day 15")(
      test("Part 1")(assertTrue(Day15.part1(input) == 1320)),
      test("Part 2")(assertTrue(Day15.part2(input) == 145))
    )
