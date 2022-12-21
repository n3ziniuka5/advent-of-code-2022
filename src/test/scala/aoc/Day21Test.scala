package aoc

import zio.test.{ZIOSpecDefault, assertTrue}

object Day21Test extends ZIOSpecDefault {
  val input = List(
    "root: pppw + sjmn",
    "dbpl: 5",
    "cczh: sllz + lgvd",
    "zczc: 2",
    "ptdq: humn - dvpt",
    "dvpt: 3",
    "lfqf: 4",
    "humn: 5",
    "ljgn: 2",
    "sjmn: drzm * dbpl",
    "sllz: 4",
    "pppw: cczh / lfqf",
    "lgvd: ljgn * ptdq",
    "drzm: hmdt - zczc",
    "hmdt: 32"
  )

  override def spec = suite("Day 21")(
    test("Part 1")(assertTrue(Day21.part1(input) == 152L)),
    test("Part 2")(assertTrue(Day21.part2(input) == 301L))
  )
}
