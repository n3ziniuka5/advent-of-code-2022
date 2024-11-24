package aoc

import zio.test.{ZIOSpecDefault, assertTrue}

object Day25Test extends ZIOSpecDefault:
    val input = List(
      "jqt: rhn xhk nvd",
      "rsh: frs pzl lsr",
      "xhk: hfx",
      "cmg: qnr nvd lhk bvb",
      "rhn: xhk bvb hfx",
      "bvb: xhk hfx",
      "pzl: lsr hfx nvd",
      "qnr: nvd",
      "ntq: jqt hfx bvb xhk",
      "nvd: lhk",
      "lsr: lhk",
      "rzs: qnr cmg lsr rsh",
      "frs: qnr lhk lsr",
    )

    override def spec = suite("Day 25")(
      test("Part 1")(assertTrue(Day25.part1(input) == 54))
    )
