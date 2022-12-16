package aoc

import aoc.Day16Types.Pressure
import zio.test.{ZIOSpecDefault, assertTrue}

object Day16Test extends ZIOSpecDefault {
  val input = List(
    "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB",
    "Valve BB has flow rate=13; tunnels lead to valves CC, AA",
    "Valve CC has flow rate=2; tunnels lead to valves DD, BB",
    "Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE",
    "Valve EE has flow rate=3; tunnels lead to valves FF, DD",
    "Valve FF has flow rate=0; tunnels lead to valves EE, GG",
    "Valve GG has flow rate=0; tunnels lead to valves FF, HH",
    "Valve HH has flow rate=22; tunnel leads to valve GG",
    "Valve II has flow rate=0; tunnels lead to valves AA, JJ",
    "Valve JJ has flow rate=21; tunnel leads to valve II",
  )

  override def spec = suite("Day 4")(
    test("Part 1")(assertTrue(Day16.part1(input) == Pressure(1651))),
    test("Part 2")(assertTrue(Day16.part2(input) == Pressure(1707)))
  )
}
