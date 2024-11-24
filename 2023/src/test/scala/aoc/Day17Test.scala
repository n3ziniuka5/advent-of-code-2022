package aoc

import zio.test.{ZIOSpecDefault, assertTrue}

object Day17Test extends ZIOSpecDefault:
    val input = List(
      "2413432311323",
      "3215453535623",
      "3255245654254",
      "3446585845452",
      "4546657867536",
      "1438598798454",
      "4457876987766",
      "3637877979653",
      "4654967986887",
      "4564679986453",
      "1224686865563",
      "2546548887735",
      "4322674655533",
    )

    override def spec = suite("Day 17")(
      test("Part 1")(assertTrue(Day17.part1(input) == 102)),
      test("Part 2")(assertTrue(Day17.part2(input) == 94))
    )
