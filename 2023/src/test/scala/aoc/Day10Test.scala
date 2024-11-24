package aoc

import zio.test.{ZIOSpecDefault, assertTrue}

object Day10Test extends ZIOSpecDefault:
    val input = List(
      "..F7.",
      ".FJ|.",
      "SJ.L7",
      "|F--J",
      "LJ..."
    )

    val inputPart2 = List(
      "..........",
      ".S------7.",
      ".|F----7|.",
      ".||....||.",
      ".||....||.",
      ".|L-7F-J|.",
      ".|..||..|.",
      ".L--JL--J.",
      ".........."
    )

    val inputPart2Complex1 = List(
      ".F----7F7F7F7F-7....",
      ".|F--7||||||||FJ....",
      ".||.FJ||||||||L7....",
      "FJL7L7LJLJ||LJ.L-7..",
      "L--J.L7...LJS7F-7L7.",
      "....F-J..F7FJ|L7L7L7",
      "....L7.F7||L7|.L7L7|",
      ".....|FJLJ|FJ|F7|.LJ",
      "....FJL-7.||.||||...",
      "....L---J.LJ.LJLJ..."
    )

    val inputPart2Complex2 = List(
      "FF7FSF7F7F7F7F7F---7",
      "L|LJ||||||||||||F--J",
      "FL-7LJLJ||||||LJL-77",
      "F--JF--7||LJLJ7F7FJ-",
      "L---JF-JLJ.||-FJLJJ7",
      "|F|F-JF---7F7-L7L|7|",
      "|FFJF7L7F-JF7|JL---7",
      "7-L-JL7||F7|L7F-7F7|",
      "L.L7LFJ|||||FJL7||LJ",
      "L7JLJL-JLJLJL--JLJ.L"
    )

    override def spec = suite("Day 10")(
      test("Part 1")(assertTrue(Day10.part1(input) == 8)),
      test("Part 2")(assertTrue(Day10.part2(inputPart2) == 4)),
      test("Part 2 complex 1")(assertTrue(Day10.part2(inputPart2Complex1) == 8)),
      test("Part 2 complex 2")(assertTrue(Day10.part2(inputPart2Complex2) == 10))
    )
