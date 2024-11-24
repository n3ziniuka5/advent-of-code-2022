package aoc

import zio.test.{ZIOSpecDefault, assertTrue}

object Day6Test extends ZIOSpecDefault {
  override def spec = suite("Day 6")(
    test("Part 1")(assertTrue(Day6.part1("bvwbjplbgvbhsrlpgdmjqwftvncz") == 5)),
    test("Part 1")(assertTrue(Day6.part1("nppdvjthqldpwncqszvftbrmjlhg") == 6)),
    test("Part 1")(assertTrue(Day6.part1("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg") == 10)),
    test("Part 1")(assertTrue(Day6.part1("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw") == 11)),
    test("Part 2")(assertTrue(Day6.part2("mjqjpqmgbljsphdztnvjfqwrcgsmlb") == 19)),
    test("Part 2")(assertTrue(Day6.part2("bvwbjplbgvbhsrlpgdmjqwftvncz") == 23)),
    test("Part 2")(assertTrue(Day6.part2("nppdvjthqldpwncqszvftbrmjlhg") == 23)),
    test("Part 2")(assertTrue(Day6.part2("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg") == 29)),
    test("Part 2")(assertTrue(Day6.part2("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw") == 26))
  )
}
