package aoc

import aoc.Common.timed

object Day3:
    def main(args: Array[String]): Unit =
        val lines = InputUtils.fetchInput(2024, 3)
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    def part1(lines: List[String]): Long =
        val line    = lines.mkString
        val matches = "mul\\((\\d+),(\\d+)\\)".r.findAllMatchIn(line)
        val tuples  = matches.map(m => (m.group(1).toInt, m.group(2).toInt))
        tuples.map(_ * _).sum

    def part2(lines: List[String]): Long =
        val line = lines.mkString

        def clean(line: String, removing: Boolean, result: String): String =
            if line.isEmpty then result
            else if removing then
                if line.startsWith("do()") then clean(line.drop(4), false, result)
                else clean(line.drop(1), true, result)
            else if line.startsWith("don't()") then clean(line.drop("don't()".length()), true, result)
            else clean(line.drop(1), false, result + line.head)

        part1(List(clean(line, false, "")))
