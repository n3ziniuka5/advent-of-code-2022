package aoc

import aoc.Common.timed

object Day11:
    def main(args: Array[String]): Unit =
        val lines = InputUtils.fetchInput(2024, 11)
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    def part1(lines: List[String]): Long =
        val initial = parse(lines)
        blink(initial, 25).toMap.values.sum

    def part2(lines: List[String]): Long =
        val initial = parse(lines)
        blink(initial, 75).toMap.values.sum

    def parse(lines: List[String]): MultiSetLong[Long] =
        val numbers = lines.head.split(' ').map(_.toLong)
        MultiSetLong.fromIterable(numbers)

    def blink(numberSet: MultiSetLong[Long], remaining: Int): MultiSetLong[Long] =
        if remaining == 0 then numberSet
        else
            val newNumberSet = numberSet.flatMap: n =>
                if n == 0 then MultiSetLong(1L)
                else if n.toString().size % 2 == 0 then
                    val (left, right) = n.toString().splitAt(n.toString().size / 2)
                    MultiSetLong(left.toLong, right.toLong)
                else MultiSetLong(n * 2024)
            blink(newNumberSet, remaining - 1)
