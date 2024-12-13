package aoc

import aoc.Common.timed
import scala.math.BigDecimal

object Day13:
    case class Point(x: Long, y: Long)
    case class Machine(buttonA: Point, buttonB: Point, prize: Point)

    def main(args: Array[String]): Unit =
        val lines = InputUtils.fetchInput(2024, 13)
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    def part1(lines: List[String]): Long =
        val machines = parse(lines)
        machines.map(coinsToWin(_, applyPressLimit = true)).sum

    def part2(lines: List[String]): Long =
        val machines = parse(lines).map: m =>
            m.copy(prize = Point(m.prize.x + 10000000000000L, m.prize.y + 10000000000000L))
        machines.map(coinsToWin(_, applyPressLimit = false)).sum

    def parse(lines: List[String]): List[Machine] =
        lines
            .filter(_.nonEmpty)
            .grouped(3)
            .toList
            .map: machineLines =>
                val buttonA = machineLines.head match
                    case s"Button A: X+$x, Y+$y" => Point(x.toInt, y.toInt)
                val buttonB = machineLines.tail.head match
                    case s"Button B: X+$x, Y+$y" => Point(x.toInt, y.toInt)
                val prize = machineLines.last match
                    case s"Prize: X=$x, Y=$y" => Point(x.toInt, y.toInt)
                Machine(buttonA, buttonB, prize)

    def coinsToWin(machine: Machine, applyPressLimit: Boolean): Long =
        val Bs     = machine.buttonB.y * machine.buttonA.x - machine.buttonB.x * machine.buttonA.y
        val equals = machine.prize.y * machine.buttonA.x - machine.prize.x * machine.buttonA.y
        val B      = BigDecimal(equals) / Bs
        val A      = (BigDecimal(machine.prize.x) - (B * machine.buttonB.x)) / machine.buttonA.x
        if (!applyPressLimit || A <= 100) && (!applyPressLimit || B <= 100) && A >= 0 && B >= 0 && A.isWhole && B.isWhole
        then A.toLong * 3 + B.toLong
        else 0
