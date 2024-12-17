package aoc

import aoc.Common.timed
import language.experimental.namedTuples

object Day17:
    def main(args: Array[String]): Unit =
        val lines = InputUtils.fetchInput(2024, 17)
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    def part1(lines: List[String]): String =
        val (a, b, c, instructions) = parseInput(lines)
        runProgram(a, b, c, instructions, 0, Vector.empty, onlyOneIteration = false).output.mkString(",")

    def part2(lines: List[String]): Long =
        val (_, _, _, instructions) = parseInput(lines)
        findLowestA(instructions)

    def runProgram(
        a: Long,
        b: Long,
        c: Long,
        instructions: Vector[Long],
        pointer: Int,
        output: Vector[Long],
        onlyOneIteration: Boolean
    ): (a: Long, b: Long, c: Long, output: Vector[Long]) =
        if pointer >= instructions.length then (a, b, c, output)
        else
            val literalOperand = instructions(pointer + 1)
            val comboOperand: Long = instructions(pointer + 1) match
                case 4 => a
                case 5 => b
                case 6 => c
                case n => n

            instructions(pointer) match
                case 0 =>
                    val res = a / (math.pow(2, comboOperand.toDouble).toLong)
                    runProgram(res, b, c, instructions, pointer + 2, output, onlyOneIteration)
                case 1 =>
                    val res = b ^ literalOperand
                    runProgram(a, res, c, instructions, pointer + 2, output, onlyOneIteration)
                case 2 =>
                    val res = comboOperand % 8
                    runProgram(a, res, c, instructions, pointer + 2, output, onlyOneIteration)
                case 3 =>
                    if a == 0 || onlyOneIteration then
                        runProgram(a, b, c, instructions, pointer + 2, output, onlyOneIteration)
                    else runProgram(a, b, c, instructions, literalOperand.toInt, output, onlyOneIteration)
                case 4 =>
                    val res = b ^ c
                    runProgram(a, res, c, instructions, pointer + 2, output, onlyOneIteration)
                case 5 =>
                    val out = comboOperand % 8
                    runProgram(a, b, c, instructions, pointer + 2, output :+ out, onlyOneIteration)
                case 6 =>
                    val res = a / (math.pow(2, comboOperand.toDouble).toLong)
                    runProgram(a, res, c, instructions, pointer + 2, output, onlyOneIteration)
                case 7 =>
                    val res = a / (math.pow(2, comboOperand.toDouble).toLong)
                    runProgram(a, b, res, instructions, pointer + 2, output, onlyOneIteration)

    def findLowestA(instructions: Vector[Long]): Long =
        def findLowestAForInstruction(remaining: List[Long], requiredA: Long): Option[Long] =
            val candidates = findPotentialAValues(instructions, requiredA, remaining.head)
            if remaining.tail.isEmpty then candidates.minOption
            else
                candidates
                    .flatMap: candidate =>
                        findLowestAForInstruction(remaining.tail, candidate)
                    .minOption

        findLowestAForInstruction(instructions.toList.reverse, 0).get

    def findPotentialAValues(instructions: Vector[Long], aAtEndMustBe: Long, outputMustBe: Long): List[Long] =
        val start = aAtEndMustBe * 8
        val end   = start + 7
        (start to end).toList.filter: potentialA =>
            val (a, _, _, output) = runProgram(potentialA, 0, 0, instructions, 0, Vector.empty, onlyOneIteration = true)
            output.head == outputMustBe && a == aAtEndMustBe

    def parseInput(lines: List[String]): (Long, Long, Long, Vector[Long]) =
        val a = lines(0).dropWhile(!_.isDigit).toLong
        val b = lines(1).dropWhile(!_.isDigit).toLong
        val c = lines(2).dropWhile(!_.isDigit).toLong

        val instructions = lines(4).drop("Program: ".length()).split(',').map(_.toLong).toVector
        (a, b, c, instructions)
