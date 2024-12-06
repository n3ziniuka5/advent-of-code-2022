package aoc

import aoc.Common.timed
import aoc.Common.middle
import scala.annotation.tailrec

object Day5:
    def main(args: Array[String]): Unit =
        val lines = InputUtils.fetchInput(2024, 5)
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    def part1(lines: List[String]): Long =
        val (dependencies, instructions) = parse(lines)
        instructions
            .filter(isGoodInstruction(dependencies, _))
            .map(_.middle)
            .sum

    def part2(lines: List[String]): Long =
        val (dependencies, instructions) = parse(lines)
        instructions
            .filter(!isGoodInstruction(dependencies, _))
            .map(fixInstruction(dependencies, _))
            .map(_.middle)
            .sum

    def parse(lines: List[String]): (Map[Int, List[Int]], List[List[Int]]) =
        val dependencyLines = lines.takeWhile(_.nonEmpty)
        val dependencies = dependencyLines.foldLeft(Map.empty[Int, List[Int]]): (acc, line) =>
            val Array(dependsOn, num) = line.split('|').map(_.toInt)
            acc.updated(num, dependsOn +: acc.getOrElse(num, Nil))

        val instructionLines = lines.drop(dependencyLines.length + 1)
        val instructions     = instructionLines.map(_.split(',').map(_.toInt).toList)
        (dependencies, instructions)

    def isGoodInstruction(dependencies: Map[Int, List[Int]], instruction: List[Int]): Boolean =
        val numberSet = instruction.toSet

        @tailrec
        def loop(remainingInstructions: List[Int], printed: Set[Int]): Boolean =
            remainingInstructions match
                case head :: tail =>
                    val dependsOn = dependencies.getOrElse(head, Nil)
                    if dependsOn.forall(n => printed.contains(n) || !numberSet.contains(n)) then
                        loop(tail, printed + head)
                    else false
                case Nil => true

        loop(instruction, Set.empty)

    def fixInstruction(dependencies: Map[Int, List[Int]], instruction: List[Int]): Vector[Int] =
        val numberSet = instruction.toSet

        @tailrec
        def loop(remainingInstructions: List[Int], printed: Set[Int], printedInstruction: Vector[Int]): Vector[Int] =
            remainingInstructions match
                case head :: tail =>
                    val dependsOn = dependencies.getOrElse(head, Nil)
                    dependsOn.find(n => !printed.contains(n) && numberSet.contains(n)) match
                        case Some(n) =>
                            loop(n +: head +: tail.filter(_ != n), printed, printedInstruction)
                        case None => loop(tail, printed + head, printedInstruction :+ head)
                case Nil => printedInstruction

        loop(instruction, Set.empty, Vector.empty)
