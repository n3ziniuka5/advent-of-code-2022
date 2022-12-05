package aoc

import aoc.Common.timed

import scala.annotation.tailrec
import scala.io.Source

object Day5:
  def main(args: Array[String]): Unit =
    val lines = Source.fromResource("day5.txt").getLines().toList
    timed("Part 1", part1(lines))
    timed("Part 2", part2(lines))

  case class Move(num: Int, from: Int, to: Int)

  val moveRegex = "move (\\d+) from (\\d+) to (\\d+)".r

  def parseMoves(str: String): Move =
    val regexMatch = moveRegex.findFirstMatchIn(str).head
    (Move(regexMatch.group(1).toInt, regexMatch.group(2).toInt - 1, regexMatch.group(3).toInt - 1))

  def readInput(lines: List[String]): (Vector[List[Char]], List[Move]) =
    @tailrec
    def loop(lines: List[String], stacks: Vector[List[Char]]): (Vector[List[Char]], List[Move]) =
      lines match
        case head :: tail =>
          if (head.charAt(1) == '1') {
            (stacks.map(_.reverse), tail.tail.map(parseMoves))
          } else {
            val toAdd = head.sliding(3, 4).map { s =>
              if (s.trim.isEmpty) None
              else Some(s.charAt(1))
            }

            val newStacks = stacks.zip(toAdd).map { (stack, toAdd) =>
              toAdd.toList ++ stack
            }

            loop(tail, newStacks)
          }

    val numCrates = lines.head.sliding(3, 4).size
    loop(lines, Vector.fill(numCrates)(List.empty))

  @tailrec
  def performMoves(moves: List[Move], stacks: Vector[List[Char]]): Vector[List[Char]] =
    moves match
      case head :: tail =>
        performMoves(
          tail,
          stacks
            .updated(head.from, stacks(head.from).drop(head.num))
            .updated(head.to, stacks(head.from).take(head.num) ++ stacks(head.to))
        )
      case Nil => stacks

  def explodeMoves(moves: List[Move]): List[Move] =
    moves.flatMap { m =>
      List.fill(m.num)(m.copy(num = 1))
    }

  def part1(lines: List[String]): String =
    val (initialStacks, moves) = readInput(lines)
    performMoves(explodeMoves(moves), initialStacks)
      .map(_.head)
      .mkString

  def part2(lines: List[String]): String =
    val (initialStacks, moves) = readInput(lines)
    performMoves(moves, initialStacks)
      .map(_.head)
      .mkString
