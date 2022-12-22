package aoc

import aoc.Common.timed
import Day22.Facing.{Down, Left, Right, Up}

import scala.annotation.tailrec
import scala.io.Source

object Day22:
  def main(args: Array[String]): Unit =
    val lines = Source.fromResource("day22.txt").getLines().toList
    timed("Part 1", part1(lines))
    timed("Part 2", part2(lines))

  case class Input(map: Map[(Int, Int), Char], directions: List[Char | Int], numRows: Int, numCols: Int)

  enum Facing:
    case Left, Right, Up, Down

  def parseDirections(str: String): List[Char | Int] =
    @tailrec
    def loop(str: String, acc: List[Char | Int]): List[Char | Int] =
      val numStr = str.takeWhile(_.isDigit)
      if (str.isEmpty)
        acc.reverse
      else if (numStr.nonEmpty)
        loop(str.drop(numStr.length), numStr.toInt +: acc)
      else
        loop(str.tail, str.head +: acc)

    loop(str, Nil)

  def parse(lines: List[String]): Input =
    def loop(lines: List[String], row: Int, acc: Input): Input =
      lines match
        case head :: tail =>
          if head.nonEmpty then
            val newMapEntries = head.toCharArray.zipWithIndex.flatMap { case (char, col) =>
              char match
                case ' '   => None
                case other => Some(((row, col + 1), other))
            }.toMap

            loop(tail, row + 1, acc.copy(map = acc.map ++ newMapEntries))
          else
            val directions = parseDirections(tail.head)
            loop(Nil, row, acc.copy(directions = directions, numRows = row - 1))

        case Nil => acc

    loop(lines, 1, Input(Map.empty, Nil, 1, lines.head.length))

  def plainTransition(input: Input, row: Int, col: Int, facing: Facing): (Int, Int, Facing) =
    facing match
      case Right =>
        val c = (1 to input.numCols).find(c => input.map.contains((row, c))).get
        (row, c, facing)
      case Left =>
        val c = (input.numCols to 1 by -1).find(c => input.map.contains((row, c))).get
        (row, c, facing)
      case Up =>
        val r = (input.numRows to 1 by -1).find(r => input.map.contains((r, col))).get
        (r, col, facing)
      case Down =>
        val r = (1 to input.numRows).find(r => input.map.contains((r, col))).get
        (r, col, facing)

  @tailrec
  def solve(
    input: Input,
    currentRow: Int,
    currentCol: Int,
    facing: Facing,
    transition: (Input, Int, Int, Facing) => (Int, Int, Facing)
  ): Int =
    input.directions match
      case (f: Char) :: tail =>
        val newFacing = (f, facing) match
          case ('R', Right) => Down
          case ('R', Left)  => Up
          case ('R', Up)    => Right
          case ('R', Down)  => Left
          case ('L', Right) => Up
          case ('L', Left)  => Down
          case ('L', Up)    => Left
          case ('L', Down)  => Right
          case _            => facing

        solve(input.copy(directions = tail), currentRow, currentCol, newFacing, transition)

      case 0 :: tail =>
        // println("skipping")
        solve(input.copy(directions = tail), currentRow, currentCol, facing, transition)
      case (n: Int) :: tail =>
        val (newRow, newCol, newFacing) = facing match
          case Right =>
            val newCol = currentCol + 1
            if (input.map.contains((currentRow, newCol))) (currentRow, newCol, facing)
            else
              transition(input, currentRow, currentCol, facing)
          case Left =>
            val newCol = currentCol - 1
            if (input.map.contains((currentRow, newCol))) (currentRow, newCol, facing)
            else
              transition(input, currentRow, currentCol, facing)
          case Up =>
            val newRow = currentRow - 1
            if (input.map.contains((newRow, currentCol))) (newRow, currentCol, facing)
            else
              transition(input, currentRow, currentCol, facing)
          case Down =>
            val newRow = currentRow + 1
            if (input.map.contains((newRow, currentCol))) (newRow, currentCol, facing)
            else
              transition(input, currentRow, currentCol, facing)

        if (input.map((newRow, newCol)) == '#')
          solve(input.copy(directions = tail), currentRow, currentCol, facing, transition)
        else
          solve(input.copy(directions = (n - 1) +: tail), newRow, newCol, newFacing, transition)

      case Nil =>
        val facingScore = facing match
          case Right => 0
          case Down  => 1
          case Left  => 2
          case Up    => 3

        1000 * currentRow + 4 * currentCol + facingScore

  def part1(lines: List[String]): Int =
    val input       = parse(lines)
    val startColumn = (1 to input.numCols).find(y => input.map.contains((1, y)) && input.map((1, y)) == '.').get

    solve(input, 1, startColumn, Right, plainTransition)

  def part2(lines: List[String]): Int =
    val input       = parse(lines)
    val startColumn = (1 to input.numCols).find(y => input.map.contains((1, y)) && input.map((1, y)) == '.').get

    solve(input, 1, startColumn, Right, plainTransition)
