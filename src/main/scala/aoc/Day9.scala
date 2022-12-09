package aoc

import aoc.Common.timed

import scala.annotation.tailrec
import scala.io.Source

object Day9:
  def main(args: Array[String]): Unit =
    val lines = Source.fromResource("day9.txt").getLines().toList
    timed("Part 1", part1(lines))
    timed("Part 2", part2(lines))

  case class Pos(x: Int, y: Int)

  def explodeInput(lines: List[String]): List[Char] =
    lines.flatMap { l =>
      val Array(direction, n) = l.split(' ')
      List.fill(n.toInt)(direction.head)
    }

  def touching(h: Pos, t: Pos): Boolean =
    val hNeighbors = for {
      x <- (-1 to 1).toSet
      y <- (-1 to 1).toSet
    } yield h.copy(x = h.x + x, y = h.y + y)

    hNeighbors.contains(t)

  def move(p: Pos, direction: Char): Pos =
    direction match
      case 'U' => p.copy(y = p.y + 1)
      case 'D' => p.copy(y = p.y - 1)
      case 'L' => p.copy(x = p.x - 1)
      case 'R' => p.copy(x = p.x + 1)

  def follow(positions: List[Pos]): List[Pos] =
    @tailrec
    def loop(positions: List[Pos], headPos: Pos, tailPos: Pos, tailVisited: List[Pos]): List[Pos] =
      positions match
        case head :: tail =>
          if (touching(head, tailPos)) loop(tail, head, tailPos, tailVisited)
          else
            val newTailPos =
              if (head.x == tailPos.x)
                tailPos.copy(y = (head.y + tailPos.y) / 2)
              else if (head.y == tailPos.y)
                tailPos.copy(x = (head.x + tailPos.x) / 2)
              else if (head.y > tailPos.y && head.x > tailPos.x)
                tailPos.copy(x = tailPos.x + 1, y = tailPos.y + 1)
              else if (head.y > tailPos.y && head.x < tailPos.x)
                tailPos.copy(x = tailPos.x - 1, y = tailPos.y + 1)
              else if (head.y < tailPos.y && head.x < tailPos.x)
                tailPos.copy(x = tailPos.x - 1, y = tailPos.y - 1)
              else if (head.y < tailPos.y && head.x > tailPos.x)
                tailPos.copy(x = tailPos.x + 1, y = tailPos.y - 1)
              else
                headPos
            loop(tail, head, newTailPos, newTailPos +: tailVisited)
        case Nil =>
          println(s"ANS ${tailVisited.reverse}")
          tailVisited.reverse

    loop(positions, positions.head, positions.head, List(positions.head))

  def followNthKnot(lines: List[String], knot: Int): List[Pos] =
    val headPositions = explodeInput(lines).scanLeft(Pos(0, 0))(move)
    (1 to knot).foldLeft(headPositions)((a, _) => follow(a))

  def part1(lines: List[String]): Int =
    followNthKnot(lines, 1).distinct.size

  def part2(lines: List[String]): Int =
    followNthKnot(lines, 9).distinct.size
