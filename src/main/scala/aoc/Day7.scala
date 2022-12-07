package aoc

import aoc.Common.timed

import scala.annotation.tailrec
import scala.io.Source

object Day7:
  def main(args: Array[String]): Unit =
    val lines = Source.fromResource("day7.txt").getLines().toList
    timed("Part 1", part1(lines))
    timed("Part 2", part2(lines))

  val cdR = "\\$ cd (.+)".r

  @tailrec
  def unfoldPath[A](path: List[String], initial: A, unfold: (A, List[String]) => A): A =
    if (path.isEmpty) initial
    else unfoldPath(path.tail, unfold(initial, path), unfold)

  @tailrec
  def sizeOfDir(
    input: List[String],
    currentPath: List[String],
    result: Map[List[String], Int]
  ): Map[List[String], Int] =
    input match
      case head :: tail =>
        if (head == "$ cd ..") sizeOfDir(tail, currentPath.tail, result)
        else if (head.head.isDigit)
          val Array(size, _) = head.split(' ')
          val newResult = unfoldPath(currentPath, result, (acc, p) => acc + (p -> (acc.getOrElse(p, 0) + size.toInt)))
          sizeOfDir(tail, currentPath, newResult)
        else
          cdR.findFirstMatchIn(head) match
            case Some(m) => sizeOfDir(tail, m.group(1) +: currentPath, result)
            case None    => sizeOfDir(tail, currentPath, result)
      case Nil => result

  def part1(lines: List[String]): Int =
    sizeOfDir(lines, List.empty, Map.empty)
      .filter(_._2 <= 100000)
      .values
      .sum

  def part2(lines: List[String]): Int =
    val dirs           = sizeOfDir(lines, List.empty, Map.empty)
    val availableSpace = 70000000 - dirs(List("/"))
    val toCleanup      = 30000000 - availableSpace
    dirs.filter(_._2 >= toCleanup).minBy(_._2)._2
