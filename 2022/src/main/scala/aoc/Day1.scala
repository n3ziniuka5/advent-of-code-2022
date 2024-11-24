package aoc

import aoc.Common.timed

import scala.collection.mutable
import scala.io.Source
import scala.annotation.tailrec
import aoc.Common.enqueueAndKeepMaxSize

object Day1 {
  def main(args: Array[String]): Unit = {
    val lines = Source.fromResource("day1.txt").getLines().toList
    timed("Part 1", part1(lines))
    timed("Part 2", part2(lines))
  }

  def part1(lines: List[String]): Long = {
    @tailrec
    def loop(lines: List[String], currentElf: Long, mostCalories: Long): Long = {
      lines match
        case head :: tail =>
          if (head == "") {
            loop(tail, 0, math.max(currentElf, mostCalories))
          } else {
            loop(tail, currentElf + head.toLong, mostCalories)
          }
        case Nil => math.max(currentElf, mostCalories)
    }

    loop(lines, 0, 0)
  }

  def part2(lines: List[String]): Long = {
    @tailrec
    def loop(
      lines: List[String],
      currentElf: Long,
      mostCalories: mutable.PriorityQueue[Long]
    ): mutable.PriorityQueue[Long] = {
      lines match
        case head :: tail =>
          if (head == "") {
            loop(tail, 0, mostCalories.enqueueAndKeepMaxSize(currentElf, 3))
          } else {
            loop(tail, currentElf + head.toLong, mostCalories)
          }
        case Nil =>
          mostCalories.enqueueAndKeepMaxSize(currentElf, 3)
    }

    val top3Elves = loop(lines, 0, new mutable.PriorityQueue[Long]()(Ordering[Long].reverse))
    top3Elves.sum
  }
}
