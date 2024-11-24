package aoc

import aoc.Common.timed

import java.util
import scala.annotation.tailrec
import scala.io.Source
import scala.collection.immutable.SortedSet

object Day14:
  def main(args: Array[String]): Unit =
    val lines = Source.fromResource("day14.txt").getLines().toList
    timed("Part 1", part1(lines))
    timed("Part 2", part2(lines))

  case class Point(x: Int, y: Int)
  case class Range(from: Point, to: Point)

  def parseRanges(lines: List[String]): List[Range] =
    lines.flatMap { l =>
      l.split(" -> ")
        .map { s =>
          val Array(x, y) = s.split(',').map(_.toInt)
          Point(x, y)
        }
        .sliding(2)
        .map { rangeList =>
          val sorted = rangeList.sortBy(p => (p.x, p.y))
          Range(sorted(0), sorted(1))
        }
    }

  def parseBlocks(ranges: List[Range], withInfiniteFloor: Boolean): Map[Int, SortedSet[Int]] =
    val defaultSet = if (withInfiniteFloor) {
      val highestY = ranges.map(_.to.y).max
      SortedSet(highestY + 2)
    } else {
      SortedSet.empty[Int]
    }

    ranges.foldLeft(Map.empty[Int, SortedSet[Int]].withDefaultValue(defaultSet)) { (acc, range) =>
      (for {
        x <- range.from.x to range.to.x
        y <- range.from.y to range.to.y
      } yield Point(x, y)).foldLeft(acc) { (acc, point) =>
        acc + (point.x -> (acc(point.x) + point.y))
      }
    }

  def dropAt(point: Point, blocks: Map[Int, SortedSet[Int]]): Option[Point] =
    val dropOnTopOf = blocks(point.x).dropWhile(_ < point.y).headOption

    dropOnTopOf
      .flatMap { y =>
        if (!blocks(point.x - 1).contains(y)) {
          dropAt(Point(point.x - 1, y), blocks)
        } else if (!blocks(point.x + 1).contains(y)) {
          dropAt(Point(point.x + 1, y), blocks)
        } else {
          Some(point.copy(y = y - 1))
        }
      }

  @tailrec
  def keepDroppingSand(turns: Int, blocks: Map[Int, SortedSet[Int]]): Int =
    val sandDropsFrom = Point(500, 0)
    dropAt(sandDropsFrom, blocks) match
      case Some(point) =>
        if (point == sandDropsFrom)
          turns + 1
        else
          keepDroppingSand(turns + 1, blocks + (point.x -> (blocks(point.x) + point.y)))
      case None => turns

  def solve(lines: List[String], withInfiniteFloor: Boolean): Int =
    val blocks = parseBlocks(parseRanges(lines), withInfiniteFloor)
    keepDroppingSand(0, blocks)

  def part1(lines: List[String]): Int =
    solve(lines, false)

  def part2(lines: List[String]): Int =
    solve(lines, true)
