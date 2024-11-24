package aoc

import aoc.Common.timed

import scala.io.Source

object Day8:
  def main(args: Array[String]): Unit =
    val lines = Source.fromResource("day8.txt").getLines().toList
    timed("Part 1", part1(lines))
    timed("Part 2", part2(lines))

  def readInput(lines: List[String]): Vector[Vector[Int]] =
    lines.map(_.toCharArray.toVector.map(_.toString.toInt)).toVector

  def visibleUp(map: Vector[Vector[Int]], i: Int, j: Int): Boolean =
    i == 0 || (0 until i).forall(k => map(k)(j) < map(i)(j))

  def visibleDown(map: Vector[Vector[Int]], i: Int, j: Int): Boolean =
    i == map.size - 1 || (i + 1 until map.size).forall(k => map(k)(j) < map(i)(j))

  def visibleLeft(map: Vector[Vector[Int]], i: Int, j: Int): Boolean =
    j == 0 || (0 until j).forall(k => map(i)(k) < map(i)(j))

  def visibleRight(map: Vector[Vector[Int]], i: Int, j: Int): Boolean =
    j == map(0).size - 1 || (j + 1 until map(0).size).forall(k => map(i)(k) < map(i)(j))

  def viewingDistanceUp(map: Vector[Vector[Int]], i: Int, j: Int): Int =
    if (i == 0) 0
    else (i - 1 until 0 by -1).takeWhile(k => map(k)(j) < map(i)(j)).size + 1

  def viewingDistanceDown(map: Vector[Vector[Int]], i: Int, j: Int): Int =
    if (i == map.size - 1) 0
    else (i + 1 until map.size - 1).takeWhile(k => map(k)(j) < map(i)(j)).size + 1

  def viewingDistanceLeft(map: Vector[Vector[Int]], i: Int, j: Int): Int =
    if (j == 0) 0
    else (j - 1 until 0 by -1).takeWhile(k => map(i)(k) < map(i)(j)).size + 1

  def viewingDistanceRight(map: Vector[Vector[Int]], i: Int, j: Int): Int =
    if (j == map(0).size - 1) 0
    else (j + 1 until map(0).size - 1).takeWhile(k => map(i)(k) < map(i)(j)).size + 1

  def part1(lines: List[String]): Int =
    val map = readInput(lines)
    (for {
      i <- map.indices
      j <- map(0).indices
    } yield (i, j)).count { (i, j) =>
      visibleUp(map, i, j) ||
      visibleDown(map, i, j) ||
      visibleLeft(map, i, j) ||
      visibleRight(map, i, j)
    }

  def part2(lines: List[String]): Int =
    val map = readInput(lines)
    (for {
      i <- map.indices
      j <- map(0).indices
    } yield (i, j)).map { (i, j) =>
      viewingDistanceUp(map, i, j) *
        viewingDistanceDown(map, i, j) *
        viewingDistanceLeft(map, i, j) *
        viewingDistanceRight(map, i, j)
    }.max
