package aoc

import aoc.Common.timed

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object Day12:
  def main(args: Array[String]): Unit =
    val lines = Source.fromResource("day12.txt").getLines().toList
    timed("Part 1", part1(lines))
    timed("Part 2", part2(lines))

  val elevation = {
    val initial = ('a' to 'z').zipWithIndex.toMap
    initial ++ Map('S' -> initial('a'), 'E' -> initial('z'))
  }

  def parse(lines: List[String]): Vector[Vector[Char]] =
    lines.map(_.toCharArray.toVector).toVector

  def findStart(map: Vector[Vector[Char]]): (Int, Int) =
    (for {
      x <- map.indices
      y <- map(x).indices
    } yield (x, y)).find { (x, y) =>
      map(x)(y) == 'S'
    }.get

  def neighbors(map: Vector[Vector[Char]], x: Int, y: Int, visited: Set[(Int, Int)]): List[(Int, Int)] =
    List((x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1))
      .filter { (x, y) =>
        x >= 0 && x < map.size && y >= 0 && y < map(0).size
      }
      .filterNot(visited)
      .filter { (tX, tY) =>
        elevation(map(tX)(tY)) - elevation(map(x)(y)) <= 1
      }

  @tailrec
  def shortestPath(
    toVisit: mutable.PriorityQueue[((Int, Int), Int)],
    map: Vector[Vector[Char]],
    visited: Set[(Int, Int)]
  ): Int =
    if (toVisit.isEmpty) Int.MaxValue
    else
      val ((x, y), steps) = toVisit.dequeue()
      if (map(x)(y) == 'E') steps
      else if (visited.contains((x, y))) shortestPath(toVisit, map, visited)
      else
        val initialNodes = neighbors(map, x, y, visited)
        toVisit.enqueue(initialNodes.map((x, y) => ((x, y), steps + 1)): _*)
        shortestPath(toVisit, map, visited + ((x, y)))

  def solve(map: Vector[Vector[Char]], start: (Int, Int)): Int =
    val initialNodes = neighbors(map, start._1, start._2, Set.empty)
    val nodeQueue    = new mutable.PriorityQueue[((Int, Int), Int)]()(Ordering.by(-_._2))
    nodeQueue.enqueue(initialNodes.map((x, y) => ((x, y), 1)): _*)

    shortestPath(
      nodeQueue,
      map,
      Set((start._1, start._2))
    )

  def part1(lines: List[String]): Int =
    val map   = parse(lines)
    val start = findStart(map)
    solve(map, start)

  def part2(lines: List[String]): Int =
    val map = parse(lines)
    val startingPositions = (for {
      x <- map.indices
      y <- map(x).indices
    } yield (x, y)).filter { (x, y) =>
      map(x)(y) == 'S' || map(x)(y) == 'a'
    }

    startingPositions.map(solve(map, _)).min
