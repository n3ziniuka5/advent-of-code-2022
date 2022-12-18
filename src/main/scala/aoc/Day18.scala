package aoc

import aoc.Common.timed

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.io.Source

object Day18:
  def main(args: Array[String]): Unit =
    val lines = Source.fromResource("day18.txt").getLines().toList
    timed("Part 1", part1(lines))
    timed("Part 2", part2(lines))

  case class Cube(x: Int, y: Int, z: Int):
    def neighbors: List[Cube] =
      List(
        this.copy(x = this.x - 1),
        this.copy(x = this.x + 1),
        this.copy(y = this.y - 1),
        this.copy(y = this.y + 1),
        this.copy(z = this.z - 1),
        this.copy(z = this.z + 1)
      )

  def sidesNotTouching(cube: Cube, otherCubes: Set[Cube]): Int =
    6 - cube.neighbors.count(otherCubes.contains)

  def isAirPocket(pocket: Cube, cubes: Set[Cube], notAirPockets: Set[Cube]): (Boolean, Set[Cube]) =
    @tailrec
    def loop(queue: Queue[Cube], visited: Set[Cube]): (Boolean, Set[Cube]) =
      queue.dequeueOption match
        case Some((current, remainingQueue)) =>
          if (visited.contains(current)) loop(remainingQueue, visited)
          else if (current == Cube(0, 0, 0)) (false, visited)
          else if (notAirPockets.contains(current)) (false, visited)
          else
            val toVisit = current.neighbors.filterNot(cubes.contains).filterNot(visited.contains)
            loop(remainingQueue.enqueueAll(toVisit), visited + current)

        case None => (true, visited)

    loop(Queue(pocket), Set.empty)

  def findAirPockets(cubeSet: Set[Cube]): Set[Cube] =
    val potentialPockets = cubeSet.toList.flatMap(_.neighbors).filterNot(cubeSet.contains)

    potentialPockets
      .foldLeft((Set.empty[Cube], Set.empty[Cube])) { case ((airPockets, notAirPockets), maybePocket) =>
        if (airPockets.contains(maybePocket) || notAirPockets.contains(maybePocket)) (airPockets, notAirPockets)
        else
          val (isPocket, visited) = isAirPocket(maybePocket, cubeSet, notAirPockets)
          if (isPocket)
            (airPockets ++ visited, notAirPockets)
          else
            (airPockets, notAirPockets ++ visited)
      }
      ._1

  def parse(lines: List[String]): List[Cube] =
    lines.map { l =>
      val Array(x, y, z) = l.split(',').map(_.toInt)
      Cube(x, y, z)
    }

  def part1(lines: List[String]): Int =
    val cubes   = parse(lines)
    val cubeSet = cubes.toSet
    cubes.map(sidesNotTouching(_, cubeSet)).sum

  def part2(lines: List[String]): Int =
    val cubes      = parse(lines)
    val cubeSet    = cubes.toSet
    val airPockets = findAirPockets(cubeSet)
    cubes.map(sidesNotTouching(_, cubeSet ++ airPockets)).sum
