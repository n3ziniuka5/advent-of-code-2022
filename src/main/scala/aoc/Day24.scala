package aoc

import aoc.Common.timed

import scala.annotation.tailrec
import scala.collection.immutable.MultiDict
import scala.collection.mutable
import scala.io.Source

object Day24:
  def main(args: Array[String]): Unit =
    val lines = Source.fromResource("day24.txt").getLines().toList
    timed("Part 1", part1(lines))
    timed("Part 2", part2(lines))

  case class Point(x: Int, y: Int):
    def couldTravelTo: List[Point] = List(this, copy(x = x + 1), copy(x = x - 1), copy(y = y + 1), copy(y = y - 1))

  case class Input(start: Point, finish: Point, storm: MultiDict[Point, Char])

  def parse(lines: List[String]): Input =
    val width  = lines.head.length
    val length = lines.size

    val storm = MultiDict.from(lines.zipWithIndex.flatMap { (l, y) =>
      l.zipWithIndex.collect {
        case (c, x) if c != '.' && c != '#' => Point(x, y) -> c
      }
    })

    Input(Point(1, 0), Point(width - 2, length - 1), storm)

  def advanceStorm(storm: MultiDict[Point, Char], start: Point, finish: Point): MultiDict[Point, Char] =
    storm.map { (point, direction) =>
      val newPoint = direction match
        case '>' =>
          val newX = point.x + 1
          if (newX > finish.x) point.copy(x = start.x)
          else point.copy(x = newX)
        case '<' =>
          val newX = point.x - 1
          if (newX < start.x) point.copy(x = finish.x)
          else point.copy(x = newX)
        case 'v' =>
          val newY = point.y + 1
          if (newY == finish.y) point.copy(y = start.y + 1)
          else point.copy(y = newY)
        case '^' =>
          val newY = point.y - 1
          if (newY == start.y) point.copy(y = finish.y - 1)
          else point.copy(y = newY)
      (newPoint, direction)
    }

  def isWithinBounds(p: Point, input: Input): Boolean =
    p.x >= input.start.x &&
      p.x <= input.finish.x &&
      (p.y > input.start.y || p == input.start) &&
      (p.y < input.finish.y || p == input.finish)

  def solve(input: Input, goTo: List[Point]): Int =
    @tailrec
    def loop(
      pq: mutable.PriorityQueue[(Point, Int)],
      checkpoints: List[Point],
      visited: Set[(Point, Int)],
      stormAtMinute: Vector[MultiDict[Point, Char]]
    ): Int =
      val (point, steps) = pq.dequeue()
      if (point == checkpoints.head)
        val remainingCheckpoints = checkpoints.tail
        if (remainingCheckpoints.isEmpty) steps
        else
          pq.clear()
          pq.enqueue((point, steps))
          loop(pq, remainingCheckpoints, Set.empty, stormAtMinute)
      else if (visited.contains((point, steps))) loop(pq, checkpoints, visited, stormAtMinute)
      else
        val addedStep = steps + 1
        val newStorm =
          if (stormAtMinute.size <= addedStep)
            stormAtMinute :+ advanceStorm(stormAtMinute(steps), input.start, input.finish)
          else stormAtMinute

        val travelTo = point.couldTravelTo
          .filter(isWithinBounds(_, input))
          .filterNot(p => newStorm(addedStep).containsKey(p))
          .filterNot(p => visited.contains((p, addedStep)))

        pq.enqueue(travelTo.map((_, addedStep))*)

        loop(pq, checkpoints, visited + ((point, steps)), newStorm)

    val pq = mutable.PriorityQueue((input.start, 0))(Ordering.by(state => -state._2))
    loop(pq, goTo, Set.empty, Vector(input.storm))

  def part1(lines: List[String]): Int =
    val input = parse(lines)
    solve(input, List(input.finish))

  def part2(lines: List[String]): Int =
    val input = parse(lines)
    solve(input, List(input.finish, input.start, input.finish))
