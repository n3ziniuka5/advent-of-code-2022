package aoc

import aoc.Common.timed
import aoc.Day16Types.{Minute, Pressure}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.collection.parallel.CollectionConverters._

object Day16Types:
  opaque type Minute = Int
  object Minute:
    def apply(i: Int): Minute   = i
    given Ordering[Minute]      = Ordering.Int
    given CanEqual[Minute, Int] = CanEqual.derived

    extension (m: Minute)
      def +(other: Minute): Minute  = m + other
      def -(other: Minute): Minute  = m - other
      def >(other: Minute): Boolean = m > other
      def unary_-                   = -m
      def asInt: Int                = m

  opaque type Pressure = Int
  object Pressure:
    def apply(i: Int): Pressure   = i
    given Ordering[Pressure]      = Ordering.Int
    given CanEqual[Pressure, Int] = CanEqual.derived

    extension (p: Pressure) def +(other: Pressure): Pressure = p + other

object Day16:
  def main(args: Array[String]): Unit =
    val lines = Source.fromResource("day16.txt").getLines().toList
    timed("Part 1", part1(lines))
    timed("Part 2", part2(lines))

  case class VisitState(
    position: String,
    openValves: Set[Valve],
    remainingValves: Map[String, Valve],
    visited: Int
  )
  case class Valve(flowRate: Int, leadsTo: List[String])

  def timeToOpen(from: String, to: String, valves: Map[String, Valve]): Minute =
    @tailrec
    def loop(queue: mutable.PriorityQueue[(String, Minute)], visited: Set[String]): Minute =
      val (pos, timeSpent) = queue.dequeue()
      if (pos == to) timeSpent + Minute(1)
      else if (visited.contains(pos)) loop(queue, visited)
      else
        val addedState = valves(pos).leadsTo.filterNot(visited.contains).map { to =>
          (to, timeSpent + Minute(1))
        }
        queue.enqueue(addedState: _*)
        loop(queue, visited + pos)

    loop(mutable.PriorityQueue((from, Minute(0)))(Ordering.by(-_._2)), Set.empty)

  def distanceMap(valves: Map[String, Valve]): Map[(String, String), Minute] =
    (for {
      from <- valves.keySet
      to   <- valves.keySet if from != to
    } yield (from, to)).map((from, to) => (from, to) -> timeToOpen(from, to, valves)).toMap

  def parse(lines: List[String]): Map[String, Valve] =
    lines.map {
      case s"Valve $valve has flow rate=$flowRateString; tunnel leads to valve $leadsTo" =>
        valve -> Valve(flowRateString.toInt, List(leadsTo))
      case s"Valve $valve has flow rate=$flowRateString; tunnels lead to valves $leadsTo" =>
        valve -> Valve(flowRateString.toInt, leadsTo.split(", ").toList)
    }.toMap

  def solve(
    valves: Map[String, Valve],
    toOpen: Map[String, Valve],
    startingTime: Minute,
    distanceMap: Map[(String, String), Minute]
  ): (Pressure, Int) =
    @tailrec
    def loop(
      queue: mutable.PriorityQueue[(VisitState, Minute, Pressure)],
      valves: Map[String, Valve]
    ): (Pressure, Int) =
      val (visitState, timeRemaining, pressureReleased) = queue.dequeue()
      if (timeRemaining == 0) (pressureReleased, visitState.visited)
      else
        val tryToOpen = visitState.remainingValves
          .map { case (pos, v) =>
            val minutesToMove = distanceMap((visitState.position, pos))
            (
              VisitState(pos, visitState.openValves + v, visitState.remainingValves - pos, visitState.visited + 1),
              timeRemaining - minutesToMove,
              pressureReleased + Pressure(visitState.openValves.map(_.flowRate).sum * minutesToMove.asInt)
            )
          }
          .filter(_._2 > Minute(0))
          .toList
        queue.enqueue(tryToOpen: _*)

        if (tryToOpen.isEmpty)
          queue.enqueue(
            (
              visitState,
              Minute(0),
              pressureReleased + Pressure(visitState.openValves.map(_.flowRate).sum * timeRemaining.asInt)
            )
          )

        loop(queue, valves)

    loop(
      mutable
        .PriorityQueue((VisitState("AA", Set.empty, toOpen, 0), startingTime, Pressure(0)))(
          Ordering.by(a => (a._2, a._3))
        ),
      valves
    )

  def part1(lines: List[String]): Pressure =
    val valves = parse(lines)
    solve(valves, valves.filter(_._2.flowRate != 0), Minute(30), distanceMap(valves))._1

  def part2(lines: List[String]): Pressure =
    val valves              = parse(lines)
    val withoutZeroPressure = valves.filter(_._2.flowRate != 0)
    val map                 = distanceMap(valves)

    val (_, workingAlonePaths) = solve(valves, withoutZeroPressure, Minute(26), map)
    val combinationSize =
      if (workingAlonePaths > (withoutZeroPressure.size / 2))
        withoutZeroPressure.size / 2
      else workingAlonePaths + 1

    withoutZeroPressure.toList
      .combinations(combinationSize)
      .toList
      .par
      .map { myPaths =>
        solve(valves, myPaths.toMap, Minute(26), map)._1 +
          solve(valves, withoutZeroPressure -- myPaths.map(_._1), Minute(26), map)._1
      }
      .max
