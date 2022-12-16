package aoc

import aoc.Common.timed
import aoc.Day16Types.{Minute, Pressure}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

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
    remainingValves: Map[String, Valve]
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

  def parse(lines: List[String]): Map[String, Valve] =
    lines.map {
      case s"Valve $valve has flow rate=$flowRateString; tunnel leads to valve $leadsTo" =>
        valve -> Valve(flowRateString.toInt, List(leadsTo))
      case s"Valve $valve has flow rate=$flowRateString; tunnels lead to valves $leadsTo" =>
        valve -> Valve(flowRateString.toInt, leadsTo.split(", ").toList)
    }.toMap

  def solve(lines: List[String]): Pressure =
    @tailrec
    def loop(
      queue: mutable.PriorityQueue[(VisitState, Minute, Pressure)],
      visited: Set[VisitState],
      valves: Map[String, Valve]
    ): Pressure =
      val (visitState, timeRemaining, pressureReleased) = queue.dequeue()
      if (timeRemaining == 0) pressureReleased
      // else if (visited.contains(visitState)) loop(queue, visited, valves)
      else
        val tryToOpen = visitState.remainingValves.map { case (pos, v) =>
          val a = timeToOpen(visitState.position, pos, valves)
          (
            VisitState(pos, visitState.openValves + v, visitState.remainingValves - pos),
            timeRemaining - a,
            pressureReleased + Pressure(visitState.openValves.map(_.flowRate).sum * a.asInt)
          )
        }
          // .filter(_._2 > Minute(0))
          // .filterNot(s => visited.contains(s._1))
          .toList
        queue.enqueue(tryToOpen: _*)
        queue.enqueue(
          (
            visitState,
            Minute(0),
            pressureReleased + Pressure(visitState.openValves.map(_.flowRate).sum * timeRemaining.asInt)
          )
        )

        loop(queue, visited + visitState, valves)

    val valves = parse(lines)
    val toOpen = valves.filter(_._2.flowRate != 0)
    loop(
      mutable
        .PriorityQueue((VisitState("AA", Set.empty, toOpen), Minute(30), Pressure(0)))(Ordering.by(a => (a._2, a._3))),
      Set.empty,
      valves
    )

  def part1(lines: List[String]): Pressure =
    solve(lines)

  def part2(lines: List[String]): Pressure =
    Pressure(0)
