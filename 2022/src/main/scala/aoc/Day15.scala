package aoc

import aoc.Common.timed

import scala.collection.MultiSet
import scala.io.Source

object Day15:
  def main(args: Array[String]): Unit =
    val lines = Source.fromResource("day15.txt").getLines().toList
    timed("Part 1", part1(lines, 2000000))
    timed("Part 2", part2(lines, 4000000))

  case class Point(x: Long, y: Long):
    def distanceTo(other: Point): Long = math.abs(x - other.x) + math.abs(y - other.y)

  case class Sensor(at: Point, closestBeacon: Point)

  def parse(line: String): Sensor =
    line match
      case s"Sensor at x=$x, y=$y: closest beacon is at x=$beaconX, y=$beaconY" =>
        Sensor(Point(x.toLong, y.toLong), Point(beaconX.toLong, beaconY.toLong))

  def impossibleBeaconWhere(sensor: Sensor, targetY: Int): Set[Point] =
    val manhattan       = sensor.at.distanceTo(sensor.closestBeacon)
    val manhattanToY    = sensor.at.distanceTo(sensor.at.copy(y = targetY))
    val manhattanBudget = manhattan - manhattanToY

    if manhattanBudget >= 0 then
      (-manhattanBudget to manhattanBudget).map { dX =>
        Point(sensor.at.x + dX, targetY)
      }.toSet - sensor.closestBeacon
    else Set.empty

  def possibleBeaconLocation(sensor: Sensor, allSensors: List[Sensor], maxCoord: Int): Option[Point] =
    val couldBeAtManhattan = sensor.at.distanceTo(sensor.closestBeacon) + 1
    (-couldBeAtManhattan to couldBeAtManhattan)
      .flatMap(dX =>
        List(
          Point(sensor.at.x + dX, sensor.at.y + (couldBeAtManhattan - math.abs(dX))),
          Point(sensor.at.x + dX, sensor.at.y - (couldBeAtManhattan - math.abs(dX)))
        )
      )
      .find { p =>
        (p.x >= 0 && p.x <= maxCoord && p.y >= 0 && p.y <= maxCoord) &&
        allSensors.forall { s =>
          s.at.distanceTo(p) > s.at.distanceTo(s.closestBeacon)
        }
      }

  def part1(lines: List[String], targetY: Int): Int =
    lines.map(parse).foldLeft(Set.empty[Point])(_ ++ impossibleBeaconWhere(_, targetY)).size

  def part2(lines: List[String], maxCoord: Int): Long =
    val sensors = lines.map(parse)
    val p = LazyList
      .from(sensors)
      .map(possibleBeaconLocation(_, sensors, maxCoord))
      .collectFirst { case Some(p) => p }
      .get

    p.x * 4000000 + p.y
