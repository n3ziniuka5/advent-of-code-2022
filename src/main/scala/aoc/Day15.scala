package aoc

import aoc.Common.timed

import scala.io.Source

object Day15:
  def main(args: Array[String]): Unit =
    val lines = Source.fromResource("day15.txt").getLines().toList
    timed("Part 1", part1(lines, 2000000))
    timed("Part 2", part2(lines))

  case class Point(x: Long, y: Long):
    def distanceTo(other: Point): Long = math.abs(x - other.x) + math.abs(y - other.y)

  case class Sensor(at: Point, closestBeacon: Point)

  def parse(line: String): Sensor =
    line match
      case s"Sensor at x=$x, y=$y: closest beacon is at x=$beaconX, y=$beaconY" =>
        Sensor(Point(x.toLong, y.toLong), Point(beaconX.toLong, beaconY.toLong))

  def impossibleBeaconWhere(sensor: Sensor, targetY: Int): Set[Point] =
    val manhattan          = sensor.at.distanceTo(sensor.closestBeacon)
    val manhattanToY       = sensor.at.distanceTo(sensor.at.copy(y = targetY))
    val remainingManhattan = manhattan - manhattanToY

    println(
      s"computing impossible beacon locations for distance $manhattan. Manhattan budget $remainingManhattan"
    )

    if (remainingManhattan >= 0) {
      (-remainingManhattan to remainingManhattan).map { dX =>
        Point(sensor.at.x + dX, targetY)
      }.toSet - sensor.closestBeacon
    } else {
      Set.empty
    }

  def part1(lines: List[String], targetY: Int): Int =
    lines.map(parse).foldLeft(Set.empty[Point])(_ ++ impossibleBeaconWhere(_, targetY)).size

  def part2(lines: List[String]): Int =
    0
