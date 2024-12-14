package aoc

import aoc.Common.timed
import scala.annotation.tailrec

object Day14:
    case class Robot(position: Point, velocity: Point)

    def main(args: Array[String]): Unit =
        val lines = InputUtils.fetchInput(2024, 14)
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    def part1(lines: List[String]): Long =
        val (robots, maxX, maxY) = parse(lines)

        val afterMoving = robots.map(move(_, maxX, maxY, 100))
        quadrantSum(afterMoving, maxX, maxY)

    def part2(lines: List[String]): Long =
        val (robots, maxX, maxY) = parse(lines)

        findTree(robots, maxX, maxY, 1)

    def parse(lines: List[String]): (robots: List[Robot], maxX: Long, maxY: Long) =
        val robots = lines.map:
            case s"p=$sX,$sY v=$vX,$vY" => Robot(Point(sX.toLong, sY.toLong), Point(vX.toLong, vY.toLong))
        val maxX = robots.maxBy(_.position.x).position.x
        val maxY = robots.maxBy(_.position.y).position.y
        (robots, maxX, maxY)

    def move(robot: Robot, maxX: Long, maxY: Long, seconds: Long): Robot =
        val moveBy      = (robot.velocity * seconds).mod(maxX + 1, maxY + 1)
        val newPosition = wrapAround(robot.position + moveBy, maxX, maxY)
        robot.copy(position = newPosition)

    def wrapAround(p: Point, maxX: Long, maxY: Long): Point =
        val newX =
            if p.x >= 0 then p.x % (maxX + 1)
            else maxX + 1 + p.x
        val newY =
            if p.y >= 0 then p.y % (maxY + 1)
            else maxY + 1 + p.y
        Point(newX, newY)

    def quadrantSum(robots: List[Robot], maxX: Long, maxY: Long): Long =
        @tailrec
        def loop(robots: List[Robot], topLeft: Long, topRight: Long, bottomLeft: Long, bottomRight: Long): Long =
            robots match
                case head :: next =>
                    if head.position.x < (maxX / 2) && head.position.y < (maxY / 2) then
                        loop(next, topLeft + 1, topRight, bottomLeft, bottomRight)
                    else if head.position.x > (maxX / 2) && head.position.y < (maxY / 2) then
                        loop(next, topLeft, topRight + 1, bottomLeft, bottomRight)
                    else if head.position.x < (maxX / 2) && head.position.y > (maxY / 2) then
                        loop(next, topLeft, topRight, bottomLeft + 1, bottomRight)
                    else if head.position.x > (maxX / 2) && head.position.y > (maxY / 2) then
                        loop(next, topLeft, topRight, bottomLeft, bottomRight + 1)
                    else loop(next, topLeft, topRight, bottomLeft, bottomRight)
                case Nil => topLeft * topRight * bottomLeft * bottomRight

        loop(robots, 0L, 0L, 0L, 0L)

    @tailrec
    def findTree(robots: List[Robot], maxX: Long, maxY: Long, attempt: Long): Long =
        val afterMoving    = robots.map(move(_, maxX, maxY, 1))
        val robotPositions = afterMoving.map(_.position).toSet
        val neighborCount  = afterMoving.map(p => p.position.adjacent.count(robotPositions.contains)).sum

        if neighborCount > 500 then
            val map = afterMoving.groupBy(_.position).view.mapValues(_ => '#').toMap
            println(Map2d(map))
            attempt
        else findTree(afterMoving, maxX, maxY, attempt + 1)
