package aoc

import aoc.Common.timed
import scala.collection.mutable.PriorityQueue
import language.experimental.namedTuples
import scala.annotation.tailrec

object Day18:
    def main(args: Array[String]): Unit =
        val lines = InputUtils.fetchInput(2024, 18)
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    def part1(lines: List[String]): Long =
        val (start, target, bytes) = parse(lines)
        val takeFirst              = if target.x == 6 then 12 else 1024
        findEscape(start, target, bytes.take(takeFirst).toSet).get.steps

    def part2(lines: List[String]): String =
        val (start, target, bytes) = parse(lines)
        val firstByte              = findPointAtWhichEscapeCloses(start, target, bytes)
        s"${firstByte.x},${firstByte.y}"

    def findEscape(start: Point, target: Point, bytes: Set[Point]): Option[(steps: Long, path: Set[Point])] =
        @tailrec
        def loop(searches: PriorityQueue[(Point, Long, Set[Point])], visited: Set[Point]): Option[(Long, Set[Point])] =
            if searches.isEmpty then return None

            val (current, steps, path) = searches.dequeue()
            if current == target then Some((steps, path))
            else if visited.contains(current) then loop(searches, visited)
            else
                val next = current.adjacent.filter: p =>
                    !visited.contains(p) &&
                        p.x >= 0 && p.x <= target.x &&
                        p.y >= 0 && p.y <= target.y
                searches.enqueue(next.map((_, steps + 1, path + current))*)
                loop(searches, visited + current)

        loop(PriorityQueue.apply((start, 0L, Set.empty[Point]))(Ordering.by(-_._2)), bytes)

    def findPointAtWhichEscapeCloses(start: Point, target: Point, bytes: List[Point]): Point =
        @tailrec
        def loop(remainingBytes: List[Point], initialVisited: Set[Point], maybeLastPath: Option[Set[Point]]): Point =
            val currentByte = remainingBytes.head
            val newVisited  = initialVisited + currentByte
            maybeLastPath match
                case Some(lastPath) if !lastPath.contains(currentByte) =>
                    loop(remainingBytes.tail, newVisited, maybeLastPath)
                case _ =>
                    val maybeNewPath = findEscape(start, target, newVisited)
                    maybeNewPath match
                        case Some(res) => loop(remainingBytes.tail, newVisited, Some(res.path))
                        case None      => currentByte

        val dropFirst = if target.x == 6 then 12 else 1024
        loop(bytes.drop(dropFirst), bytes.take(dropFirst).toSet, None)

    def parse(lines: List[String]): (start: Point, target: Point, bytes: List[Point]) =
        val bytes = lines.map: line =>
            val Array(x, y) = line.split(',').map(_.toLong)
            Point(x, y)

        (Point(0, 0), Point(bytes.maxBy(_.x).x, bytes.maxBy(_.y).y), bytes)
