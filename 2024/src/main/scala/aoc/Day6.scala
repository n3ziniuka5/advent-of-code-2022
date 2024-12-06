package aoc

import aoc.Common.timed

import scala.collection.mutable
import scala.io.Source
import scala.annotation.tailrec
import scala.language.experimental.namedTuples

object Day6:
    def main(args: Array[String]): Unit =
        val lines = InputUtils.fetchInput(2024, 6)
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    enum Direction:
        case Up, Down, Left, Right

        def turn: Direction = this match
            case Up    => Right
            case Right => Down
            case Down  => Left
            case Left  => Up

    def part1(lines: List[String]): Long =
        val (map, start) = parse(lines)
        travel(map, start, Direction.Up).visited.size

    def part2(lines: List[String]): Long =
        val (map, start) = parse(lines)
        travel(map, start, Direction.Up).visited.count: wallPoint =>
            if wallPoint == start then false
            else
                val updatedMap = Map2d.apply(map.underlying.updated(wallPoint, '#'))
                travel(updatedMap, start, Direction.Up).inLoop

    def parse(lines: List[String]): (map: Map2d[Char], start: Point) =
        val map   = Map2d.fromLines(lines)
        val start = map.underlying.find((_, v) => v == '^').get._1
        (Map2d(map.underlying.updated(start, '.')), start)

    def travel(map: Map2d[Char], start: Point, direction: Direction): (visited: Set[Point], inLoop: Boolean) =
        def loop(current: Point, direction: Direction, visited: Set[(Point, Direction)]): (Set[Point], Boolean) =
            val next = direction match
                case Direction.Up    => current.up
                case Direction.Down  => current.down
                case Direction.Left  => current.left
                case Direction.Right => current.right
            val newVisited = visited + ((current, direction))
            if visited.contains((current, direction)) then (newVisited.map(_._1), true)
            else if map.get(next).contains('.') then loop(next, direction, newVisited)
            else if map.get(next).contains('#') then loop(current, direction.turn, newVisited)
            else (newVisited.map(_._1), false)

        loop(start, direction, Set.empty)
