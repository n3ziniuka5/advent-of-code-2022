package aoc

import aoc.Common.timed

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
        travel(map, start, Direction.Up, Set.empty).visited.size

    def part2(lines: List[String]): Long =
        val (map, start) = parse(lines)
        numLoops(map, start, Direction.Up, 0)

    def parse(lines: List[String]): (map: Map2d[Char], start: Point) =
        val map   = Map2d.fromLines(lines)
        val start = map.underlying.find((_, v) => v == '^').get._1
        (Map2d(map.underlying.updated(start, '.')), start)

    def move(point: Point, direction: Direction): Point = direction match
        case Direction.Up    => point.up
        case Direction.Down  => point.down
        case Direction.Left  => point.left
        case Direction.Right => point.right

    @tailrec
    def numLoops(
        map: Map2d[Char],
        current: Point,
        direction: Direction,
        foundLoops: Int
    ): Int =
        val next = move(current, direction)
        if map.get(next).contains('.') then
            val wallPlaced             = Map2d.apply(map.underlying.updated(next, '#'))
            val loopExistsIfPlacedHere = travel(wallPlaced, current, direction, Set.empty).inLoop
            val newLoopCount           = if loopExistsIfPlacedHere then foundLoops + 1 else foundLoops
            numLoops(map, next, direction, newLoopCount)
        else if map.get(next).contains('#') then numLoops(map, current, direction.turn, foundLoops)
        else foundLoops

    @tailrec
    def travel(
        map: Map2d[Char],
        current: Point,
        direction: Direction,
        visited: Set[(Point, Direction)]
    ): (visited: Set[Point], inLoop: Boolean) =
        val next       = move(current, direction)
        val newVisited = visited + ((current, direction))
        if visited.contains((current, direction)) then (newVisited.map(_._1), true)
        else if map.get(next).contains('.') then travel(map, next, direction, newVisited)
        else if map.get(next).contains('#') then travel(map, current, direction.turn, newVisited)
        else (newVisited.map(_._1), false)
