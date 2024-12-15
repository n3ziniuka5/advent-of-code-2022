package aoc

import aoc.Common.timed

object Day15:
    def main(args: Array[String]): Unit =
        val lines = InputUtils.fetchInput(2024, 15)
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    def part1(lines: List[String]): Long =
        val (map, start, instructions) = parse(lines, expand = false)
        val newMap                     = performMoves(map, start, instructions)
        calculateScore(newMap)

    def part2(lines: List[String]): Long =
        val (map, start, instructions) = parse(lines, expand = true)
        val newMap                     = performMoves(map, start, instructions)
        calculateScore(newMap)

    def parse(lines: List[String], expand: Boolean): (map: Map2d[Char], start: Point, instructions: List[Char]) =
        val mapLines = lines.takeWhile(_.nonEmpty)
        val mapLinesExpanded =
            if expand then
                mapLines.map: line =>
                    line.map:
                        case '#' => "##"
                        case '.' => ".."
                        case '@' => "@."
                        case 'O' => "[]"
                    .mkString
            else mapLines

        val map          = Map2d.fromLines(mapLinesExpanded)
        val start        = map.underlying.find(_._2 == '@').get._1
        val instructions = lines.drop(mapLinesExpanded.length + 1).mkString.toList
        (map, start, instructions)

    def calculateScore(map: Map2d[Char]): Long =
        map.underlying.toList
            .filter((_, char) => char == 'O' || char == '[')
            .map: (point, _) =>
                point.y * 100 + point.x
            .sum

    def performMoves(map: Map2d[Char], robotPosition: Point, instructions: List[Char]): Map2d[Char] =
        instructions match
            case head :: tail =>
                val direction = head match
                    case '^' => Direction.Up
                    case 'v' => Direction.Down
                    case '<' => Direction.Left
                    case '>' => Direction.Right
                val (newMap, newRobotPosition) = moveRobot(map, robotPosition, direction)
                performMoves(newMap, newRobotPosition, tail)
            case Nil => map

    def moveRobot(
        map: Map2d[Char],
        from: Point,
        direction: Direction
    ): (newMap: Map2d[Char], newRobotPosition: Point) =
        def loop(
            from: Point,
            moveList: List[(Point, Point)],
            addBlankSpacesTo: Set[Point]
        ): (moves: List[(Point, Point)], addBlankSpacesTo: Set[Point]) =
            val moveTo = from.move(direction)
            map(moveTo) match
                case '.' =>
                    ((from, moveTo) +: moveList, addBlankSpacesTo)
                case 'O' =>
                    loop(moveTo, (from, moveTo) +: moveList, addBlankSpacesTo)
                case ']' | '[' if direction == Direction.Up || direction == Direction.Down =>
                    val otherBoxHalfAt = if map(moveTo) == '[' then moveTo.right else moveTo.left
                    val (otherBoxHalfMoves, additionalBlankSpaces) = loop(otherBoxHalfAt, Nil, Set.empty)

                    if otherBoxHalfMoves.isEmpty then (Nil, Set.empty)
                    else
                        loop(
                          moveTo,
                          ((from, moveTo) +: otherBoxHalfMoves) ++ moveList,
                          addBlankSpacesTo + otherBoxHalfAt ++ additionalBlankSpaces
                        )

                case ']' | '[' => loop(moveTo, (from, moveTo) +: moveList, addBlankSpacesTo)
                case '#'       => (Nil, Set.empty)

        val (moveList, addBlankSpacesTo) = loop(from, Nil, Set.empty)
        if moveList.isEmpty then (map, from)
        else
            val newMap = moveList
                .foldLeft(map): (accumulatedMap, currentMove) =>
                    accumulatedMap.updated(currentMove._2, map(currentMove._1))

            val mapWithoutDanglingHalfBoxes =
                val blankSpacesWithRobot = addBlankSpacesTo + from
                val allPointsMovedTo     = moveList.map(_._2)
                (blankSpacesWithRobot -- allPointsMovedTo).foldLeft(newMap): (accumulatedMap, point) =>
                    accumulatedMap.updated(point, '.')

            (mapWithoutDanglingHalfBoxes, from.move(direction))
