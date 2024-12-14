package aoc

import aoc.Common.timed

object Day4:
    def main(args: Array[String]): Unit =
        val lines = InputUtils.fetchInput(2024, 4)
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    def part1(lines: List[String]): Long =
        val map = Map2d.fromLines(lines)
        countXmas(map)

    def part2(lines: List[String]): Long =
        val map = Map2d.fromLines(lines)
        countXmasPart2(map)

    def countXmas(map: Map2d[Char]): Int =
        val a = for
            x <- 0L to map.maxX
            y <- 0L to map.maxY
        yield
            val point = Point(x, y)
            if map.get(point).contains('X') then
                val up          = (1 to 3).map(i => Point(x, y - i)).toList
                val down        = (1 to 3).map(i => Point(x, y + i)).toList
                val left        = (1 to 3).map(i => Point(x - i, y)).toList
                val right       = (1 to 3).map(i => Point(x + i, y)).toList
                val topLeft     = (1 to 3).map(i => Point(x - i, y - i)).toList
                val topRight    = (1 to 3).map(i => Point(x + i, y - i)).toList
                val bottomLeft  = (1 to 3).map(i => Point(x - i, y + i)).toList
                val bottomRight = (1 to 3).map(i => Point(x + i, y + i)).toList

                val searches = List(up, down, left, right, topLeft, topRight, bottomLeft, bottomRight)

                searches.filter: search =>
                    search.map(map.get(_).getOrElse('.')).mkString == "MAS"
            else Nil
        a.flatten.size

    def countXmasPart2(map: Map2d[Char]): Int =
        val a = for
            x <- 0L to map.maxX
            y <- 0L to map.maxY
        yield
            val point = Point(x, y)
            if map.get(point).contains('A') then
                val topLeftToBottomRight = List(-1, 0, 1).map(i => Point(x + i, y + i)).toList
                val topRightToBottomLeft = List(-1, 0, 1).map(i => Point(x - i, y + i)).toList
                val bottomLeftToTopRight = List(-1, 0, 1).map(i => Point(x + i, y - i)).toList
                val bottomRightToTopLeft = List(-1, 0, 1).map(i => Point(x - i, y - i)).toList

                val topLeftToBottomRightGood = topLeftToBottomRight.map(map.get(_).getOrElse('.')).mkString == "MAS"
                val topRightToBottomLeftGood = topRightToBottomLeft.map(map.get(_).getOrElse('.')).mkString == "MAS"
                val bottomLeftToTopRightGood = bottomLeftToTopRight.map(map.get(_).getOrElse('.')).mkString == "MAS"
                val bottomRightToTopLeftGood = bottomRightToTopLeft.map(map.get(_).getOrElse('.')).mkString == "MAS"

                if (topLeftToBottomRightGood || bottomRightToTopLeftGood) && (topRightToBottomLeftGood || bottomLeftToTopRightGood)
                then Some(point)
                else None
            else None
        a.flatten.size
