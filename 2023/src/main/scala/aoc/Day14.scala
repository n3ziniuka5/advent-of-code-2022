package aoc

import aoc.Common.timed

import scala.annotation.tailrec
import scala.io.Source

object Day14:
    def main(args: Array[String]): Unit =
        val lines = Source.fromResource("day14.txt").getLines().toList
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    def part1(lines: List[String]): Long =
        val map    = Map2DVec.fromLines(lines)
        val newMap = tiltUp(map)
        northLoad(newMap)

    def part2(lines: List[String]): Long =
        val map    = Map2DVec.fromLines(lines)
        val newMap = cycleFor(map, 1000000000, Map.empty)
        northLoad(newMap)

    def tiltUp(map: Map2DVec[Char]): Map2DVec[Char] =
        map.mapCols(slideRow)

    def tiltDown(map: Map2DVec[Char]): Map2DVec[Char] =
        map.mapCols(c => slideRow(c.reverse).reverse)

    def tiltLeft(map: Map2DVec[Char]): Map2DVec[Char] =
        map.mapRows(slideRow)

    def tiltRight(map: Map2DVec[Char]): Map2DVec[Char] =
        map.mapRows(r => slideRow(r.reverse).reverse)

    def northLoad(map: Map2DVec[Char]): Long =
        (for {
            x <- 0 to map.maxX
            y <- 0 to map.maxY
        } yield {
            if (map(x, y) == 'O') (map.maxY + 1) - y
            else 0
        }).sum

    def slideRow(row: Vector[Char]): Vector[Char] =
        val array = collection.mutable.ListBuffer.empty[Char]
        array.sizeHint(row.length)

        row.zipWithIndex.foreach { case (char, i) =>
            if char == 'O' then array.append(char)
            else if char == '#' then
                (array.size until i).foreach(_ => array.append('.'))
                array.append('#')
            else ()
        }

        (array.size until row.length).foreach(_ => array.append('.'))

        array.toVector

    @tailrec
    def cycleFor(map: Map2DVec[Char], target: Long, seenAt: Map[Map2DVec[Char], Long]): Map2DVec[Char] =
        if target == 0 then map
        else if seenAt.contains(map) then
            val lastSeenAt = seenAt(map)
            val cycleSize  = lastSeenAt - target

            target % cycleSize match
                case 0 => map
                case remainder =>
                    cycleFor(map, remainder, Map.empty)
        else
            val newMap = cycle(map)
            cycleFor(newMap, target - 1, seenAt + (map -> target))

    def cycle(map: Map2DVec[Char]): Map2DVec[Char] =
        val afterUp   = tiltUp(map)
        val afterLeft = tiltLeft(afterUp)
        val afterDown = tiltDown(afterLeft)
        tiltRight(afterDown)
