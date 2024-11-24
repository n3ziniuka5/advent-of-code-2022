package aoc

import aoc.Common.timed

import scala.annotation.tailrec
import scala.collection.immutable.{MultiDict, MultiSet}
import scala.io.Source

object Day23:
    def main(args: Array[String]): Unit =
        val lines = Source.fromResource("day23.txt").getLines().toList
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    case class SearchState(current: Point, visited: Set[Point], steps: Int)

    @tailrec
    def longestPath(searches: List[SearchState], slopes: Boolean, map: Map2DVec[Char], result: Int): Int =
        searches match {
            case Nil => result
            case head :: tail =>
                if (head.current == Point(map.maxX - 1, map.maxY))
                    val newHighest = if (head.steps > result)
                        println(s"found longer path ${head.steps}")
                        head.steps
                    else result
                    longestPath(tail, slopes, map, newHighest)
                else if (head.visited.contains(head.current)) longestPath(tail, slopes, map, result)
                else
                    val next =
                        if (slopes)
                            map(head.current) match
                                case '.' => head.current.adjacent.filter(_.inBounds(map)).filter(p => map(p) != '#')
                                case '<' => List(head.current.left)
                                case '>' => List(head.current.right)
                                case '^' => List(head.current.up)
                                case 'v' => List(head.current.down)
                        else
                            head.current.adjacent.filter(_.inBounds(map)).filter(p => map(p) != '#')

                    val nextSearches = next.map { p =>
                        SearchState(p, head.visited + head.current, head.steps + 1)
                    }
                    longestPath(nextSearches ++ tail, slopes, map, result)
        }

    def part1(lines: List[String]): Int =
        val map = Map2DVec.fromLines(lines)
        longestPath(List(SearchState(Point(1, 0), Set.empty, 0)), true, map, 0)

    def part2(lines: List[String]): Int =
        val map = Map2DVec.fromLines(lines)
        longestPath(List(SearchState(Point(1, 0), Set.empty, 0)), false, map, 0)
