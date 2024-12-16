package aoc

import aoc.Common.timed
import scala.collection.mutable.PriorityQueue
import scala.language.experimental.namedTuples
import scala.annotation.tailrec

object Day16:
    def main(args: Array[String]): Unit =
        val lines = InputUtils.fetchInput(2024, 16)
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    def part1(lines: List[String]): Long =
        val (map, start) = parse(lines)
        shortestPath(map, start).lowestScore

    def part2(lines: List[String]): Long =
        val (map, start) = parse(lines)
        shortestPath(map, start).numTiles

    def parse(lines: List[String]): (Map2d[Char], Point) =
        val map   = Map2d.fromLines(lines)
        val start = map.underlying.find(_._2 == 'S').get._1
        (map, start)

    def shortestPath(map: Map2d[Char], from: Point): (lowestScore: Long, numTiles: Long) =
        type SearchData = (current: Point, direction: Direction, score: Long, tilesInThisPath: Set[Point])

        @tailrec
        def loop(
            search: PriorityQueue[SearchData],
            visited: Map[(Point, Direction), Long],
            maybeLowestScore: Option[Long],
            tilesInShortestPath: Set[Point]
        ): (lowestScore: Long, numTiles: Long) =
            val (current, direction, score, tilesInThisPath) = search.dequeue()
            if score > maybeLowestScore.getOrElse(Long.MaxValue) then
                (maybeLowestScore.get, tilesInShortestPath.size + 1)
            else if map.get(current).contains('E') then
                loop(search, visited, Some(score), tilesInShortestPath ++ tilesInThisPath)
            else if score > visited.getOrElse((current, direction), Long.MaxValue) then
                loop(search, visited, maybeLowestScore, tilesInShortestPath)
            else
                val moveAlongThePath = (current.move(direction), direction, score + 1, tilesInThisPath + current)
                val sideDirections = direction match
                    case Direction.Up | Direction.Down    => List(Direction.Left, Direction.Right)
                    case Direction.Left | Direction.Right => List(Direction.Up, Direction.Down)

                val sideMoves =
                    sideDirections.map(d => (current.move(d), d, score + 1001, tilesInThisPath + current): SearchData)
                val searchesToAdd = (moveAlongThePath +: sideMoves).filter: s =>
                    val mapTile = map.get(s.current)
                    mapTile.contains('.') || mapTile.contains('E')

                search.enqueue(searchesToAdd*)
                loop(search, visited + ((current, direction) -> score), maybeLowestScore, tilesInShortestPath)

        val search =
            PriorityQueue.apply((from, Direction.Right, 0L, Set.empty[Point]): SearchData)(Ordering.by(-_.score))
        loop(search, Map.empty, None, Set.empty)
