package aoc

import aoc.Common.timed

import scala.annotation.tailrec
import scala.collection.immutable.{MultiDict, MultiSet}
import scala.collection.mutable
import scala.io.Source
import scala.collection.parallel.CollectionConverters.*

object Day25:
    def main(args: Array[String]): Unit =
        val lines = Source.fromResource("day25.txt").getLines().toList
        timed("Part 1", part1(lines))

    case class SortedPair(a: String, b: String)
    object SortedPair:
        def apply(a: String, b: String): SortedPair =
            if (a < b) new SortedPair(a, b) else new SortedPair(b, a)

    def part1(lines: List[String]): Int =
        val graph = parse(lines)
        val connections = graph.toList.flatMap { (a, v) =>
            v.map { b =>
                SortedPair(a, b)
            }
        }.distinct

        val connectionsToRemove = connections.par
            .map { pair =>
                val distinctPaths = numDistinctPaths(pair.a, pair.b, graph)
                (pair.a, pair.b, distinctPaths)
            }
            .filter(_._3 == 3)

        val newGraph = connectionsToRemove.foldLeft(graph) { case (g, (a, b, _)) =>
            removeConnection(g, a, b)
        }

        val (firstComponentCount, secondComponentCount) = countComponents(newGraph)

        firstComponentCount * secondComponentCount

    def parse(lines: List[String]): Map[String, List[String]] =
        val m = collection.mutable.Map.empty[String, List[String]]

        def addConnection(a: String, b: String): Unit =
            m.update(a, b +: m.getOrElse(a, Nil))
            m.update(b, a +: m.getOrElse(b, Nil))

        lines.foreach { case s"$src: $destinations" =>
            destinations.split(' ').foreach(addConnection(src, _))
        }

        m.toMap

    def numDistinctPaths(a: String, b: String, graph: Map[String, List[String]]): Int =
        @tailrec
        def loop(visited: Set[SortedPair], count: Int): Int =
            if (count == 4) count
            else
                shortestPath(a, b, graph, visited) match
                    case None    => count
                    case Some(v) => loop(visited ++ v, count + 1)

        loop(Set.empty, 0)

    def removeConnection(g: Map[String, List[String]], a: String, b: String): Map[String, List[String]] =
        g.updated(a, g(a).filter(_ != b))
            .updated(b, g(b).filter(_ != a))

    def countComponents(g: Map[String, List[String]]): (Int, Int) =
        val searchFrom = g.head._1

        @tailrec
        def dfs(searches: List[String], visited: Set[String]): Int =
            if searches.isEmpty then visited.size
            else if visited.contains(searches.head) then dfs(searches.tail, visited)
            else dfs(g(searches.head) ++ searches.tail, visited + searches.head)

        val firstComponentCount = dfs(List(searchFrom), Set.empty)
        (firstComponentCount, g.size - firstComponentCount)

    def shortestPath(
        a: String,
        b: String,
        graph: Map[String, List[String]],
        unusableEdges: Set[SortedPair]
    ): Option[Set[SortedPair]] =
        @tailrec
        def loop(
            queue: mutable.PriorityQueue[(String, String, Long, Set[SortedPair])],
            visited: Set[String]
        ): Option[Set[SortedPair]] =
            if queue.isEmpty then None
            else
                val (from, current, steps, edgesUsedSoFar) = queue.dequeue()
                if (visited.contains(current)) loop(queue, visited)
                else if current == b then Some(edgesUsedSoFar + SortedPair(from, current))
                else
                    val searchesToAdd = graph(current)
                        .filterNot(v => edgesUsedSoFar.contains(SortedPair(current, v)))
                        .map(next => (current, next, steps + 1, edgesUsedSoFar + SortedPair(current, next)))
                    queue.enqueue(searchesToAdd: _*)
                    loop(queue, visited + current)

        val pq = mutable.PriorityQueue(("", a, 0L, unusableEdges))(Ordering.by(_._3 * -1))
        loop(pq, Set.empty)
