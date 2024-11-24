package aoc

import aoc.Common.timed
import zio.prelude.ZSet

import scala.io.Source

object Day12:
    def main(args: Array[String]): Unit =
        val lines = Source.fromResource("day12.txt").getLines().toList
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    def part1(lines: List[String]): Long =
        parse(lines)
            .map(waysToSolve)
            .sum

    def part2(lines: List[String]): Long =
        parse(lines)
            .map { case (line, segments) =>
                val newLine     = (1 to 5).map(_ => line).mkString("?")
                val newSegments = (1 to 5).toList.flatMap(_ => segments)
                (newLine, newSegments)
            }
            .map(waysToSolve)
            .sum

    def parse(lines: List[String]): List[(String, List[Int])] =
        lines.map { line =>
            val Array(lineStr, segmentsStr) = line.split(" ")
            val segments                    = segmentsStr.split(",").map(_.toInt).toList
            (lineStr, segments)
        }

    // returns a list of possible string sizes
    def dfs(lines: List[(String, Int, Int)], target: Int, result: ZSet[Int, Int]): ZSet[Int, Int] =
        if (lines.isEmpty) result
        else
            val (line, discardedLength, currentBroken) = lines.head
            val mustBeBroken                           = line.takeWhile(_ == '#')
            if mustBeBroken.nonEmpty then
                val newBroken = currentBroken + mustBeBroken.length
                if newBroken > target then dfs(lines.tail, target, result)
                else if newBroken == target then
                    dfs(lines.tail, target, result.combine(ZSet(discardedLength + mustBeBroken.length + 1)))
                else
                    val newLine = (line.drop(mustBeBroken.length), discardedLength + mustBeBroken.length, newBroken)
                    dfs(
                      newLine +: lines.tail,
                      target,
                      result
                    )
            else if currentBroken == target then dfs(lines.tail, target, result.combine(ZSet(discardedLength + 1)))
            else if line.isEmpty then dfs(lines.tail, target, result)
            else if line.head == '.' && currentBroken > 0 then dfs(lines.tail, target, result)
            else if line.head == '.' then
                val dotsDropped = line.dropWhile(_ == '.')
                val newLine     = (dotsDropped, discardedLength + (line.length - dotsDropped.length), currentBroken)
                dfs(newLine +: lines.tail, target, result)
            else if currentBroken == 0 then // line.head will always be ?
                val newLineEmpty = (line.tail, discardedLength + 1, 0)
                val newLineFull  = (line.tail, discardedLength + 1, 1)
                dfs(List(newLineEmpty, newLineFull) ++ lines.tail, target, result)
            else
                val newLine = (line.tail, discardedLength + 1, currentBroken + 1)
                dfs(newLine +: lines.tail, target, result)

    val cache = collection.mutable.Map.empty[(String, List[Int]), Long]
    def waysToSolve(line: String, segments: List[Int]): Long =
        if line.isEmpty && segments.isEmpty then 1
        else if line.isEmpty then 0
        else if segments.isEmpty && line.contains('#') then 0
        else if segments.isEmpty then 1
        else if line.head == '.' then waysToSolve(line.tail, segments)
        else
            cache.get((line, segments)) match
                case Some(value) => value
                case None =>
                    val untilBroken = line.takeWhile(_ != '#')
                    val withBroken  = line.drop(untilBroken.length).takeWhile(_ != '.')

                    val maximumSegmentSize = untilBroken ++ withBroken

                    val singleSegmentResult = dfs(
                      List((maximumSegmentSize, 0, 0)),
                      segments.head,
                      ZSet.empty
                    ).toMap

                    val allSegmentResult = singleSegmentResult.map { case (lineSize, count) =>
                        val remainingLineWays = waysToSolve(line.drop(lineSize), segments.tail)
                        remainingLineWays * count
                    }.sum

                    cache.update((line, segments), allSegmentResult)
                    allSegmentResult
