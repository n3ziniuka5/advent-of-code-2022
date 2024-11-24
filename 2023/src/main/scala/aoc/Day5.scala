package aoc

import aoc.Common.timed

import scala.annotation.tailrec
import scala.collection.immutable.MultiDict
import scala.io.Source

object Day5:
    case class Range(source: Long, destination: Long, size: Long)
    case class Input(seeds: List[Long], maps: MultiDict[String, Range])

    def main(args: Array[String]): Unit =
        val lines = Source.fromResource("day5.txt").getLines().toList
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    def part1(lines: List[String]): Long =
        val data = parse(lines)
        solve(data.seeds.map((_, 0)), data.maps, data.seeds.head, Long.MaxValue)

    def part2(lines: List[String]): Long =
        val data = parse(lines)
        solve(data.seeds.sliding(2, 2).map(l => (l.head, l(1))).toList, data.maps, data.seeds.head, Long.MaxValue)

    def parse(lines: List[String]): Input =
        val seeds = lines.head.drop("seeds: ".length).split(" ").map(_.trim.toLong).toList

        @tailrec
        def loop(lines: List[String], currentMap: String, maps: MultiDict[String, Range]): MultiDict[String, Range] =
            if lines.isEmpty then maps
            else if lines.head.isEmpty then loop(lines.tail, currentMap, maps)
            else
                lines.head match
                    case s"$map map:" => loop(lines.tail, map, maps)
                    case l =>
                        val Array(dest, src, range) = l.split(" ").map(_.trim.toLong)
                        val r                       = Range(src, dest, range)
                        loop(lines.tail, currentMap, maps.add(currentMap, r))

        val maps = loop(lines.drop(2), "", MultiDict.empty)
        Input(seeds, maps)

    @tailrec
    def solve(seeds: List[(Long, Long)], maps: MultiDict[String, Range], currentSeed: Long, min: Long): Long =
        val (currentRange, currentRangeSize) = seeds.head
        if currentSeed > currentRange + currentRangeSize then
            if (seeds.tail.isEmpty) min
            else solve(seeds.tail, maps, seeds.tail.head._1, min)
        else
            val (location, canSkip) = locationForSeed(currentSeed, maps)

            solve(seeds, maps, currentSeed + math.max(1, canSkip), math.min(min, location))

    // returns location and a number of seeds you can safely skip without encountering a lower location
    def locationForSeed(seed: Long, maps: MultiDict[String, Range]): (Long, Long) =
        def getRange(mapName: String, numberAndSkippedLastTime: (Long, Long)): (Long, Long) =
            val (number, skippedLastTime) = numberAndSkippedLastTime
            maps
                .get(mapName)
                .find { range =>
                    (range.source <= number) && ((range.source + range.size) >= number)
                }
                .map { range =>
                    val destination = range.destination + (number - range.source)
                    val canSkip     = range.source + range.size - number
                    (destination, math.min(canSkip, skippedLastTime))
                }
                .getOrElse {
                    val canSkip = maps
                        .get(mapName)
                        .map { range =>
                            range.source - number - 1
                        }
                        .filter(_ >= 0)
                        .minOption
                        .getOrElse(Long.MaxValue)
                    (number, math.min(canSkip, skippedLastTime))
                }

        getRange(
          "humidity-to-location",
          getRange(
            "temperature-to-humidity",
            getRange(
              "light-to-temperature",
              getRange(
                "water-to-light",
                getRange(
                  "fertilizer-to-water",
                  getRange("soil-to-fertilizer", getRange("seed-to-soil", (seed, Long.MaxValue)))
                )
              )
            )
          )
        )
