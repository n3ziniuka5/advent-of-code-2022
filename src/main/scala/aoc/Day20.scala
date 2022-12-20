package aoc

import aoc.Common.timed

import scala.collection.mutable
import scala.io.Source

object Day20:
  def main(args: Array[String]): Unit =
    val lines = Source.fromResource("day20.txt").getLines().toList
    timed("Part 1", part1(lines))
    timed("Part 2", part2(lines))

  def parse(lines: List[String]): mutable.ArrayBuffer[Long] =
    mutable.ArrayBuffer.from(lines.map(_.toInt))

  def mixNumbers(input: mutable.ArrayBuffer[Long], timesToMix: Int): mutable.ArrayBuffer[Long] =
    def loop(
      i: Int,
      items: mutable.ArrayBuffer[(Long, Int)]
    ): mutable.ArrayBuffer[(Long, Int)] =
      if (i >= items.size) items
      else {
        val actualI = items.indexWhere(_._2 == i)

        val org @ (dI, _) = items(actualI)

        val newIndex = actualI + dI
        val adjustedNewIndex = if (dI > 0) {
          val numberOfWrapArounds = (newIndex - 1) / (items.size - 1)
          (newIndex + numberOfWrapArounds) % items.size
        } else {
          if (newIndex < 0) {
            val numberOfWrapArounds = (math.abs(newIndex) + items.size - 2) / (items.size - 1)
            math.floorMod(newIndex - numberOfWrapArounds, items.size)
          } else {
            newIndex
          }
        }

        items.remove(actualI)
        items.insert(adjustedNewIndex.toInt, org)

        loop(i + 1, items)
      }

    val initialItemsWithIndex = input.zipWithIndex

    (1 to timesToMix)
      .foldLeft(initialItemsWithIndex) { (items, _) =>
        loop(0, items)
      }
      .map(_._1)

  def solve(items: mutable.ArrayBuffer[Long], timesToMix: Int): Long =
    val afterMixing = mixNumbers(items, timesToMix)
    val zeroI       = afterMixing.indexWhere(_ == 0)
    List(1000, 2000, 3000).map { i =>
      afterMixing((zeroI + i) % afterMixing.size)
    }.sum

  def part1(lines: List[String]): Long =
    solve(parse(lines), 1)

  def part2(lines: List[String]): Long =
    solve(parse(lines).map(_ * 811589153L), 10)
