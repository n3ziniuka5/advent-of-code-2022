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

  def calculateWrapArounds(currentIndex: Int, newIndex: Int, arraySize: Int): Int =
    if (newIndex >= arraySize) {
      val temp = arraySize - currentIndex
      ((newIndex - temp + 1) / arraySize)
    } else {
      0
    }

  def mixNumbers(input: mutable.ArrayBuffer[Long], timesToMix: Int): mutable.ArrayBuffer[Long] =
    def loop(i: Int, a: mutable.ArrayBuffer[(Long, Int)], indexMap: Map[Int, Int]): mutable.ArrayBuffer[(Long, Int)] =
      if (i >= a.size)
        // println(s"final sequence is ${a.toList.map(_._1)}")
        // println(s"untouched elements ${a.filter(!_._2).toList}")
        a
      // else if (a(i)._2) loop(i + 1, a)
      // else if (a(i)._1 != 11 && a(i)._1 != 12) loop(i + 1, a) // todo
      // else if (a(i)._1 > 65) loop(i + 1, a) // todo
      // else if (a(i)._1 == 0) loop(i + 1, a)
      else {
        // println(s"current sequence is ${a.map(_._1).toList}")

        val actualI = a.indexWhere(_._2 == i)

        val org @ (dI, _) = a(actualI)

        if (dI > 0) {
          val newIndex = actualI + dI
          val adjustedNewIndex = if (newIndex >= a.size) {
            // val numberOfWrapArounds = newIndex / a.size
            val numberOfWrapArounds = {

              val magicNumber = actualI + (a.size - actualI - 1)

              /*println(s"i + (a.size - i - 1) = ${i + (a.size - i - 1)}")
              println(s" ((newIndex - 1) / magicNumber) = ${((newIndex - 1) / magicNumber)}")*/
              ((newIndex - 1) / magicNumber)
            }
            // println(s"WRAP AROUND ${numberOfWrapArounds}")

            (newIndex + numberOfWrapArounds) % a.size
          } else {
            newIndex
          }

          // println(s"moving $dI from $i to $adjustedNewIndex")

          /*if (adjustedNewIndex == i) {
            // println("FISHY STUFF POS NUMBER")
          }*/

          a.remove(actualI)
          a.insert(adjustedNewIndex.toInt, org)
        } else {
          val newIndex = actualI + dI
          val adjustedNewIndex = if (newIndex < 0) {
            // val numberOfWrapArounds = (newIndex / a.size) - 1

            val numberOfWrapArounds = {
              val magicNumber = actualI + (a.size - actualI - 1)
              /*println(s"MAGIC NUMBER IS $magicNumber")

              println(s"i + (a.size - i - 1) = ${i + (a.size - i - 1)}")
              println(s" ((newIndex - 1) / magicNumber) = ${((newIndex - 1) / magicNumber)}")*/

              (math.abs(newIndex) + a.size - 2) / magicNumber
            }

            // println(s"WRAP AROUND ${numberOfWrapArounds}")

            math.floorMod(newIndex - numberOfWrapArounds, a.size)
          } else {
            newIndex
          }

          // println(s"moving $dI from $i to $adjustedNewIndex")

          a.remove(actualI)
          a.insert(adjustedNewIndex.toInt, org)

        }

        // println
        loop(i + 1, a, indexMap)
      }

    val initialItemsWithIndex = input.zipWithIndex

    (1 to timesToMix)
      .foldLeft(initialItemsWithIndex) { (items, _) =>

        val map = items.indices.map { actualIndex =>
          items(actualIndex)._2 -> actualIndex
        }.toMap

        loop(0, items, map)
      }
      .map(_._1)

  def part1(lines: List[String]): Long =
    val result = mixNumbers(parse(lines), 1)
    // println(result.toList)
    val zeroI = result.indexWhere(_ == 0)
    List(1000, 2000, 3000).map { i =>
      val a = result((zeroI + i) % result.size)
      println(a)
      a

    }.sum

  def part2(lines: List[String]): Long =
    val result = mixNumbers(parse(lines).map(_ * 811589153L), 10)
    // println(result.toList)
    val zeroI = result.indexWhere(_ == 0)
    List(1000, 2000, 3000).map { i =>
      val a = result((zeroI + i) % result.size)
      println(a)
      a

    }.sum
