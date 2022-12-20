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

  def solve(input: mutable.ArrayBuffer[Long]): mutable.ArrayBuffer[Long] =
    def loop(i: Int, a: mutable.ArrayBuffer[(Long, Boolean)]): mutable.ArrayBuffer[(Long, Boolean)] =
      if (i >= a.size)
        println(s"final sequence is ${a.toList.map(_._1)}")
        println(s"untouched elements ${a.filter(!_._2).toList}")
        a
      else if (a(i)._2) loop(i + 1, a)
      // else if (a(i)._1 != 11 && a(i)._1 != 12) loop(i + 1, a) // todo
      // else if (a(i)._1 > 65) loop(i + 1, a) // todo
      else if (a(i)._1 == 0) loop(i + 1, a)
      else {
        println(s"current sequence is ${a.map(_._1).toList}")
        val (dI, _) = a(i)

        if (dI > 0) {
          val newIndex = i + dI

          val adjustedNewIndex = if (newIndex >= a.size) {
            // val numberOfWrapArounds = newIndex / a.size
            val numberOfWrapArounds = {

              val magicNumber = i + (a.size - i - 1)

              println(s"i + (a.size - i - 1) = ${i + (a.size - i - 1)}")
              println(s" ((newIndex - 1) / magicNumber) = ${((newIndex - 1) / magicNumber)}")
              ((newIndex - 1) / magicNumber)
            }
            println(s"WRAP AROUND ${numberOfWrapArounds}")

            (newIndex + numberOfWrapArounds) % a.size
          } else {
            newIndex
          }

          // println(s"moving $dI from $i to $adjustedNewIndex")

          if (adjustedNewIndex == i) {
            // println("FISHY STUFF POS NUMBER")
          }

          a.remove(i)
          a.insert(adjustedNewIndex.toInt, (dI, true))
        } else {
          val newIndex = i + dI
          val adjustedNewIndex = if (newIndex < 0) {
            // val numberOfWrapArounds = (newIndex / a.size) - 1

            val numberOfWrapArounds = {
              val magicNumber = i + (a.size - i - 1)
              println(s"MAGIC NUMBER IS $magicNumber")

              println(s"i + (a.size - i - 1) = ${i + (a.size - i - 1)}")
              println(s" ((newIndex - 1) / magicNumber) = ${((newIndex - 1) / magicNumber)}")

              (math.abs(newIndex) + a.size - 2) / magicNumber
            }

            println(s"WRAP AROUND ${numberOfWrapArounds}")

            math.floorMod(newIndex - numberOfWrapArounds, a.size)
          } else {
            newIndex
          }

          // println(s"moving $dI from $i to $adjustedNewIndex")

          if (adjustedNewIndex == i) {
            // println("FISHY STUFF NEG NUMBER")
          }

          a.remove(i)
          a.insert(adjustedNewIndex.toInt, (dI, true))

        }

        println
        loop(i, a)
      }

    loop(0, input.map(i => (i, false))).map(_._1)

  def part1(lines: List[String]): Long =
    val result = solve(parse(lines))
    // println(result.toList)
    val zeroI = result.indexWhere(_ == 0)
    List(1000, 2000, 3000).map { i =>
      val a = result((zeroI + i) % result.size)
      println(a)
      a

    }.sum

  def part2(lines: List[String]): Long =
    0
    /*val result = solve(parse(lines).map(_ * 811589153L))
    // println(result.toList)
    val zeroI = result.indexWhere(_ == 0)
    List(1000, 2000, 3000).map { i =>
      val a = result((zeroI + i) % result.size)
      println(a)
      a

    }.sum*/
