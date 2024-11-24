package aoc

import aoc.Common.timed

import scala.annotation.tailrec
import scala.io.Source

object Day15:
    def main(args: Array[String]): Unit =
        val lines = Source.fromResource("day15.txt").getLines().toList
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    def part1(lines: List[String]): Long =
        lines.head.split(",").map(computeHash).sum

    def part2(lines: List[String]): Long =
        val boxes = lines.head.split(",").foldLeft(State(Map.empty)) { (state, str) =>
            val boxNumber = computeHash(str.takeWhile(_.isLetter))

            str.dropWhile(_.isLetter).head match
                case '-' => state.removeFromBox(boxNumber, str)
                case '=' => state.addToBox(boxNumber, str)
        }
        boxes.focusingPower

    def computeHash(str: String): Int =
        @tailrec
        def loop(str: String, hash: Int): Int =
            if str.isEmpty then hash
            else
                val newHash = (hash + str.head.toInt) * 17 % 256
                loop(str.tail, newHash)
        loop(str, 0)

    case class State(boxes: Map[Int, List[(String, Int)]]):
        def focusingPower: Long =
            boxes.map { case (boxNumber, lenses) =>
                lenses.reverse.zipWithIndex.map { case ((_, focalLength), idx) =>
                    (boxNumber + 1) * focalLength * (idx + 1)
                }.sum
            }.sum

        def addToBox(boxNumber: Int, str: String): State =
            val box         = boxes.getOrElse(boxNumber, Nil)
            val letters     = str.takeWhile(_.isLetter)
            val focalLength = str.last.toString.toInt

            val newBox =
                if box.exists(_._1 == letters) then
                    box.map { case (str, len) =>
                        if str == letters then (str, focalLength)
                        else (str, len)
                    }
                else (letters, focalLength) +: box

            copy(boxes = boxes.updated(boxNumber, newBox))

        def removeFromBox(boxNumber: Int, str: String): State =
            val box     = boxes.getOrElse(boxNumber, Nil)
            val letters = str.takeWhile(_.isLetter)
            val newBox  = box.filter(_._1 != letters)
            copy(boxes = boxes.updated(boxNumber, newBox))
