package aoc

import aoc.Common.timed
import aoc.Day13.Value.ManyItems
import aoc.Day13.Value.Single

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object Day13:
  def main(args: Array[String]): Unit =
    val lines = Source.fromResource("day13.txt").getLines().toList
    timed("Part 1", part1(lines))
    timed("Part 2", part2(lines))

  enum Value:
    case ManyItems(a: ArrayBuffer[Value])
    case Single(i: Int)

  def isDivider(items: ManyItems): Boolean =
    items.a.size == 1 &&
      (items.a.head match
        case ManyItems(b) =>
          b.size == 1 &&
          (b.head == Single(2) || b.head == Single(6))
        case _: Single => false
      )

  @tailrec
  def parseLine(lines: List[Char], stack: List[ManyItems]): ManyItems =
    lines.head match
      case ',' => parseLine(lines.tail, stack)
      case '[' =>
        val newBuffer: ManyItems = ManyItems(ArrayBuffer[Value]())
        stack.headOption.foreach(_.a.append(newBuffer))
        parseLine(lines.tail, newBuffer +: stack)
      case ']' => if (stack.tail.nonEmpty) parseLine(lines.tail, stack.tail) else stack.head
      case _ =>
        val numString = lines.takeWhile(_.isDigit).mkString
        // println(numString)
        stack.head.a.append(Single(numString.toInt))
        // println(stack.head.a.toList)
        parseLine(lines.drop(numString.length), stack)

  def compare(left: ManyItems, right: ManyItems, i: Int): Option[Boolean] =
    // val compResult =
    if (left.a.size <= i && right.a.size <= i) None
    else if (left.a.size <= i) Some(true)
    else if (right.a.size <= i) Some(false)
    else
      (left.a(i), right.a(i)) match
        case (Single(l), Single(r)) =>
          if (l < r) Some(true) else if (r < l) Some(false) else compare(left, right, i + 1)
        case (l: ManyItems, r: Single) => compare(l, ManyItems(ArrayBuffer(r)), 0).orElse(compare(left, right, i + 1))
        case (l: Single, r: ManyItems) => compare(ManyItems(ArrayBuffer(l)), r, 0).orElse(compare(left, right, i + 1))
        case (l: ManyItems, r: ManyItems) => compare(l, r, 0).orElse(compare(left, right, i + 1))
  // compResult.orElse(compare(left, right, i + 1))

  def part1(lines: List[String]): Int =
    val pairs = lines
      .sliding(2, 3)
      // .take(1)
      .map(_.map { l =>
        /*println(s"parsing $l");*/
        parseLine(l.toCharArray.toList, Nil)
      })
      .toVector
    pairs.zipWithIndex
      .map { (p, i) =>
        // println(p)
        (compare(p(0), p(1), 0), i + 1)
      }
      // .tapEach(println)
      .filter(_._1.contains(true))
      // .map(println)
      .map(_._2)
      .sum

  def part2(lines: List[String]): Int =
    implicit val ordering: Ordering[ManyItems] = new Ordering[ManyItems]:
      override def compare(x: ManyItems, y: ManyItems): Int = Day13.compare(x, y, 0) match
        case Some(false) => 1
        case Some(true)  => -1
        case None        => 0

    ("[[2]]" +: "[[6]]" +: lines)
      .filter(_.nonEmpty)
      // .take(1)
      .map(l =>
        /*println(s"parsing $l");*/
        parseLine(l.toCharArray.toList, Nil)
      )
      .sorted
      .zipWithIndex
      .filter(a => isDivider(a._1))
      .map(_._2 + 1)
      .product
