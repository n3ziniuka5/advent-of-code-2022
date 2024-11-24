package aoc

import aoc.Common.timed

import scala.io.Source
import Day7.HandType.*

object Day7:
    def main(args: Array[String]): Unit =
        val lines = Source.fromResource("day7.txt").getLines().toList
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    enum HandType:
        case FiveKind, FourKind, FullHouse, ThreeKind, TwoPair, OnePair, HighCard

    val cardOrder = List('2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A').zipWithIndex.toMap
    def cardOrdering(map: Map[Char, Int]) = new Ordering[Char]:
        override def compare(x: Char, y: Char): Int =
            map(x) - map(y)

    val typeOrder =
        List(HighCard, OnePair, TwoPair, ThreeKind, FullHouse, FourKind, FiveKind).zipWithIndex.toMap
    val typeOrdering = new Ordering[HandType]:
        override def compare(x: HandType, y: HandType): Int =
            typeOrder(x) - typeOrder(y)

    def part1(lines: List[String]): Long =
        solve(lines, handToType, cardOrdering(cardOrder))

    def part2(lines: List[String]): Long =
        solve(lines, handToTypeReplaceJokers, cardOrdering(cardOrder.updated('J', -1)))

    def solve(lines: List[String], handToType: List[Char] => HandType, cardOrdering: Ordering[Char]): Long =
        lines
            .map { case s"$cards $bid" =>
                (cards.toCharArray.toList, bid.trim.toInt)
            }
            .sortBy(_._1)(handOrder(handToType, cardOrdering))
            .zipWithIndex
            .map { case ((hand, bid), rank) =>
                bid * (rank + 1)
            }
            .sum

    def handToType(hand: List[Char]): HandType =
        val grouped = hand.groupBy(identity).view.mapValues(_.size).toVector
        if (grouped.size == 1) {
            FiveKind
        } else if (grouped.size == 2) {
            if (grouped.exists(_._2 == 4)) {
                FourKind
            } else {
                FullHouse
            }
        } else if (grouped.size == 3) {
            if (grouped.exists(_._2 == 3)) {
                ThreeKind
            } else {
                TwoPair
            }
        } else if (grouped.size == 4) {
            OnePair
        } else {
            HighCard
        }

    def handToTypeReplaceJokers(hand: List[Char]): HandType =
        val grouped = hand.groupBy(identity).view.mapValues(_.size)
        if (grouped.contains('J')) {
            grouped.toList.filter(_._1 != 'J').maxByOption(_._2) match
                case Some((replacement, _)) => handToType(hand.map(c => if (c == 'J') replacement else c))
                case None                   => handToType(hand)
        } else {
            handToType(hand)
        }

    def handOrder(handToType: List[Char] => HandType, cardOrdering: Ordering[Char]) = new Ordering[List[Char]]:
        override def compare(x: List[Char], y: List[Char]): Int =
            val xType = handToType(x)
            val yType = handToType(y)
            if (xType == yType) {
                x.zip(y).map((x, y) => cardOrdering.compare(x, y)).find(_ != 0).getOrElse(0)
            } else {
                typeOrdering.compare(xType, yType)
            }
