package aoc

import aoc.Common.timed
import Day17.Rock.{L, Line, Minus, Plus, Square}
import aoc.Day17.Direction.{Down, Sideways}

import scala.annotation.tailrec
import scala.collection.immutable.SortedSet
import scala.io.Source

object Day17:
  def main(args: Array[String]): Unit =
    val lines = Source.fromResource("day17.txt").getLines().toList
    timed("Part 1", part1(lines.head))
    timed("Part 2", part2(lines.head))

  case class Point(x: Int, y: Long):
    def inBounds: Boolean = x >= 0 && x <= 6 && y > 0

  enum Direction:
    case Down, Sideways

    def opposite: Direction = this match
      case Down     => Sideways
      case Sideways => Down

  enum Rock:
    case Minus, Plus, L, Line, Square

    def startingPosition(y: Long): Set[Point] = this match
      case Minus  => (0 to 3).map(dX => Point(2 + dX, y)).toSet
      case Plus   => Set(Point(3, y), Point(3, y + 1), Point(3, y + 2), Point(2, y + 1), Point(4, y + 1))
      case L      => Set(Point(2, y), Point(3, y), Point(4, y), Point(4, y + 1), Point(4, y + 2))
      case Line   => (0 to 3).map(dY => Point(2, y + dY)).toSet
      case Square => Set(Point(2, y), Point(2, y + 1), Point(3, y), Point(3, y + 1))

  def rockStream: LazyList[Rock] = LazyList.from(List(Minus, Plus, L, Line, Square)) #::: rockStream

  def parse(jetPattern: String): LazyList[Char] =
    val charArray = jetPattern.toCharArray
    LazyList.from(charArray) #::: parse(jetPattern)

  @tailrec
  def dropRock(
    rock: Set[Point],
    direction: Direction,
    jets: LazyList[Char],
    blocks: Map[Int, SortedSet[Long]]
  ): (Map[Int, SortedSet[Long]], LazyList[Char]) =
    direction match
      case Sideways =>
        val dX = jets.head match
          case '>' => 1
          case '<' => -1

        val newRock = rock.map(r => r.copy(x = r.x + dX))

        if (newRock.forall(_.inBounds) && newRock.forall(r => !blocks(r.x).contains(r.y)))
          dropRock(newRock, direction.opposite, jets.tail, blocks)
        else dropRock(rock, direction.opposite, jets.tail, blocks)

      case Down =>
        val newRock = rock.map(r => r.copy(y = r.y - 1))
        if (newRock.forall(_.inBounds) && newRock.forall(r => !blocks(r.x).contains(r.y)))
          dropRock(newRock, direction.opposite, jets, blocks)
        else
          val newBlocks = rock.foldLeft(blocks) { (acc, p) =>
            acc + (p.x -> (acc(p.x) + p.y))
          }
          (newBlocks, jets)

  def maxHeight(blocks: Map[Int, SortedSet[Long]]): Long =
    blocks.values.map(_.head).maxOption.getOrElse(0)

  def keepDroppingRocks(
    dropCount: Long,
    targetDropCount: Long,
    rocks: LazyList[Rock],
    jets: LazyList[Char],
    jetCount: Int,
    blocks: Map[Int, SortedSet[Long]],
    observedPatterns: Map[String, (Long, Long)]
  ): Long =
    val currentHeight = maxHeight(blocks)
    if (dropCount == targetDropCount) currentHeight
    else
      val floorPattern = (1 to 6).foldLeft(s"${rocks.head} ${jets.take(jetCount).mkString} 0") { (acc, i) =>
        acc + s" ${blocks(i).headOption.getOrElse(0L) - blocks(0).headOption.getOrElse(0L)}"
      }
      observedPatterns.get(floorPattern) match
        case Some((lastDropCount, lastHeight)) =>
          val dropDiff   = dropCount - lastDropCount
          val heightDiff = currentHeight - lastHeight

          val dropsRemaining     = targetDropCount - dropCount
          val repeatedDropHeight = dropsRemaining / dropDiff * heightDiff
          val remainingHeight =
            keepDroppingRocks(0, dropsRemaining % dropDiff, rocks, jets, jetCount, blocks, Map.empty)

          val result = repeatedDropHeight + remainingHeight

          result
        case None =>
          val (newBlocks, newJets) = dropRock(rocks.head.startingPosition(currentHeight + 4), Sideways, jets, blocks)

          keepDroppingRocks(
            dropCount + 1,
            targetDropCount,
            rocks.tail,
            newJets,
            jetCount,
            newBlocks,
            observedPatterns + (floorPattern -> ((dropCount, currentHeight)))
          )

  def solve(jetPatternString: String, targetDropCount: Long): Long =
    keepDroppingRocks(
      0,
      targetDropCount,
      rockStream,
      parse(jetPatternString),
      jetPatternString.length,
      Map.empty.withDefaultValue(SortedSet.empty(Ordering.Long.reverse)),
      Map.empty
    )

  def part1(line: String): Long =
    solve(line, 2022L)

  def part2(line: String): Long =
    solve(line, 1000000000000L)
