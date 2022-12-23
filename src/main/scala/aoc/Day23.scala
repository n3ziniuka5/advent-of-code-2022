package aoc

import aoc.Common.timed

import scala.annotation.tailrec
import scala.collection.immutable.MultiSet
import scala.io.Source

object Day23:
  def main(args: Array[String]): Unit =
    val lines = Source.fromResource("day23.txt").getLines().toList
    timed("Part 1", part1(lines))
    timed("Part 2", part2(lines))

  case class DirectionDelta(x: Int, y: Int)
  case class DirectionProposal(lookAt: List[DirectionDelta], moveInto: DirectionDelta)
  case class Point(x: Int, y: Int):
    def neighbors: Set[Point] =
      for {
        dX <- Set(-1, 0, 1)
        dY <- Set(-1, 0, 1) if dX != 0 || dY != 0
      } yield Point(x + dX, y + dY)

    def applyDelta(delta: DirectionDelta): Point = Point(x + delta.x, y + delta.y)

  def parse(lines: List[String]): Set[Point] =
    lines.reverse.zipWithIndex.flatMap { (l, y) =>
      l.zipWithIndex.filter(_._1 == '#').map((_, x) => Point(x, y))
    }.toSet

  def proposedDirectionStream: LazyList[DirectionProposal] =
    LazyList(
      DirectionProposal(List(DirectionDelta(0, 1), DirectionDelta(1, 1), DirectionDelta(-1, 1)), DirectionDelta(0, 1)),
      DirectionProposal(
        List(DirectionDelta(0, -1), DirectionDelta(1, -1), DirectionDelta(-1, -1)),
        DirectionDelta(0, -1)
      ),
      DirectionProposal(
        List(DirectionDelta(-1, 0), DirectionDelta(-1, 1), DirectionDelta(-1, -1)),
        DirectionDelta(-1, 0)
      ),
      DirectionProposal(
        List(DirectionDelta(1, 0), DirectionDelta(1, 1), DirectionDelta(1, -1)),
        DirectionDelta(1, 0)
      )
    ) #::: proposedDirectionStream

  def drawElves(elves: Set[Point]): String =
    val minX = elves.minBy(_.x).x
    val maxX = elves.maxBy(_.x).x
    val minY = elves.minBy(_.y).y
    val maxY = elves.maxBy(_.y).y

    println(s"minX - $minX, maxX - $maxX, minY - $minY, maxY - $maxY")

    (for {
      y <- maxY to minY by -1
      x <- minX to maxX
    } yield if (elves.contains(Point(x, y))) '#' else '.').grouped(maxX - minX + 1).map(_.mkString).mkString("\n")

  @tailrec
  def move(maxRounds: Int, elves: Set[Point], proposals: LazyList[DirectionProposal]): Set[Point] =
    if (maxRounds == 0) elves
    else
      val shouldMoveTo = elves.map { location =>
        if (location.neighbors.forall(!elves.contains(_))) {
          (location, location)
        } else {
          proposals.take(4).find { proposal =>
            proposal.lookAt.forall(d => !elves.contains(location.applyDelta(d)))
          } match
            case Some(proposal) => (location, location.applyDelta(proposal.moveInto))
            case None           => (location, location)
        }
      }

      val moveIntoCounts = shouldMoveTo
        .foldLeft(MultiSet.empty[Point]) { case (acc, (_, moveInto)) =>
          acc + moveInto
        }
        .occurrences

      val newElves = shouldMoveTo.map { (current, moveInto) =>
        if (moveIntoCounts(moveInto) > 1) current
        else moveInto
      }

      move(maxRounds - 1, newElves, proposals.tail)

  def part1(lines: List[String]): Int =

    val initialElves = parse(lines)
    val finalElves   = move(10, initialElves, proposedDirectionStream)

    val minX = finalElves.minBy(_.x).x
    val maxX = finalElves.maxBy(_.x).x
    val minY = finalElves.minBy(_.y).y
    val maxY = finalElves.maxBy(_.y).y

    println
    // println(drawElves(initialElves))
    println
    // println(drawElves(finalElves))
    println

    // println(s"minX - $minX, maxX - $maxX, minY - $minY, maxY - $maxY")

    (maxX - minX + 1) * (maxY - minY + 1) - finalElves.size

  def part2(lines: List[String]): Int =
    0
