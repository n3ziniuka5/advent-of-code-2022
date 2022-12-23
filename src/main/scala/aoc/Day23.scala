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
    def neighbors: IndexedSeq[Point] =
      for {
        dX <- -1 to 1
        dY <- -1 to 1 if dX != 0 || dY != 0
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

  @tailrec
  def move(
    maxRounds: Int,
    roundsPlayed: Int,
    elves: Set[Point],
    proposals: LazyList[DirectionProposal]
  ): (Set[Point], Int) =
    if (maxRounds == 0) (elves, roundsPlayed)
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

      if (newElves == elves)
        (elves, roundsPlayed + 1)
      else
        move(maxRounds - 1, roundsPlayed + 1, newElves, proposals.tail)

  def part1(lines: List[String]): Int =
    val (finalElves, _) = move(10, 0, parse(lines), proposedDirectionStream)

    val minX = finalElves.minBy(_.x).x
    val maxX = finalElves.maxBy(_.x).x
    val minY = finalElves.minBy(_.y).y
    val maxY = finalElves.maxBy(_.y).y

    (maxX - minX + 1) * (maxY - minY + 1) - finalElves.size

  def part2(lines: List[String]): Int =
    move(Int.MaxValue, 0, parse(lines), proposedDirectionStream)._2
