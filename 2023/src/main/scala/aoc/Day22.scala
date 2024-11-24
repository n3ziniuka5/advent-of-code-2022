package aoc

import aoc.Common.timed

import scala.annotation.tailrec
import scala.collection.immutable.{MultiDict, MultiSet}
import scala.io.Source

object Day22:
    def main(args: Array[String]): Unit =
        val lines = Source.fromResource("day22.txt").getLines().toList
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    case class Xyz(x: Int, y: Int, z: Int)
    case class Brick(start: Xyz, end: Xyz)

    def part1(lines: List[String]): Int =
        val bricks            = parse(lines).sortBy(_.start.z)
        val (dependencies, _) = fallBricks(bricks, MultiDict.empty, Map.empty, 0)

        val cannotRemove = dependencies.flatMap { (brick, dependsOn) =>
            if (dependsOn.size == 1) Some(dependsOn.head) else None
        }.toSet

        bricks.size - cannotRemove.size

    def part2(lines: List[String]): Int =
        val bricks            = parse(lines).sortBy(_.start.z)
        val (dependencies, _) = fallBricks(bricks, MultiDict.empty, Map.empty, 0)

        dependencies.map { (b, _) =>
            val remainingBricks = (dependencies - b).keys.toList.sortBy(_.start.z)
            fallBricks(remainingBricks, MultiDict.empty, Map.empty, 0)._2
        }.sum

    def parse(lines: List[String]): List[Brick] =
        def parseXyz(str: String): Xyz =
            val Array(x, y, z) = str.split(',').map(_.toInt)
            Xyz(x, y, z)
        lines.map { line =>
            val Array(s, e) = line.split('~')
            Brick(parseXyz(s), parseXyz(e))
        }

    @tailrec
    def fallBricks(
        remaining: List[Brick],
        onTheGround: MultiDict[Int, Brick],
        dependencies: Map[Brick, Set[Brick]],
        fallCount: Int,
    ): (Map[Brick, Set[Brick]], Int) =
        remaining match
            case Nil => (dependencies, fallCount)
            case head :: tail =>
                if (head.start.z == 1)
                    fallBricks(
                      tail,
                      onTheGround.add(head.end.z, head),
                      dependencies + (head -> Set.empty),
                      fallCount
                    )
                else
                    def loop(falling: Brick, thisHasFallen: Int): (Brick, Set[Brick], Int) =
                        if (falling.start.z == 1) (falling, Set.empty, thisHasFallen)
                        else
                            val below = onTheGround.get(falling.start.z - 1)
                            val dependenciesBelow = below.filter { b =>
                                val intersectingX =
                                    (falling.start.x >= b.start.x && falling.start.x <= b.end.x) ||
                                        (falling.end.x >= b.start.x && falling.end.x <= b.end.x) ||
                                        (falling.start.x < b.start.x && falling.end.x > b.end.x)
                                val intersectingY =
                                    (falling.start.y >= b.start.y && falling.start.y <= b.end.y) ||
                                        (falling.end.y >= b.start.y && falling.end.y <= b.end.y) ||
                                        (falling.start.y < b.start.y && falling.end.y > b.end.y)

                                intersectingX && intersectingY
                            }.toSet

                            if (dependenciesBelow.isEmpty) {
                                loop(
                                  falling.copy(
                                    start = falling.start.copy(z = falling.start.z - 1),
                                    end = falling.end.copy(z = falling.end.z - 1)
                                  ),
                                  1
                                )
                            } else {
                                (falling, dependenciesBelow, thisHasFallen)
                            }

                    val (fallenBrick, addedDependencies, hasFallen) = loop(head, 0)

                    fallBricks(
                      tail,
                      onTheGround.add(fallenBrick.end.z, fallenBrick),
                      dependencies + (fallenBrick -> addedDependencies),
                      fallCount + hasFallen
                    )
