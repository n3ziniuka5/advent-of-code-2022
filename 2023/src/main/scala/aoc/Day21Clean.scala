package aoc

import aoc.Common.timed

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object Day21Clean:
    val realInput = true

    val testInput = List(
      "...........",
      ".....###.#.",
      ".###.##..#.",
      "..#.#...#..",
      "....#.#....",
      ".##..S####.",
      ".##..#...#.",
      ".......##..",
      ".##.#.####.",
      ".##..##.##.",
      "...........",
    )

    def main(args: Array[String]): Unit =
        val lines = if (realInput) Source.fromResource("day21.txt").getLines().toList else testInput
        timed("part 2", part2(lines))

    private val maxSteps = 26501365L

    def pathScore(paths: Map[Point, Long]): Long =
        paths.count { (_, steps) =>
            val remainingSteps = maxSteps - steps
            remainingSteps >= 0 && remainingSteps % 2 == 0
        }

    def part2(lines: List[String]): Long =
        val initialMap = Map2DVec.fromLines(lines)

        println(s"maxX ${initialMap.maxX}")
        println(s"maxY ${initialMap.maxY}")

        val initialStartingPoint = Point(initialMap.maxX / 2, initialMap.maxY / 2)
        val initialMapPaths      = shortestPaths(List((initialStartingPoint, 0L)), initialMap)
        val initialMapScore      = pathScore(initialMapPaths)
        val initialStepsTaken    = initialMapPaths.values.max
        println("initial map score " + initialMapScore)
        println

        val oddIterationIncrease  = 30384L
        val evenIterationIncrease = 30308L
        val stepsToAdd            = 131L

        val goingStraightEdgeDistances = List(130, 129, 128, 127, 126, 125, 124, 123, 122, 121, 120, 119, 118, 117, 116,
          115, 114, 113, 112, 111, 110, 109, 108, 107, 106, 105, 104, 103, 102, 101, 100, 99, 98, 97, 96, 95, 94, 93,
          92, 91, 90, 89, 88, 87, 86, 85, 84, 83, 82, 81, 80, 79, 78, 77, 76, 75, 74, 73, 72, 71, 70, 69, 68, 67, 66,
          65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
          92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114,
          115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128, 129, 130).map(_.toLong)

        def expandCross(iteration: Int, score: Long, stepsTaken: Long, crossSize: Long, crossInnerCorners: Long): Long =
            /*val iterationsDone = iteration - 1
            val straightEndSteps = goingStraightEdgeDistances
                .map(_ + (stepsToAdd * iterationsDone))

            val cornerStepsTaken = straightEndSteps.max - stepsToAdd + 2

            println(s"after $iterationsDone iterations")
            println(straightEndSteps)
            println(cornerStepsTaken)
            println

            // println(s"iteration $iteration $score $stepsTaken $crossSize $crossInnerCorners $cornerStepsTaken")

            if (iteration > 5) {
                throw new RuntimeException()
            }*/

            val newStepsTaken = stepsTaken + stepsToAdd
            if (newStepsTaken > maxSteps)
                val iterationsDone = iteration - 1
                println(s"stopped after ${iterationsDone} iterations, cross size is $crossSize $crossInnerCorners")

                val straightEndSteps = goingStraightEdgeDistances
                    .map(_ + (stepsToAdd * iterationsDone))

                val cornerStepsTaken = straightEndSteps.max - stepsToAdd + 2

                println(
                  s"should start corner searches at $cornerStepsTaken, which is ${maxSteps - cornerStepsTaken} away from the end"
                )

                println(straightEndSteps)
                println()
                println(straightEndSteps.max)
                println(maxSteps - straightEndSteps.max)
                println(maxSteps - straightEndSteps.min)
                println(
                  s"corner will have steps taken ${cornerStepsTaken} which is ${maxSteps - cornerStepsTaken} away from the end"
                )

                val cornerScores = List( // from Day21Research file output
                  7601L,
                  7612L,
                  7615L,
                  7618L
                ).map(_ * crossInnerCorners).sum

                // from Day21Research file output
                val upScore    = 7639L
                val downScore  = 7653L
                val leftScore  = 5725L
                val rightScore = 5711L

                score + cornerScores + upScore + downScore + leftScore + rightScore
            else
                val newScore = score + (if (iteration % 2 == 0) evenIterationIncrease * iteration
                                        else oddIterationIncrease * iteration)
                expandCross(iteration + 1, newScore, newStepsTaken, crossSize + 2, crossInnerCorners + 1)

        expandCross(1, initialMapScore, initialStepsTaken, 1, 0)

    def shortestPaths(
        from: List[(Point, Long)],
        map: Map2DVec[Char]
    ): Map[Point, Long] =
        @tailrec
        def loop(
            search: mutable.PriorityQueue[(Point, Long)],
            map: Map2DVec[Char],
            results: Map[Point, Long],
        ): Map[Point, Long] =
            if (search.isEmpty) results
            else
                val (headPoint, headSteps) = search.dequeue()
                // println(s"running search from $headPoint $headSteps")
                if (results.contains(headPoint)) loop(search, map, results)
                else
                    val newResults = results + (headPoint -> headSteps)
                    val newSearches = headPoint.adjacent
                        .filter(_.inBounds(map))
                        .filter(p => map(p) != '#' && map(p) != ' ')
                        .map(p => (p, headSteps + 1))
                    search.enqueue(newSearches: _*)
                    loop(search, map, newResults)

        if (from.sizeIs == 1) {
            if (map(from.head._1) != 'S') throw new RuntimeException("BAD STARTING POINT")
        }

        loop(mutable.PriorityQueue(from: _*)(Ordering.by(_._2 * -1)), map, Map.empty)
