package aoc

import aoc.Common.timed

import java.nio.file.{Files, Paths}
import scala.io.Source
import collection.mutable
import scala.annotation.tailrec

object Day21Research:
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
        timed("research", research(lines))

    private val maxSteps = 26501365L

    def pathScore(paths: Map[Point, Long]): Long =
        paths.count { (_, steps) =>
            val remainingSteps = maxSteps - steps
            remainingSteps >= 0 && remainingSteps % 2 == 0
        }

    def squareMap(initial: List[String], squareSize: Int): Map2DVec[Char] =
        val wideLines = initial.map(_.repeat(squareSize))
        val tallLines = (1 to squareSize).toList.flatMap(_ => wideLines)
        Map2DVec.fromLines(tallLines)

    def crossMap(initial: List[String], crossSize: Int): Map2DVec[Char] =
        val centerColumn = (1 to crossSize).flatMap(_ => initial).toList

        val rows     = centerColumn.sliding(initial.size, initial.size).toList
        val midPoint = rows.size / 2

        /*println(
              s"${centerColumn.size} grouped by ${initialMap.maxY + 1} ${centerColumn
                      .sliding(initialMap.maxY + 1, initialMap.maxY + 1)
                      .toList
                      .size} ${rows.toList.size}"
            )*/
        val m2 = rows.zipWithIndex.flatMap { (tempLines, rowN) =>
            val blankColsToAdd = math.abs(rowN - midPoint)
            val fullCollsToAdd = (crossSize - 1) / 2 - blankColsToAdd

            tempLines.zipWithIndex.map { (tempLine, l) =>
                val leftSide = " ".repeat(blankColsToAdd * (initial.size)) + initial(l).repeat(
                  fullCollsToAdd
                )
                val rightSide = initial(l).repeat(
                  fullCollsToAdd
                ) + " ".repeat(blankColsToAdd * (initial.size))
                leftSide + tempLine + rightSide
            }
        }

        val m = Map2DVec.fromLines(m2)
        // println(m)
        // println(s"${m.maxX} ${m.maxY}")
        m

    def research(lines: List[String]): Unit =
        val initialMap = Map2DVec.fromLines(lines)

        println(s"maxX ${initialMap.maxX}")
        println(s"maxY ${initialMap.maxY}")

        val initialStartingPoint = Point(initialMap.maxX / 2, initialMap.maxY / 2)
        val initialMapPaths      = shortestPaths(List((initialStartingPoint, 0L)), initialMap)
        val initialMapScore      = pathScore(initialMapPaths)
        println("initial map score " + initialMapScore)
        println

        val linearSeq = List(1, 2, 3, 4, 5, 6, 7)
        val oddSeq    = List(1, 3, 5, 7, 9, 11)

        def diffInSequences(label: String, results: List[(Int, Long, Long)]) =
            println(s"==== $label ====")
            results.sliding(2).foreach { case List((aSquare, aScore, aMaxSteps), (bSquare, bScore, bMaxSteps)) =>
                println(
                  s"$aSquare -> $bSquare: ${bScore - aScore} ${bMaxSteps - aMaxSteps}             ${aScore} $aMaxSteps"
                )
            }
            println("=======================")
            println

        val squareExpansion = oddSeq.map { squareSize =>
            val expandedMap = squareMap(lines, squareSize)
            val expandedMapPaths =
                shortestPaths(List((Point(expandedMap.maxX / 2, expandedMap.maxY / 2), 0L)), expandedMap)
            val score     = pathScore(expandedMapPaths)
            val maxLength = expandedMapPaths.values.max

            val stepsVisualization = (for {
                y <- 0 to expandedMap.maxY
                x <- 0 to expandedMap.maxX
            } yield Point(x, y))
                .map { p =>
                    expandedMapPaths.get(p) match {
                        case Some(steps) =>
                            val stepsStr = steps.toString
                            if (stepsStr.length == 1) s"  $stepsStr  "
                            else if (stepsStr.length == 2) s" $stepsStr  "
                            else s" $stepsStr "
                        case None => "     "
                    }
                }
                .grouped(expandedMap.maxY + 1)
                .map(_.mkString)
                .mkString("\n")

            // println(stepsVisualization)
            // println

            (squareSize, score, maxLength)
        }
        val crossExpansion = oddSeq.map { squareSize =>
            val expandedMap = crossMap(lines, squareSize)
            val expandedMapPaths =
                shortestPaths(List((Point(expandedMap.maxX / 2, expandedMap.maxY / 2), 0L)), expandedMap)
            val score     = pathScore(expandedMapPaths)
            val maxLength = expandedMapPaths.values.max

            val stepsVisualization = (for {
                y <- 0 to expandedMap.maxY
                x <- 0 to expandedMap.maxX
            } yield Point(x, y))
                .map { p =>
                    expandedMapPaths.get(p) match {
                        case Some(steps) =>
                            val stepsStr = steps.toString
                            if (stepsStr.length == 1) s"  $stepsStr  "
                            else if (stepsStr.length == 2) s" $stepsStr  "
                            else s" $stepsStr "
                        case None => "     "
                    }
                }
                .grouped(expandedMap.maxY + 1)
                .map(_.mkString)
                .mkString("\n")

            Files.write(Paths.get(s"cross-expansion-${squareSize}.txt"), stepsVisualization.getBytes())

            // println(stepsVisualization)
            // println

            (squareSize, score, maxLength)
        }

        diffInSequences("square expansion", squareExpansion)
        diffInSequences("cross expansion", crossExpansion)

        val searchingCornerFrom = 26501170L
        val trippleMap          = Map2DVec.fromLines(lines.map(_.repeat(3)))

        val startPaths = List(
          Point(0, trippleMap.maxY),
          Point(0, 0),
          Point(trippleMap.maxX, trippleMap.maxY),
          Point(trippleMap.maxX, 0),
        )

        startPaths.map { p =>
            val paths = shortestPaths(List((p, searchingCornerFrom)), trippleMap)
            val score = pathScore(paths)

            println(s"starting search from ${p} gave $score")

            (p, score)
        }

        val trippleSquareMap = squareMap(lines, 3)

        val goingStraightEdgeSteps = List(26501299, 26501298, 26501297, 26501296, 26501295, 26501294, 26501293,
          26501292, 26501291, 26501290, 26501289, 26501288, 26501287, 26501286, 26501285, 26501284, 26501283, 26501282,
          26501281, 26501280, 26501279, 26501278, 26501277, 26501276, 26501275, 26501274, 26501273, 26501272, 26501271,
          26501270, 26501269, 26501268, 26501267, 26501266, 26501265, 26501264, 26501263, 26501262, 26501261, 26501260,
          26501259, 26501258, 26501257, 26501256, 26501255, 26501254, 26501253, 26501252, 26501251, 26501250, 26501249,
          26501248, 26501247, 26501246, 26501245, 26501244, 26501243, 26501242, 26501241, 26501240, 26501239, 26501238,
          26501237, 26501236, 26501235, 26501234, 26501235, 26501236, 26501237, 26501238, 26501239, 26501240, 26501241,
          26501242, 26501243, 26501244, 26501245, 26501246, 26501247, 26501248, 26501249, 26501250, 26501251, 26501252,
          26501253, 26501254, 26501255, 26501256, 26501257, 26501258, 26501259, 26501260, 26501261, 26501262, 26501263,
          26501264, 26501265, 26501266, 26501267, 26501268, 26501269, 26501270, 26501271, 26501272, 26501273, 26501274,
          26501275, 26501276, 26501277, 26501278, 26501279, 26501280, 26501281, 26501282, 26501283, 26501284, 26501285,
          26501286, 26501287, 26501288, 26501289, 26501290, 26501291, 26501292, 26501293, 26501294, 26501295, 26501296,
          26501297, 26501298, 26501299).map(_.toLong).map(_ + 1)

        val upSearch = (0 to initialMap.maxX).toList
            .map { x =>
                Point(x + initialMap.maxX + 1, trippleSquareMap.maxY)
            }
            .zip(goingStraightEdgeSteps)

        val downSearch = (0 to initialMap.maxX).toList
            .map { x =>
                Point(x + initialMap.maxX + 1, 0)
            }
            .zip(goingStraightEdgeSteps)

        val leftSearch = (0 to trippleMap.maxY).toList
            .map { y =>
                Point(trippleMap.maxX, y)
            }
            .zip(goingStraightEdgeSteps)

        val rightSearch = (0 to trippleMap.maxY).toList
            .map { y =>
                Point(0, y)
            }
            .zip(goingStraightEdgeSteps)

        val upScore = {
            val paths = shortestPaths(upSearch, trippleSquareMap)
            val score = pathScore(paths)

            println(s"up search gave $score")

            score
        }

        val downScore = {
            val paths = shortestPaths(downSearch, trippleSquareMap)
            val score = pathScore(paths)

            println(s"down search gave $score")

            score
        }

        val leftScore = {
            val paths = shortestPaths(leftSearch, trippleMap)
            val score = pathScore(paths)

            println(s"left search gave $score")

            score
        }

        val rightScore = {
            val paths = shortestPaths(rightSearch, trippleMap)
            val score = pathScore(paths)

            println(s"right search gave $score")

            score
        }

        ()

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

        /*if (from.sizeIs == 1) {
            if (map(from.head._1) != 'S') throw new RuntimeException("BAD STARTING POINT")
        }*/

        loop(mutable.PriorityQueue(from: _*)(Ordering.by(_._2 * -1)), map, Map.empty)
