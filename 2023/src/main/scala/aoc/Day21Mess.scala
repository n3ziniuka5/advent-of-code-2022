package aoc

import aoc.Common.timed

import java.time.Instant
import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.io.Source
import scala.collection.parallel.CollectionConverters.*
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContextExecutor, Future}

object Day21Mess:
    implicit val ec: ExecutionContextExecutor = scala.concurrent.ExecutionContext.global

    def main(args: Array[String]): Unit =
        val lines = Source.fromResource("day21.txt").getLines().toList
        timed("Part 1", part1(lines, 64))
        timed("Part 2", part2(lines, 26501365))

        // 26501365     - total available steps
        // 26501170
        // 26501300

        // timed("Part 2", part2(lines, 1000000))

    var globalMaxSteps = 0L
    def pathScore(p: Map[Point, Long]): Long =
        p.count { case (_, s) =>
            val remainingSteps = globalMaxSteps - s
            remainingSteps >= 0 && remainingSteps % 2 == globalMaxSteps % 2
        }

    def pathScore2(p: Map[Point, (Point, Long)]): Long =
        p.count { case (_, (_, s)) =>
            val remainingSteps = globalMaxSteps - s
            remainingSteps >= 0 && remainingSteps % 2 == globalMaxSteps % 2
        }

    def part1(lines: List[String], maxSteps: Int): Long =
        val map           = Map2DVec.fromLines(lines)
        val startingPoint = Point(map.maxX / 2, map.maxY / 2)
        println(s"part 1 starting point $startingPoint")
        println(s"part 1 width ${map.maxX + 1}")

        shortestPaths(List((startingPoint, 0L)), map).count { case (_, s) =>
            val remainingSteps = maxSteps - s
            remainingSteps >= 0 && remainingSteps % 2 == maxSteps % 2
        }

    var timeSpent             = 0L
    var spentUsingCache: Long = 0L

    val upMapCache    = collection.mutable.Map.empty[Map[Point, Long], (Map[Point, Long], Long)]
    val downMapCache  = collection.mutable.Map.empty[Map[Point, Long], (Map[Point, Long], Long)]
    val leftMapCache  = collection.mutable.Map.empty[Map[Point, Long], (Map[Point, Long], Long)]
    val rightMapCache = collection.mutable.Map.empty[Map[Point, Long], (Map[Point, Long], Long)]

    var cacheHits: Long   = 0L
    var cacheMisses: Long = 0L

    var topScore: Long    = 0L
    var bottomScore: Long = 0L
    var leftScore: Long   = 0L
    var rightScore: Long  = 0L

    def part2(lines: List[String], maxSteps: Int): Long =
        globalMaxSteps = maxSteps
        val initialMap    = Map2DVec.fromLines(lines)
        val startingPoint = Point(initialMap.maxX / 2, initialMap.maxY / 2)

        val centerPaths = shortestPaths(List((startingPoint, 0L)), initialMap)

        @tailrec
        def mapSearches(searches: Queue[Point], maps: Map[Point, (Map[Point, Long], Long)], score: Long): Long =
            if (searches.isEmpty)
                println(s"processed ${cacheHits + cacheMisses} maps, hits $cacheHits misses $cacheMisses")
                println(s"top score $topScore")
                println(s"bottom score $bottomScore")
                println(s"left score $leftScore")
                println(s"right score $rightScore")
                println(s"center score ${pathScore(centerPaths)}")
                score
            else
                val start        = System.currentTimeMillis()
                val (head, rest) = searches.dequeue
                if (maps.contains(head)) mapSearches(rest, maps, score)
                else if (head == Point(0, 0))
                    val newScore    = pathScore(centerPaths)
                    val newMaps     = maps + (head -> (centerPaths, newScore))
                    val newSearches = head.adjacent
                    // val newSearches = List(head.down, head.up)
                    // val newSearches = List(head.up)
                    mapSearches(rest.enqueueAll(newSearches), newMaps, newScore)
                else
                    val hasMapAbove      = maps.contains(Point(head.x, head.y - 1))
                    val hasMapBelow      = maps.contains(Point(head.x, head.y + 1))
                    val hasMapToTheLeft  = maps.contains(Point(head.x - 1, head.y))
                    val hasMapToTheRight = maps.contains(Point(head.x + 1, head.y))

                    /*println(
                      s"searching from $head hasMapAbove $hasMapAbove hasMapBelow $hasMapBelow hasMapToTheLeft $hasMapToTheLeft hasMapToTheRight $hasMapToTheRight"
                    )*/

                    // println(s"currentScore $score")

                    val (newPaths, addedPoints) = if (hasMapAbove) {
                        val (mapAbove, mapAboveScore) = maps(Point(head.x, head.y - 1))

                        val (r, s) = if (upMapCache.contains(mapAbove)) {
                            cacheHits += 1
                            upMapCache(mapAbove)
                        } else {
                            // cacheMisses += 1
                            val searches = (0 to initialMap.maxX).toList.map { x =>
                                val p             = Point(x, 0)
                                val opposingPoint = Point(x, initialMap.maxY)

                                (p, mapAbove(opposingPoint) + 1)
                            }

                            val res = shortestPathsCached(searches, initialMap)
                            // val res2 = res.view.mapValues(_._2).toMap
                            // val s    = pathScore2(res)

                            // println("NOT HITTING CACHE UP")

                            upMapCache.put(mapAbove, (res._1, res._2))

                            (res._1, res._2)
                        }
                        topScore += s

//                        println(
//                          s"above map max point ${mapAbove.maxBy(_._2)} current map max point ${res.maxBy(_._2._2)}"
//                        )

                        /*val score = if (Point(head.x, head.y - 1) != Point(0, 0)) {
                            val key = ("above", mapAboveScore)

                            if (pointsCache.contains(key)) {
                                println(s"above score was ${mapAboveScore} can use cache ${pointsCache(key)}")
                                pointsCache(key)
                            } else {
                                val s = pathScore2(res)
                                pointsCache.put(key, s)
                                s
                            }
                        } else {
                            pathScore2(res)
                        }*/

                        // if (res.maxBy(_._2._2)._2._2 < maxSteps) {
                        /*val aboveScore = pathScore(mapAbove)
                        val resScore   = pathScore2(res)

                        if (resScore == 35) {
                            println(res.filter(_._2._2 > maxSteps))
                        }

                        println(s"above score $aboveScore res score $resScore")*/
                        // }

                        (r, s) // TODO optimize
                    } else if (hasMapBelow) {
                        val (mapBelow, mapBelowScore) = maps(Point(head.x, head.y + 1))

                        val (r, s) = if (downMapCache.contains(mapBelow)) {
                            cacheHits += 1
                            downMapCache(mapBelow)
                        } else {
                            // cacheMisses += 1
                            val searches = (0 to initialMap.maxX).toList.map { x =>
                                val p             = Point(x, initialMap.maxY)
                                val opposingPoint = Point(x, 0)
                                (p, mapBelow(opposingPoint) + 1)
                            }

                            val res = shortestPathsCached(searches, initialMap)
                            // val res2 = res.view.mapValues(_._2).toMap
                            // val s    = pathScore2(res)

                            // println("NOT HITTING CACHE DOWN")

                            downMapCache.put(mapBelow, (res._1, res._2))

                            (res._1, res._2)
                        }
                        bottomScore += s
                        (r, s)

                        /*val pq = mutable.PriorityQueue(searches: _*)(Ordering.by(_._2 * -1))
                        shortestPaths(pq, initialMap, Map.empty)*/

                        // val res = shortestPathsCached(searches, initialMap)

                        /*if (res.maxBy(_._2._2)._2._2 < maxSteps) {
                            val aboveScore = pathScore(mapBelow)
                            val resScore   = pathScore2(res)

                            if (resScore == 35) {
                                println(res)
                            }

                            println(s"below score $aboveScore res score $resScore")
                        }*/

                        // (res.view.mapValues(_._2).toMap, pathScore2(res)) // TODO optimize
                    } else if (hasMapToTheLeft) {
                        val (mapToTheLeft, mapToTheLeftScore) = maps(Point(head.x - 1, head.y))

                        val (r, s) = if (leftMapCache.contains(mapToTheLeft)) {
                            cacheHits += 1
                            leftMapCache(mapToTheLeft)
                        } else {
                            // cacheMisses += 1
                            val searches = (0 to initialMap.maxY).toList.map { y =>
                                val p             = Point(0, y)
                                val opposingPoint = Point(initialMap.maxX, y)
                                (p, mapToTheLeft(opposingPoint) + 1)
                            }

                            val res = shortestPathsCached(searches, initialMap)
                            // val res2 = res.view.mapValues(_._2).toMap
                            // val s    = pathScore2(res)

                            // println("NOT HITTING CACHE LEFT")

                            leftMapCache.put(mapToTheLeft, (res._1, res._2))

                            (res._1, res._2)
                        }
                        leftScore += s

                        (r, s)

                        /*val pq = mutable.PriorityQueue(searches: _*)(Ordering.by(_._2 * -1))
                        shortestPaths(pq, initialMap, Map.empty)*/
                        // val res = shortestPathsCached(searches, initialMap)

                        /*if (res.maxBy(_._2._2)._2._2 < maxSteps) {
                            val aboveScore = pathScore(mapToTheLeft)
                            val resScore   = pathScore2(res)

                            if (resScore == 35) {
                                println(res)
                            }

                            println(s"left score $aboveScore res score $resScore")
                        }*/

                        // (res.view.mapValues(_._2).toMap, pathScore2(res)) // TODO optimize
                    } else if (hasMapToTheRight) {
                        val (mapToTheRight, mapToTheRightScore) = maps(Point(head.x + 1, head.y))

                        val (r, s) = if (rightMapCache.contains(mapToTheRight)) {
                            cacheHits += 1
                            rightMapCache(mapToTheRight)
                        } else {
                            // cacheMisses += 1
                            val searches = (0 to initialMap.maxY).toList.map { y =>
                                val p             = Point(initialMap.maxX, y)
                                val opposingPoint = Point(0, y)
                                (p, mapToTheRight(opposingPoint) + 1)
                            }

                            val res = shortestPathsCached(searches, initialMap)
                            // val res2 = res.view.mapValues(_._2).toMap
                            // val s    = pathScore2(res)

                            // println("NOT HITTING CACHE RIGHT")

                            rightMapCache.put(mapToTheRight, (res._1, res._2))

                            (res._1, res._2)
                        }
                        rightScore += s

                        (r, s)

                        /*val pq = mutable.PriorityQueue(searches: _*)(Ordering.by(_._2 * -1))
                        shortestPaths(pq, initialMap, Map.empty)*/
                        // val res = shortestPathsCached(searches, initialMap)

                        /*if (res.maxBy(_._2._2)._2._2 < maxSteps) {
                            val aboveScore = pathScore(mapToTheRight)
                            val resScore   = pathScore2(res)

                            if (resScore == 35) {
                                println(res)
                            }

                            println(s"right score $aboveScore res score $resScore")
                        }*/

                        // (res.view.mapValues(_._2).toMap, pathScore2(res)) // TODO optimize
                    } else {
                        ???
                    }

                    // val addedScore = pathScore(newPaths)

                    // println(s"$end $start diff - ${end - start}")

                    if (addedPoints > 0) {
                        val newMaps     = maps + (head -> (newPaths, addedPoints))
                        val newSearches = head.adjacent
                        // val newSearches = List(head.down, head.up)
                        // val newSearches = List(head.up)

                        val newQueue = rest.enqueueAll(newSearches)
                        val end      = System.currentTimeMillis()
                        timeSpent = timeSpent + (end - start)
                        mapSearches(newQueue, newMaps, score + addedPoints)
                    } else {
                        val end = System.currentTimeMillis()
                        timeSpent = timeSpent + (end - start)
                        mapSearches(rest, maps, score)
                    }

        timeSpent = 0L
        spentUsingCache = 0L

        cache.clear()
        cache2.clear()
        pointsCache.clear()
        /*val r = mapSearches(Queue(Point(0, 0)), Map.empty, 0)
        println(s"cache size ${cache.keySet.size}")

        println(s"timespent in main loop ${timeSpent}ms")
        println(s"timespent using cache ${spentUsingCache}ms")

        r*/

        /*def leftEdge(m: Map[Point, Long])  = m.filter(_._1.x == 0).toList.sortBy(_._1.y)
        def rightEdge(m: Map[Point, Long]) = m.filter(_._1.x == initialMap.maxX).toList.sortBy(_._1.y)

        def mapToTheLeftSearchPoints(leftEdge: List[(Point, Long)]): List[(Point, Long)] =
            leftEdge.map { case (p, s) =>
                (p.copy(x = initialMap.maxX), s + 1)
            }

        def mapToTheRightSearchPoints(rightEdge: List[(Point, Long)]): List[(Point, Long)] =
            rightEdge.map { case (p, s) =>
                (p.copy(x = 0), s + 1)
            }

        val firstLeftEdge = leftEdge(centerPaths)
        println(s"first left edge ${firstLeftEdge}")

        val firstMapToTheLeft = shortestPaths(mapToTheLeftSearchPoints(firstLeftEdge), initialMap)
        val secondLeftEdge    = leftEdge(firstMapToTheLeft)

        println()
        println(s"second left edge ${secondLeftEdge}")
        println()*/

        // println(firstMapToTheLeft)
        // println()

        /*val secondMapToTheLeft = shortestPaths(mapToTheLeftSearchPoints(secondLeftEdge), initialMap)
        val thirdLeftEdge      = leftEdge(secondMapToTheLeft)
        println(s"third left edge ${thirdLeftEdge}")*/
        // println()

        // println(secondMapToTheLeft)
        // println()

        /*val thirdMapToTheLeft = shortestPaths(mapToTheLeftSearchPoints(thirdLeftEdge), initialMap)
        val fourthLeftEdge    = leftEdge(thirdMapToTheLeft)*/

        // println(s"fourth left edge ${fourthLeftEdge}")
        // println(thirdMapToTheLeft)

        /*val fourthMapToTheLeft = shortestPaths(mapToTheLeftSearchPoints(fourthLeftEdge), initialMap)
        val fifthLeftEdge      = leftEdge(fourthMapToTheLeft)
        println(s"fifth left edge ${fifthLeftEdge}")*/

        /*val adsfsadf = (1 to 20)
            .scanLeft(firstLeftEdge) { (acc, i) =>
                val mapToTheLeft = shortestPaths(mapToTheLeftSearchPoints(acc), initialMap)
                leftEdge(mapToTheLeft)
            }
            .zipWithIndex*/

        /*(1 until leftRepeatsEvery.size).foreach { i =>
            val thisEdge     = leftRepeatsEvery(i)._1
            val previousEdge = leftRepeatsEvery(i - 1)._1

            val firstEdgeMin        = thisEdge.minBy(_._2)._2
            val firstEdgeNormalized = thisEdge.map(a => (a._1, a._2 - firstEdgeMin))
            val leftEdgeMin         = previousEdge.minBy(_._2)._2
            val leftEdgeNormalized  = previousEdge.map(a => (a._1, a._2 - leftEdgeMin))

            /*println(s"firstEdgeNormalized $firstEdgeNormalized")
            println(s"leftEdgeNormalized $leftEdgeNormalized")
            println*/

            /*if (firstEdgeNormalized == leftEdgeNormalized) {
                println(s"$i is the same as ${i - 1}")
            }*/
        }*/

        // val leftRepeatsEvery = 1

        /*.drop(1)
            .find { (leftEdge, _) =>
                val firstEdgeMin        = secondLeftEdge.minBy(_._2)._2
                val firstEdgeNormalized = secondLeftEdge.map(a => (a._1, a._2 - firstEdgeMin))
                val leftEdgeMin         = leftEdge.minBy(_._2)._2
                val leftEdgeNormalized  = leftEdge.map(a => (a._1, a._2 - leftEdgeMin))

                println(s"firstEdgeNormalized $firstEdgeNormalized")
                println(s"leftEdgeNormalized $leftEdgeNormalized")
                println

                firstEdgeNormalized == leftEdgeNormalized
            }*/

        // println(s"left repeats every $leftRepeatsEvery")

        /*val rightRepeatsEvery = (1 to 5)
            .scanLeft(secondRightEdge) { (acc, i) =>
                val mapToTheLeft = shortestPaths(mapToTheLeftSearchPoints(acc), initialMap)
                leftEdge(mapToTheLeft)
            }
            .zipWithIndex
            .drop(1)
            .find { (leftEdge, _) =>
                val firstEdgeMin        = secondLeftEdge.minBy(_._2)._2
                val firstEdgeNormalized = secondLeftEdge.map(a => (a._1, a._2 - firstEdgeMin))
                val leftEdgeMin         = leftEdge.minBy(_._2)._2
                val leftEdgeNormalized  = leftEdge.map(a => (a._1, a._2 - leftEdgeMin))

                firstEdgeNormalized == leftEdgeNormalized
            }

        println(s"right repeats every $leftRepeatsEvery")*/

        def loop(pq: mutable.PriorityQueue[(Point, Long)], visited: Set[Point]): Set[Point] =
            if (pq.isEmpty) visited
            else
                val (head, dist) = pq.dequeue()
                if (visited.contains(head)) loop(pq, visited)
                else if (dist > globalMaxSteps) loop(pq, visited)
                else
                    val left  = head.copy(x = head.x - 131)
                    val right = head.copy(x = head.x + 131)
                    val up    = head.copy(y = head.y - 131)
                    val down  = head.copy(y = head.y + 131)

                    val newSearches = List(left, right, up, down)
                        .map(p => (p, dist + 131))

                    pq.enqueue(newSearches: _*)
                    loop(pq, visited + head)

        val pq = mutable.PriorityQueue((Point(0, 0), 0L))(Ordering.by(_._2 * -1))
        // println(s"running loop until ${globalMaxSteps}")
        // val res = loop(pq, Set.empty)
        // println("loop complete")
        // println(s"visited ${res.size} points")

        /* -------------------------------------- */
        val numIterations =
            List(1, 3, 5, 7, 9 /*, 9, 11, 13, 15, 17, 19, 21 , 23, 25, 27, 29, 31, 33, 36, 39, 41 */ )
        /*val numIterations =
            List(1, 2, 3, 4, 5, 6, 7 /*, 9, 11, 13, 15, 17, 19, 21 , 23, 25, 27, 29, 31, 33, 36, 39, 41 */ )*/

        val results = numIterations.map { repeats =>
            val expandedMap = Map2DVec.fromLines {
                val increasedWidth = lines.map(l => l.repeat(repeats))
                (1 to repeats).flatMap(_ => increasedWidth).toList
            }
            val startingPoint = Point(expandedMap.maxX / 2, expandedMap.maxY / 2)

            // println("MAP CONSTRUCTION COMPLETE")

            val paths = shortestPaths(List((startingPoint, 0L)), expandedMap)
            val ans = paths.count { case (_, s) =>
                val remainingSteps = maxSteps - s
                remainingSteps >= 0 && remainingSteps % 2 == maxSteps % 2
            }

            val topLeft     = paths(Point(0, 0))
            val topRight    = paths(Point(expandedMap.maxX, 0))
            val bottomLeft  = paths(Point(0, expandedMap.maxY))
            val bottomRight = paths(Point(expandedMap.maxX, expandedMap.maxY))

            println(s"steps at corners for repetition $repeats")
            println(s"topLeft ${topLeft}")
            println(s"topRight ${topRight}")
            println(s"bottomLeft ${bottomLeft}")
            println(s"bottomRight ${bottomRight}")
            println("-----")
            println(s"topMid ${paths(Point(expandedMap.maxX / 2, 0))}")
            println(s"bottomMid ${paths(Point(expandedMap.maxX / 2, expandedMap.maxY))}")
            println(s"leftMid ${paths(Point(0, expandedMap.maxY / 2))}")
            println(s"rightMid ${paths(Point(expandedMap.maxX, expandedMap.maxY / 2))}")
            println()

            val maxStepsInSearch = paths.maxBy(_._2)._2

            (repeats, ans, maxStepsInSearch, paths, topLeft, topRight, bottomLeft, bottomRight)

        // println(s"repeats - $repeats ans - $ans")
        }

        val ansGap: Long   = results(1)._2 - results(0)._2
        val topLeftGap     = results(1)._5 - results(0)._5
        val topRightGap    = results(1)._6 - results(0)._6
        val bottomLeftGap  = results(1)._7 - results(0)._7
        val bottomRightGap = results(1)._8 - results(0)._8

        println(s"ANS GAP $ansGap")
        println(s"top left gap $topLeftGap")
        println(s"top right gap $topRightGap")
        println(s"bottom left gap $bottomLeftGap")
        println(s"bottom right gap $bottomRightGap")

        val topLeftStart     = results(0)._5
        val topRightStart    = results(0)._6
        val bottomLeftStart  = results(0)._7
        val bottomRightStart = results(0)._8

        println
        println(s"top left start $topLeftStart")
        println(s"top right start $topRightStart")
        println(s"bottom left start $bottomLeftStart")
        println(s"bottom right start $bottomRightStart")

        def calcLoop(
            topLeft: Long,
            topRight: Long,
            bottomLeft: Long,
            bottomRight: Long,
            rotation: Int,
            score: Long
        ): Long =
            if (topLeft + topLeftGap > maxSteps) {
                println(s"reached top left gap limit at rotation $rotation")
                println(s"topLeft $topLeft topRight $topRight bottomLeft $bottomLeft bottomRight $bottomRight")
                score
            } else if (topRight + topRightGap > maxSteps) {
                println(s"reached top right gap limit at rotation $rotation")
                println(s"topLeft $topLeft topRight $topRight bottomLeft $bottomLeft bottomRight $bottomRight")
                score
            } else if (bottomLeft + bottomLeftGap > maxSteps) {
                println(s"reached bottom left gap limit at rotation $rotation")
                println(s"topLeft $topLeft topRight $topRight bottomLeft $bottomLeft bottomRight $bottomRight")
                score
            } else if (bottomRight + bottomRightGap > maxSteps) {
                println(s"reached bottom right gap limit at rotation $rotation")
                println(s"topLeft $topLeft topRight $topRight bottomLeft $bottomLeft bottomRight $bottomRight")
                score
            } else {
                val newScore = score + ansGap * rotation
                calcLoop(
                  topLeft + topLeftGap,
                  topRight + topRightGap,
                  bottomLeft + bottomLeftGap,
                  bottomRight + bottomRightGap,
                  rotation + 1,
                  newScore
                )
            }

        val r = calcLoop(topLeftStart, topRightStart, bottomLeftStart, bottomRightStart, 1, results(0)._2)

        //        /val topLeftStart = re

        /*val aaa = cornerPoints.map { p =>
            val s              = results(0)._4(p)
            val remainingSteps = maxSteps - s
            if (remainingSteps % 2 == maxSteps % 2) {
                println(s"CORNER GOOD ${s} ${maxSteps - s} ${(maxSteps - s) / 2}")
            }

            maxSteps - s
        }.sum
        println(s"corner sum $aaa")*/

        println
        results.zipWithIndex.foreach { case ((repeats, ans, maxSteps, _, _, _, _, _), i) =>
            val fromPreviousAnd   = if (i == 0) 0 else ans - results(i - 1)._2
            val fromPreviousSteps = if (i == 0) 0 else maxSteps - results(i - 1)._3
            println(s"repeats - $repeats ans - ($ans $fromPreviousAnd) ($maxSteps $fromPreviousSteps)")
        }
        println()
        println("new expansions method starts here ==============================")
        println()

        val resultsWithoutCorners = numIterations.filter(_ % 2 == 1).map { repeats =>
            val expandedMap =
                if (repeats == 1) initialMap
                else
                    val centerColumn = (1 to repeats).flatMap(_ => lines).toList

                    val rows     = centerColumn.sliding(initialMap.maxY + 1, initialMap.maxY + 1).toList
                    val midPoint = rows.size / 2

                    /*println(
                          s"${centerColumn.size} grouped by ${initialMap.maxY + 1} ${centerColumn
                                  .sliding(initialMap.maxY + 1, initialMap.maxY + 1)
                                  .toList
                                  .size} ${rows.toList.size}"
                        )*/
                    val m = rows.zipWithIndex.flatMap { (tempLines, rowN) =>
                        val blankColsToAdd = math.abs(rowN - midPoint)
                        val fullCollsToAdd = (repeats - 1) / 2 - blankColsToAdd

                        tempLines.zipWithIndex.map { (tempLine, l) =>
                            val leftSide = " ".repeat(blankColsToAdd * (initialMap.maxX + 1)) + lines(l).repeat(
                              fullCollsToAdd
                            )
                            val rightSide = lines(l).repeat(
                              fullCollsToAdd
                            ) + " ".repeat(blankColsToAdd * (initialMap.maxX + 1))
                            leftSide + tempLine + rightSide
                        }
                    }

                    /*println
                        println(m.mkString("\n"))
                        println*/

                    Map2DVec.fromLines(m)

            val startingPoint = Point(expandedMap.maxX / 2, expandedMap.maxY / 2)

            // println("MAP CONSTRUCTION COMPLETE")

            val paths = shortestPaths(List((startingPoint, 0L)), expandedMap)
            val ans = paths.count { case (p, s) =>
                val remainingSteps = maxSteps - s
                remainingSteps >= 0 && remainingSteps % 2 == maxSteps % 2
            }

            // val topLeft = paths(Point(0, 0))
            // val topRight = paths(Point(expandedMap.maxX, 0))
            // val bottomLeft = paths(Point(0, expandedMap.maxY))
            // val bottomRight = paths(Point(expandedMap.maxX, expandedMap.maxY))

            /*println(s"steps at corners for repetition $repeats")
            println(s"topLeft ${topLeft}")
            println(s"topRight ${topRight}")
            println(s"bottomLeft ${bottomLeft}")
            println(s"bottomRight ${bottomRight}")
            println("-----")
            println(s"topMid ${paths(Point(expandedMap.maxX / 2, 0))}")
            println(s"bottomMid ${paths(Point(expandedMap.maxX / 2, expandedMap.maxY))}")
            println(s"leftMid ${paths(Point(0, expandedMap.maxY / 2))}")
            println(s"rightMid ${paths(Point(expandedMap.maxX, expandedMap.maxY / 2))}")
            println()*/

            val maxStepsInSearch = paths.maxBy(_._2)._2

            (repeats, ans, maxStepsInSearch, paths)

        // println(s"repeats - $repeats ans - $ans")
        }

        resultsWithoutCorners.zipWithIndex.foreach { case ((repeats, ans, maxSteps, _), i) =>
            val fromPreviousAnd   = if (i == 0) 0 else ans - resultsWithoutCorners(i - 1)._2
            val fromPreviousSteps = if (i == 0) 0 else maxSteps - resultsWithoutCorners(i - 1)._3
            println(s"repeats - $repeats ans - ($ans $fromPreviousAnd) ($maxSteps $fromPreviousSteps)")
        }

        val resultsToTheLeft = numIterations.map { repeats =>
            val expandedMap =
                if (repeats == 1) initialMap
                else
                    val newLines = lines.map(_.repeat(repeats))
                    Map2DVec.fromLines(newLines)

            val startingPoint = Point(expandedMap.maxX - (initialMap.maxX / 2), expandedMap.maxY / 2)
            if (expandedMap(startingPoint) != 'S') {
                println("starting point is not S")
                System.exit(1)
            }

            // println(s"!!!!! ${expandedMap(startingPoint)}")

            // println("MAP CONSTRUCTION COMPLETE")

            val paths = shortestPaths(List((startingPoint, 0L)), expandedMap)
            val ans = paths.count { case (p, s) =>
                val remainingSteps = maxSteps - s
                remainingSteps >= 0 && remainingSteps % 2 == maxSteps % 2
            }

            // val topLeft = paths(Point(0, 0))
            // val topRight = paths(Point(expandedMap.maxX, 0))
            // val bottomLeft = paths(Point(0, expandedMap.maxY))
            // val bottomRight = paths(Point(expandedMap.maxX, expandedMap.maxY))

            /*println(s"steps at corners for repetition $repeats")
            println(s"topLeft ${topLeft}")
            println(s"topRight ${topRight}")
            println(s"bottomLeft ${bottomLeft}")
            println(s"bottomRight ${bottomRight}")
            println("-----")
            println(s"topMid ${paths(Point(expandedMap.maxX / 2, 0))}")
            println(s"bottomMid ${paths(Point(expandedMap.maxX / 2, expandedMap.maxY))}")
            println(s"leftMid ${paths(Point(0, expandedMap.maxY / 2))}")
            println(s"rightMid ${paths(Point(expandedMap.maxX, expandedMap.maxY / 2))}")
            println()*/

            val maxStepsInSearch = paths.maxBy(_._2)._2

            (repeats, ans, maxStepsInSearch, paths)

            // println(s"repeats - $repeats ans - $ans")
        }

        println()
        println("expanding to the left ==============================")
        println()

        resultsToTheLeft.zipWithIndex.foreach { case ((repeats, ans, maxSteps, _), i) =>
            val fromPreviousAnd   = if (i == 0) 0 else ans - resultsToTheLeft(i - 1)._2
            val fromPreviousSteps = if (i == 0) 0 else maxSteps - resultsToTheLeft(i - 1)._3
            println(s"repeats - $repeats ans - ($ans $fromPreviousAnd) ($maxSteps $fromPreviousSteps)")
        }

        val resultsToTheLeft1Up = numIterations.map { repeats =>
            val expandedMap =
                if (repeats == 1) initialMap
                else
                    val newLines = lines.map(_.repeat(repeats))
                    Map2DVec.fromLines(newLines)

            // println(s"!!!!! ${expandedMap(startingPoint)}")

            // println("MAP CONSTRUCTION COMPLETE")

            val edgeDistances = List(261, 260, 259, 258, 257, 256, 255, 254, 253, 252, 251, 250, 249, 248, 247, 246,
              245, 244, 243, 242, 241, 240, 239, 238, 237, 236, 235, 234, 233, 232, 231, 230, 229, 228, 227, 226, 225,
              224, 223, 222, 221, 220, 219, 218, 217, 216, 215, 214, 213, 212, 211, 210, 209, 208, 207, 206, 205, 204,
              203, 202, 201, 200, 199, 198, 197, 196, 195, 194, 193, 192, 191, 190, 189, 188, 187, 186, 185, 184, 183,
              182, 181, 180, 179, 178, 177, 176, 175, 174, 173, 172, 171, 170, 169, 168, 167, 166, 165, 164, 163, 162,
              161, 160, 159, 158, 157, 156, 155, 154, 153, 152, 151, 150, 149, 148, 147, 146, 145, 144, 143, 142, 141,
              140, 139, 138, 137, 136, 135, 134, 133, 132, 131).map(_.toLong)

            val searches = (0 to initialMap.maxY)
                .map { y =>
                    Point(expandedMap.maxX, y)
                }
                .zip(edgeDistances)
                .map((p, d) => (p, d + 1))

            val paths = shortestPaths(searches.toList, expandedMap)
            val ans = paths.count { case (p, s) =>
                val remainingSteps = maxSteps - s
                remainingSteps >= 0 && remainingSteps % 2 == maxSteps % 2
            }

            println(s"printing left edge 1 up HERE ${repeats}")
            val lE = (0 to initialMap.maxY)
                .map { y =>
                    Point(0, y)
                }
                .map(paths)
            println(lE)

            // val topLeft = paths(Point(0, 0))
            // val topRight = paths(Point(expandedMap.maxX, 0))
            // val bottomLeft = paths(Point(0, expandedMap.maxY))
            // val bottomRight = paths(Point(expandedMap.maxX, expandedMap.maxY))

            /*println(s"steps at corners for repetition $repeats")
            println(s"topLeft ${topLeft}")
            println(s"topRight ${topRight}")
            println(s"bottomLeft ${bottomLeft}")
            println(s"bottomRight ${bottomRight}")
            println("-----")
            println(s"topMid ${paths(Point(expandedMap.maxX / 2, 0))}")
            println(s"bottomMid ${paths(Point(expandedMap.maxX / 2, expandedMap.maxY))}")
            println(s"leftMid ${paths(Point(0, expandedMap.maxY / 2))}")
            println(s"rightMid ${paths(Point(expandedMap.maxX, expandedMap.maxY / 2))}")
            println()*/

            val maxStepsInSearch = paths.maxBy(_._2)._2

            (repeats, ans, maxStepsInSearch, paths)

            // println(s"repeats - $repeats ans - $ans")
        }

        println()
        println("expanding to the left 1 up ==============================")
        println()

        resultsToTheLeft1Up.zipWithIndex.foreach { case ((repeats, ans, maxSteps, _), i) =>
            val fromPreviousAnd   = if (i == 0) 0 else ans - resultsToTheLeft1Up(i - 1)._2
            val fromPreviousSteps = if (i == 0) 0 else maxSteps - resultsToTheLeft1Up(i - 1)._3
            println(s"repeats - $repeats ans - ($ans $fromPreviousAnd) ($maxSteps $fromPreviousSteps)")
        }

        val resultsToTheLeft1Down = numIterations.map { repeats =>
            val expandedMap =
                if (repeats == 1) initialMap
                else
                    val newLines = lines.map(_.repeat(repeats))
                    Map2DVec.fromLines(newLines)

            // println(s"!!!!! ${expandedMap(startingPoint)}")

            // println("MAP CONSTRUCTION COMPLETE")

            val edgeDistances = List(261, 260, 259, 258, 257, 256, 255, 254, 253, 252, 251, 250, 249, 248, 247, 246,
              245, 244, 243, 242, 241, 240, 239, 238, 237, 236, 235, 234, 233, 232, 231, 230, 229, 228, 227, 226, 225,
              224, 223, 222, 221, 220, 219, 218, 217, 216, 215, 214, 213, 212, 211, 210, 209, 208, 207, 206, 205, 204,
              203, 202, 201, 200, 199, 198, 197, 196, 195, 194, 193, 192, 191, 190, 189, 188, 187, 186, 185, 184, 183,
              182, 181, 180, 179, 178, 177, 176, 175, 174, 173, 172, 171, 170, 169, 168, 167, 166, 165, 164, 163, 162,
              161, 160, 159, 158, 157, 156, 155, 154, 153, 152, 151, 150, 149, 148, 147, 146, 145, 144, 143, 142, 141,
              140, 139, 138, 137, 136, 135, 134, 133, 132, 131).map(_.toLong).reverse

            val searches = (0 to initialMap.maxY)
                .map { y =>
                    Point(expandedMap.maxX, y)
                }
                .zip(edgeDistances)
                .map((p, d) => (p, d + 1))

            val paths = shortestPaths(searches.toList, expandedMap)
            val ans = paths.count { case (p, s) =>
                val remainingSteps = maxSteps - s
                remainingSteps >= 0 && remainingSteps % 2 == maxSteps % 2
            }

            /*println(s"printing left edge 1 up HERE ${repeats}")
            val lE = (0 to initialMap.maxY)
                .map { y =>
                    Point(0, y)
                }
                .map(paths)
            println(lE)*/

            // val topLeft = paths(Point(0, 0))
            // val topRight = paths(Point(expandedMap.maxX, 0))
            // val bottomLeft = paths(Point(0, expandedMap.maxY))
            // val bottomRight = paths(Point(expandedMap.maxX, expandedMap.maxY))

            /*println(s"steps at corners for repetition $repeats")
            println(s"topLeft ${topLeft}")
            println(s"topRight ${topRight}")
            println(s"bottomLeft ${bottomLeft}")
            println(s"bottomRight ${bottomRight}")
            println("-----")
            println(s"topMid ${paths(Point(expandedMap.maxX / 2, 0))}")
            println(s"bottomMid ${paths(Point(expandedMap.maxX / 2, expandedMap.maxY))}")
            println(s"leftMid ${paths(Point(0, expandedMap.maxY / 2))}")
            println(s"rightMid ${paths(Point(expandedMap.maxX, expandedMap.maxY / 2))}")
            println()*/

            val maxStepsInSearch = paths.maxBy(_._2)._2

            (repeats, ans, maxStepsInSearch, paths)

            // println(s"repeats - $repeats ans - $ans")
        }

        println()
        println("expanding to the left 1 down ==============================")
        println()

        resultsToTheLeft1Down.zipWithIndex.foreach { case ((repeats, ans, maxSteps, _), i) =>
            val fromPreviousAnd   = if (i == 0) 0 else ans - resultsToTheLeft1Down(i - 1)._2
            val fromPreviousSteps = if (i == 0) 0 else maxSteps - resultsToTheLeft1Down(i - 1)._3
            println(s"repeats - $repeats ans - ($ans $fromPreviousAnd) ($maxSteps $fromPreviousSteps)")
        }

        val resultsToTheRight1Up = numIterations.map { repeats =>
            val expandedMap =
                if (repeats == 1) initialMap
                else
                    val newLines = lines.map(_.repeat(repeats))
                    Map2DVec.fromLines(newLines)

            val startingPoint = Point(expandedMap.maxX - (initialMap.maxX / 2), expandedMap.maxY / 2)
            if (expandedMap(startingPoint) != 'S') {
                println("starting point is not S")
                System.exit(1)
            }

            // println(s"!!!!! ${expandedMap(startingPoint)}")

            // println("MAP CONSTRUCTION COMPLETE")

            val edgeDistances = List(261, 260, 259, 258, 257, 256, 255, 254, 253, 252, 251, 250, 249, 248, 247, 246,
              245, 244, 243, 242, 241, 240, 239, 238, 237, 236, 235, 234, 233, 232, 231, 230, 229, 228, 227, 226, 225,
              224, 223, 222, 221, 220, 219, 218, 217, 216, 215, 214, 213, 212, 211, 210, 209, 208, 207, 206, 205, 204,
              203, 202, 201, 200, 199, 198, 197, 196, 195, 194, 193, 192, 191, 190, 189, 188, 187, 186, 185, 184, 183,
              182, 181, 180, 179, 178, 177, 176, 175, 174, 173, 172, 171, 170, 169, 168, 167, 166, 165, 164, 163, 162,
              161, 160, 159, 158, 157, 156, 155, 154, 153, 152, 151, 150, 149, 148, 147, 146, 145, 144, 143, 142, 141,
              140, 139, 138, 137, 136, 135, 134, 133, 132, 131).map(_.toLong)

            val searches = (0 to initialMap.maxY)
                .map { y =>
                    Point(0, y)
                }
                .zip(edgeDistances)
                .map((p, d) => (p, d + 1))

            val paths = shortestPaths(searches.toList, expandedMap)
            val ans = paths.count { case (p, s) =>
                val remainingSteps = maxSteps - s
                remainingSteps >= 0 && remainingSteps % 2 == maxSteps % 2
            }

            // println(s"printing left edge 1 up HERE ${repeats}")
            val lE = (0 to initialMap.maxY)
                .map { y =>
                    Point(0, y)
                }
                .map(paths)
            // println(lE)

            // val topLeft = paths(Point(0, 0))
            // val topRight = paths(Point(expandedMap.maxX, 0))
            // val bottomLeft = paths(Point(0, expandedMap.maxY))
            // val bottomRight = paths(Point(expandedMap.maxX, expandedMap.maxY))

            /*println(s"steps at corners for repetition $repeats")
            println(s"topLeft ${topLeft}")
            println(s"topRight ${topRight}")
            println(s"bottomLeft ${bottomLeft}")
            println(s"bottomRight ${bottomRight}")
            println("-----")
            println(s"topMid ${paths(Point(expandedMap.maxX / 2, 0))}")
            println(s"bottomMid ${paths(Point(expandedMap.maxX / 2, expandedMap.maxY))}")
            println(s"leftMid ${paths(Point(0, expandedMap.maxY / 2))}")
            println(s"rightMid ${paths(Point(expandedMap.maxX, expandedMap.maxY / 2))}")
            println()*/

            val maxStepsInSearch = paths.maxBy(_._2)._2

            (repeats, ans, maxStepsInSearch, paths)

            // println(s"repeats - $repeats ans - $ans")
        }

        println()
        println("expanding to the right 1 up ==============================")
        println()

        resultsToTheRight1Up.zipWithIndex.foreach { case ((repeats, ans, maxSteps, _), i) =>
            val fromPreviousAnd   = if (i == 0) 0 else ans - resultsToTheRight1Up(i - 1)._2
            val fromPreviousSteps = if (i == 0) 0 else maxSteps - resultsToTheRight1Up(i - 1)._3
            println(s"repeats - $repeats ans - ($ans $fromPreviousAnd) ($maxSteps $fromPreviousSteps)")
        }

        val resultsToTheLeft2Up = numIterations.map { repeats =>
            val expandedMap =
                if (repeats == 1) initialMap
                else
                    val newLines = lines.map(_.repeat(repeats))
                    Map2DVec.fromLines(newLines)

            val startingPoint = Point(expandedMap.maxX - (initialMap.maxX / 2), expandedMap.maxY / 2)
            if (expandedMap(startingPoint) != 'S') {
                println("starting point is not S")
                System.exit(1)
            }

            // println(s"!!!!! ${expandedMap(startingPoint)}")

            // println("MAP CONSTRUCTION COMPLETE")

            val edgeDistances = List(261, 260, 259, 258, 257, 256, 255, 254, 253, 252, 251, 250, 249, 248, 247, 246,
              245, 244, 243, 242, 241, 240, 239, 238, 237, 236, 235, 234, 233, 232, 231, 230, 229, 228, 227, 226, 225,
              224, 223, 222, 221, 220, 219, 218, 217, 216, 215, 214, 213, 212, 211, 210, 209, 208, 207, 206, 205, 204,
              203, 202, 201, 200, 199, 198, 197, 196, 195, 194, 193, 192, 191, 190, 189, 188, 187, 186, 185, 184, 183,
              182, 181, 180, 179, 178, 177, 176, 175, 174, 173, 172, 171, 170, 169, 168, 167, 166, 165, 164, 163, 162,
              161, 160, 159, 158, 157, 156, 155, 154, 153, 152, 151, 150, 149, 148, 147, 146, 145, 144, 143, 142, 141,
              140, 139, 138, 137, 136, 135, 134, 133, 132, 131).map(_.toLong + 131)

            val searches = (0 to initialMap.maxY)
                .map { y =>
                    Point(expandedMap.maxX, y)
                }
                .zip(edgeDistances)
                .map((p, d) => (p, d + 1))

            val paths = shortestPaths(searches.toList, expandedMap)
            val ans = paths.count { case (p, s) =>
                val remainingSteps = maxSteps - s
                remainingSteps >= 0 && remainingSteps % 2 == maxSteps % 2
            }

            println(s"printing left edge 2 up HERE ${repeats}")
            val lE = (0 to initialMap.maxY)
                .map { y =>
                    Point(0, y)
                }
                .map(paths)
            println(lE)

            // val topLeft = paths(Point(0, 0))
            // val topRight = paths(Point(expandedMap.maxX, 0))
            // val bottomLeft = paths(Point(0, expandedMap.maxY))
            // val bottomRight = paths(Point(expandedMap.maxX, expandedMap.maxY))

            /*println(s"steps at corners for repetition $repeats")
            println(s"topLeft ${topLeft}")
            println(s"topRight ${topRight}")
            println(s"bottomLeft ${bottomLeft}")
            println(s"bottomRight ${bottomRight}")
            println("-----")
            println(s"topMid ${paths(Point(expandedMap.maxX / 2, 0))}")
            println(s"bottomMid ${paths(Point(expandedMap.maxX / 2, expandedMap.maxY))}")
            println(s"leftMid ${paths(Point(0, expandedMap.maxY / 2))}")
            println(s"rightMid ${paths(Point(expandedMap.maxX, expandedMap.maxY / 2))}")
            println()*/

            val maxStepsInSearch = paths.maxBy(_._2)._2

            (repeats, ans, maxStepsInSearch, paths)

            // println(s"repeats - $repeats ans - $ans")
        }

        println()
        println("expanding to the left 2 up ==============================")
        println()

        resultsToTheLeft2Up.zipWithIndex.foreach { case ((repeats, ans, maxSteps, _), i) =>
            val fromPreviousAnd   = if (i == 0) 0 else ans - resultsToTheLeft2Up(i - 1)._2
            val fromPreviousSteps = if (i == 0) 0 else maxSteps - resultsToTheLeft2Up(i - 1)._3
            println(s"repeats - $repeats ans - ($ans $fromPreviousAnd) ($maxSteps $fromPreviousSteps)")
        }

        val resultsToTheRight2Up = numIterations.map { repeats =>
            val expandedMap =
                if (repeats == 1) initialMap
                else
                    val newLines = lines.map(_.repeat(repeats))
                    Map2DVec.fromLines(newLines)

            val startingPoint = Point(expandedMap.maxX - (initialMap.maxX / 2), expandedMap.maxY / 2)
            if (expandedMap(startingPoint) != 'S') {
                println("starting point is not S")
                System.exit(1)
            }

            // println(s"!!!!! ${expandedMap(startingPoint)}")

            // println("MAP CONSTRUCTION COMPLETE")

            val edgeDistances = List(261, 260, 259, 258, 257, 256, 255, 254, 253, 252, 251, 250, 249, 248, 247, 246,
              245, 244, 243, 242, 241, 240, 239, 238, 237, 236, 235, 234, 233, 232, 231, 230, 229, 228, 227, 226, 225,
              224, 223, 222, 221, 220, 219, 218, 217, 216, 215, 214, 213, 212, 211, 210, 209, 208, 207, 206, 205, 204,
              203, 202, 201, 200, 199, 198, 197, 196, 195, 194, 193, 192, 191, 190, 189, 188, 187, 186, 185, 184, 183,
              182, 181, 180, 179, 178, 177, 176, 175, 174, 173, 172, 171, 170, 169, 168, 167, 166, 165, 164, 163, 162,
              161, 160, 159, 158, 157, 156, 155, 154, 153, 152, 151, 150, 149, 148, 147, 146, 145, 144, 143, 142, 141,
              140, 139, 138, 137, 136, 135, 134, 133, 132, 131).map(_.toLong + 131)

            val searches = (0 to initialMap.maxY)
                .map { y =>
                    Point(0, y)
                }
                .zip(edgeDistances)
                .map((p, d) => (p, d + 1))

            val paths = shortestPaths(searches.toList, expandedMap)
            val ans = paths.count { case (p, s) =>
                val remainingSteps = maxSteps - s
                remainingSteps >= 0 && remainingSteps % 2 == maxSteps % 2
            }

            println(s"printing left edge 2 up HERE ${repeats}")
            val lE = (0 to initialMap.maxY)
                .map { y =>
                    Point(0, y)
                }
                .map(paths)
            println(lE)

            // val topLeft = paths(Point(0, 0))
            // val topRight = paths(Point(expandedMap.maxX, 0))
            // val bottomLeft = paths(Point(0, expandedMap.maxY))
            // val bottomRight = paths(Point(expandedMap.maxX, expandedMap.maxY))

            /*println(s"steps at corners for repetition $repeats")
            println(s"topLeft ${topLeft}")
            println(s"topRight ${topRight}")
            println(s"bottomLeft ${bottomLeft}")
            println(s"bottomRight ${bottomRight}")
            println("-----")
            println(s"topMid ${paths(Point(expandedMap.maxX / 2, 0))}")
            println(s"bottomMid ${paths(Point(expandedMap.maxX / 2, expandedMap.maxY))}")
            println(s"leftMid ${paths(Point(0, expandedMap.maxY / 2))}")
            println(s"rightMid ${paths(Point(expandedMap.maxX, expandedMap.maxY / 2))}")
            println()*/

            val maxStepsInSearch = paths.maxBy(_._2)._2

            (repeats, ans, maxStepsInSearch, paths)

            // println(s"repeats - $repeats ans - $ans")
        }

        println()
        println("expanding to the right 2 up ==============================")
        println()

        resultsToTheRight2Up.zipWithIndex.foreach { case ((repeats, ans, maxSteps, _), i) =>
            val fromPreviousAnd   = if (i == 0) 0 else ans - resultsToTheRight2Up(i - 1)._2
            val fromPreviousSteps = if (i == 0) 0 else maxSteps - resultsToTheRight2Up(i - 1)._3
            println(s"repeats - $repeats ans - ($ans $fromPreviousAnd) ($maxSteps $fromPreviousSteps)")
        }

        val resultsToTheRight = numIterations.map { repeats =>
            val expandedMap =
                if (repeats == 1) initialMap
                else
                    val newLines = lines.map(_.repeat(repeats))
                    Map2DVec.fromLines(newLines)

            val startingPoint = Point(initialMap.maxX / 2, expandedMap.maxY / 2)
            if (expandedMap(startingPoint) != 'S') {
                println("starting point is not S")
                System.exit(1)
            }

            // println(s"!!!!! ${expandedMap(startingPoint)}")

            // println("MAP CONSTRUCTION COMPLETE")

            val paths = shortestPaths(List((startingPoint, 0L)), expandedMap)
            val ans = paths.count { case (p, s) =>
                val remainingSteps = maxSteps - s
                remainingSteps >= 0 && remainingSteps % 2 == maxSteps % 2
            }

            // val topLeft = paths(Point(0, 0))
            // val topRight = paths(Point(expandedMap.maxX, 0))
            // val bottomLeft = paths(Point(0, expandedMap.maxY))
            // val bottomRight = paths(Point(expandedMap.maxX, expandedMap.maxY))

            /*println(s"steps at corners for repetition $repeats")
            println(s"topLeft ${topLeft}")
            println(s"topRight ${topRight}")
            println(s"bottomLeft ${bottomLeft}")
            println(s"bottomRight ${bottomRight}")
            println("-----")
            println(s"topMid ${paths(Point(expandedMap.maxX / 2, 0))}")
            println(s"bottomMid ${paths(Point(expandedMap.maxX / 2, expandedMap.maxY))}")
            println(s"leftMid ${paths(Point(0, expandedMap.maxY / 2))}")
            println(s"rightMid ${paths(Point(expandedMap.maxX, expandedMap.maxY / 2))}")
            println()*/

            val maxStepsInSearch = paths.maxBy(_._2)._2

            (repeats, ans, maxStepsInSearch, paths)

            // println(s"repeats - $repeats ans - $ans")
        }

        println()
        println("expanding to the right ==============================")
        println()

        resultsToTheRight.zipWithIndex.foreach { case ((repeats, ans, maxSteps, _), i) =>
            val fromPreviousAnd   = if (i == 0) 0 else ans - resultsToTheRight(i - 1)._2
            val fromPreviousSteps = if (i == 0) 0 else maxSteps - resultsToTheRight(i - 1)._3
            println(s"repeats - $repeats ans - ($ans $fromPreviousAnd) ($maxSteps $fromPreviousSteps)")
        }

        val resultsUp = numIterations.map { repeats =>
            val expandedMap =
                if (repeats == 1) initialMap
                else
                    val newLines = (1 to repeats).flatMap(_ => lines).toList
                    Map2DVec.fromLines(newLines)

            val startingPoint = Point(initialMap.maxX / 2, expandedMap.maxY - (initialMap.maxY / 2))

            if (expandedMap(startingPoint) != 'S') {
                println("starting point is not S")
                System.exit(1)
            }

            // println("MAP CONSTRUCTION COMPLETE")

            val paths = shortestPaths(List((startingPoint, 0L)), expandedMap)
            val ans = paths.count { case (p, s) =>
                val remainingSteps = maxSteps - s
                remainingSteps >= 0 && remainingSteps % 2 == maxSteps % 2
            }

            // val topLeft = paths(Point(0, 0))
            // val topRight = paths(Point(expandedMap.maxX, 0))
            // val bottomLeft = paths(Point(0, expandedMap.maxY))
            // val bottomRight = paths(Point(expandedMap.maxX, expandedMap.maxY))

            /*println(s"steps at corners for repetition $repeats")
            println(s"topLeft ${topLeft}")
            println(s"topRight ${topRight}")
            println(s"bottomLeft ${bottomLeft}")
            println(s"bottomRight ${bottomRight}")
            println("-----")
            println(s"topMid ${paths(Point(expandedMap.maxX / 2, 0))}")
            println(s"bottomMid ${paths(Point(expandedMap.maxX / 2, expandedMap.maxY))}")
            println(s"leftMid ${paths(Point(0, expandedMap.maxY / 2))}")
            println(s"rightMid ${paths(Point(expandedMap.maxX, expandedMap.maxY / 2))}")
            println()*/

            val maxStepsInSearch = paths.maxBy(_._2)._2

            (repeats, ans, maxStepsInSearch, paths)

            // println(s"repeats - $repeats ans - $ans")
        }

        println()
        println("expanding upwards ==============================")
        println()

        resultsUp.zipWithIndex.foreach { case ((repeats, ans, maxSteps, _), i) =>
            val fromPreviousAnd   = if (i == 0) 0 else ans - resultsUp(i - 1)._2
            val fromPreviousSteps = if (i == 0) 0 else maxSteps - resultsUp(i - 1)._3
            println(s"repeats - $repeats ans - ($ans $fromPreviousAnd) ($maxSteps $fromPreviousSteps)")
        }

        val resultsDown = numIterations.map { repeats =>
            val expandedMap =
                if (repeats == 1) initialMap
                else
                    val newLines = (1 to repeats).flatMap(_ => lines).toList
                    Map2DVec.fromLines(newLines)

            val startingPoint = Point(initialMap.maxX / 2, initialMap.maxY / 2)

            if (expandedMap(startingPoint) != 'S') {
                println("starting point is not S")
                System.exit(1)
            }

            // println("MAP CONSTRUCTION COMPLETE")

            val paths = shortestPaths(List((startingPoint, 0L)), expandedMap)
            val ans = paths.count { case (p, s) =>
                val remainingSteps = maxSteps - s
                remainingSteps >= 0 && remainingSteps % 2 == maxSteps % 2
            }

            // val topLeft = paths(Point(0, 0))
            // val topRight = paths(Point(expandedMap.maxX, 0))
            // val bottomLeft = paths(Point(0, expandedMap.maxY))
            // val bottomRight = paths(Point(expandedMap.maxX, expandedMap.maxY))

            /*println(s"steps at corners for repetition $repeats")
            println(s"topLeft ${topLeft}")
            println(s"topRight ${topRight}")
            println(s"bottomLeft ${bottomLeft}")
            println(s"bottomRight ${bottomRight}")
            println("-----")
            println(s"topMid ${paths(Point(expandedMap.maxX / 2, 0))}")
            println(s"bottomMid ${paths(Point(expandedMap.maxX / 2, expandedMap.maxY))}")
            println(s"leftMid ${paths(Point(0, expandedMap.maxY / 2))}")
            println(s"rightMid ${paths(Point(expandedMap.maxX, expandedMap.maxY / 2))}")
            println()*/

            val maxStepsInSearch = paths.maxBy(_._2)._2

            (repeats, ans, maxStepsInSearch, paths)

            // println(s"repeats - $repeats ans - $ans")
        }

        println()
        println("expanding bottom ==============================")
        println()

        resultsDown.zipWithIndex.foreach { case ((repeats, ans, maxSteps, _), i) =>
            val fromPreviousAnd   = if (i == 0) 0 else ans - resultsDown(i - 1)._2
            val fromPreviousSteps = if (i == 0) 0 else maxSteps - resultsDown(i - 1)._3
            println(s"repeats - $repeats ans - ($ans $fromPreviousAnd) ($maxSteps $fromPreviousSteps)")
        }

        val topEdge     = (0 to initialMap.maxX).map(x => Point(x, 0)).toList.map(centerPaths)
        val topEdge1Up  = (0 to initialMap.maxX).map(x => Point(x, 0)).toList.map(resultsUp(1)._4)
        val topEdge2Up  = (0 to initialMap.maxX).map(x => Point(x, 0)).toList.map(resultsUp(2)._4)
        val leftEdge1Up = (0 to initialMap.maxY).map(y => Point(0, y)).toList.map(resultsUp(1)._4)

        val bottomEdge = (0 to initialMap.maxX).map(x => Point(x, initialMap.maxY)).toList.map(centerPaths)
        val leftEdge   = (0 to initialMap.maxY).map(y => Point(0, y)).toList.map(centerPaths)
        val rightEdge  = (0 to initialMap.maxY).map(y => Point(initialMap.maxX, y)).toList.map(centerPaths)

        println()
        println(s"top edge $topEdge")
        println(s"top edge 1 up ${topEdge1Up}")
        println(s"top edge 2 up ${topEdge2Up}")
        println(s"left edge 1 up ${leftEdge1Up}")

        println(s"bottom edge $bottomEdge")
        println(s"left edge $leftEdge")
        println(s"right edge $rightEdge")
        println(s"all equal ${topEdge == bottomEdge && bottomEdge == leftEdge && leftEdge == rightEdge}")

        println("calculating =====================================")
        val oddIterationAddition  = 7577L
        val evenIterationAddition = 7596L
        val stepsToAdd            = 131L
        val stepsToStartWith      = 130L
        val doubleScore           = oddIterationAddition + evenIterationAddition

        val topEdgeDistances = List(130, 129, 128, 127, 126, 125, 124, 123, 122, 121, 120, 119, 118, 117, 116, 115, 114,
          113, 112, 111, 110, 109, 108, 107, 106, 105, 104, 103, 102, 101, 100, 99, 98, 97, 96, 95, 94, 93, 92, 91, 90,
          89, 88, 87, 86, 85, 84, 83, 82, 81, 80, 79, 78, 77, 76, 75, 74, 73, 72, 71, 70, 69, 68, 67, 66, 65, 66, 67,
          68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94,
          95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117,
          118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128, 129, 130).map(_.toLong)

        val leftEdgeDistances = List(261, 260, 259, 258, 257, 256, 255, 254, 253, 252, 251, 250, 249, 248, 247, 246,
          245, 244, 243, 242, 241, 240, 239, 238, 237, 236, 235, 234, 233, 232, 231, 230, 229, 228, 227, 226, 225, 224,
          223, 222, 221, 220, 219, 218, 217, 216, 215, 214, 213, 212, 211, 210, 209, 208, 207, 206, 205, 204, 203, 202,
          201, 200, 199, 198, 197, 196, 195, 194, 193, 192, 191, 190, 189, 188, 187, 186, 185, 184, 183, 182, 181, 180,
          179, 178, 177, 176, 175, 174, 173, 172, 171, 170, 169, 168, 167, 166, 165, 164, 163, 162, 161, 160, 159, 158,
          157, 156, 155, 154, 153, 152, 151, 150, 149, 148, 147, 146, 145, 144, 143, 142, 141, 140, 139, 138, 137, 136,
          135, 134, 133, 132, 131).map(_.toLong)

        def doubleLoopLeftFromTop(
            iteration: Long,
            totalStepsTaken: Long,
            score: Long,
            leftEdgeAtTheStart: List[Long],
        ): Long =
            if (totalStepsTaken + (stepsToAdd * 2) > maxSteps) {
                val scoreToAdd = { // if (iteration == 1) {
                    val rightEdgeBeforeStart = leftEdgeAtTheStart.map(_ + ((iteration - 1) * stepsToAdd * 2)).map(_ + 1)

                    val trippleMap = Map2DVec.fromLines(lines.map(_.repeat(3)))

                    val search = (0 to trippleMap.maxY)
                        .map { y =>
                            Point(trippleMap.maxX, y)
                        }
                        .zip(rightEdgeBeforeStart)
                        .toList

                    val lastPaths = shortestPaths(search, trippleMap)
                    val lastScore = pathScore(lastPaths)

                    println(s"adding $lastScore")
                    lastScore
                } /* else {
                    7561
                }*/

                score + scoreToAdd
            } else {
                doubleLoopLeftFromTop(
                  iteration + 1,
                  totalStepsTaken + (stepsToAdd * 2),
                  score + doubleScore,
                  leftEdgeAtTheStart
                )
            }

        var toAddForTop    = 0L
        var toAddForBottom = 0L

        var toAddForSideGoingUp   = 0L
        var toAddForSideGoingDown = 0L

        @tailrec
        def loopLeftFromTop(
            iteration: Long,
            totalStepsTaken: Long,
            score: Long,
            leftEdgeAtTheStart: List[Long],
            oddModifier: Long
        ): Long =
            if (totalStepsTaken + stepsToAdd > maxSteps) {
                /*val rightEdgeBeforeStart = leftEdgeAtTheStart.map(_ + ((iteration - 1) * stepsToAdd)).map(_ + 1)

                val trippleMap = Map2DVec.fromLines(lines.map(_.repeat(3)))

                val search = (0 to initialMap.maxY)
                    .map { y =>
                        Point(initialMap.maxX, y)
                    }
                    .zip(rightEdgeBeforeStart)
                    .toList

                val lastPaths = shortestPaths(search, trippleMap)
                val lastScore = pathScore(lastPaths)

                println(s"calculated remainder $lastScore")*/

                /*val scoreToAdd = if (iteration == 1) {
                    val rightEdgeBeforeStart = leftEdgeAtTheStart.map(_ + ((iteration - 1) * stepsToAdd)).map(_ + 1)

                    val trippleMap = Map2DVec.fromLines(lines.map(_.repeat(3)))

                    val search = (0 to trippleMap.maxY)
                        .map { y =>
                            Point(trippleMap.maxX, y)
                        }
                        .zip(rightEdgeBeforeStart)
                        .toList

                    val lastPaths = shortestPaths(search, trippleMap)
                    val lastScore = pathScore(lastPaths)

                    println(s"added $lastScore")
                    lastScore
                } else {
                    7561
                }*/

                val scoreToAddGoingUp = if (iteration == 1) {
                    val rightEdgeBeforeStart = leftEdgeAtTheStart.map(_ + ((iteration - 1) * stepsToAdd)).map(_ + 1)

                    val trippleMap = Map2DVec.fromLines(lines.map(_.repeat(3)))

                    val search = (0 to trippleMap.maxY)
                        .map { y =>
                            Point(trippleMap.maxX, y)
                        }
                        .zip(rightEdgeBeforeStart)
                        .toList

                    val lastPaths = shortestPaths(search, trippleMap)
                    val lastScore = pathScore(lastPaths)

                    // println(s"added $lastScore")
                    lastScore
                } else {
                    7561
                }

                val scoreToAddGoingDown = if (iteration == 1) {
                    val rightEdgeBeforeStart =
                        leftEdgeAtTheStart.map(_ + ((iteration - 1) * stepsToAdd)).map(_ + 1).reverse

                    val trippleMap = Map2DVec.fromLines(lines.map(_.repeat(3)))

                    val search = (0 to trippleMap.maxY)
                        .map { y =>
                            Point(trippleMap.maxX, y)
                        }
                        .zip(rightEdgeBeforeStart)
                        .toList

                    val lastPaths = shortestPaths(search, trippleMap)
                    val lastScore = pathScore(lastPaths)

                    // println(s"added $lastScore")
                    lastScore
                } else {
                    7572
                }

                toAddForSideGoingUp += scoreToAddGoingUp
                toAddForSideGoingDown += scoreToAddGoingDown

                // println(s"score to add for left going up ${scoreToAddGoingUp}")
                // println(s"score to add for left going down ${scoreToAddGoingDown}")

                // println(s"adding additional $scoreToAdd $oddModifier")
                // throw new RuntimeException()

                score
            } else {
                val scoreToAdd = if ((iteration + oddModifier) % 2 == 0) evenIterationAddition else oddIterationAddition

                /*if (oddModifier > 2) {
                    val manualScore = {
                        val topEdgeDistanceAfterIterations =
                            leftEdgeAtTheStart.map(_ + ((iteration - 1) * stepsToAdd)).map(_ + 1)

                        val search = (0 to initialMap.maxY)
                            .map { y =>
                                Point(initialMap.maxX, y)
                            }
                            .zip(topEdgeDistanceAfterIterations)
                            .toList

                        val lastPaths = shortestPaths(search, initialMap)
                        pathScore(lastPaths)

                    }

                    if (scoreToAdd != manualScore) {
                        println(
                          s"iteration $iteration oddModifier $oddModifier thisTileScore $scoreToAdd manualScore $manualScore"
                        )
                        throw new RuntimeException()
                    } else {
                        println("identical score")
                    }
                }*/

                loopLeftFromTop(
                  iteration + 1,
                  totalStepsTaken + stepsToAdd,
                  score + scoreToAdd,
                  leftEdgeAtTheStart,
                  oddModifier
                )
            }

        @tailrec
        def loopRightFromTop(
            iteration: Long,
            totalStepsTaken: Long,
            score: Long,
            leftEdgeAtTheStart: List[Long],
            oddModifier: Long
        ): Long =
            if (totalStepsTaken + stepsToAdd > maxSteps) {
                /*val rightEdgeBeforeStart = leftEdgeAtTheStart.map(_ + ((iteration - 1) * stepsToAdd)).map(_ + 1)

                val trippleMap = Map2DVec.fromLines(lines.map(_.repeat(3)))

                val search = (0 to initialMap.maxY)
                    .map { y =>
                        Point(initialMap.maxX, y)
                    }
                    .zip(rightEdgeBeforeStart)
                    .toList

                val lastPaths = shortestPaths(search, trippleMap)
                val lastScore = pathScore(lastPaths)

                println(s"calculated remainder $lastScore")*/

                val scoreToAddGoingUp = if (iteration == 1) {
                    val rightEdgeBeforeStart = leftEdgeAtTheStart.map(_ + ((iteration - 1) * stepsToAdd)).map(_ + 1)

                    val trippleMap = Map2DVec.fromLines(lines.map(_.repeat(3)))

                    val search = (0 to trippleMap.maxY)
                        .map { y =>
                            Point(0, y)
                        }
                        .zip(rightEdgeBeforeStart)
                        .toList

                    val lastPaths = shortestPaths(search, trippleMap)
                    val a         = pathScore(lastPaths)

                    println(s"calculated a ${a}")
                    a
                } else {
                    7555
                }

                val scoreToAddGoingDown = if (iteration == 1) {
                    val rightEdgeBeforeStart =
                        leftEdgeAtTheStart.map(_ + ((iteration - 1) * stepsToAdd)).map(_ + 1).reverse

                    val trippleMap = Map2DVec.fromLines(lines.map(_.repeat(3)))

                    val search = (0 to trippleMap.maxY)
                        .map { y =>
                            Point(0, y)
                        }
                        .zip(rightEdgeBeforeStart)
                        .toList

                    val lastPaths = shortestPaths(search, trippleMap)
                    val b         = pathScore(lastPaths)

                    println(s"calculated b ${b}")
                    b
                } else {
                    7558
                }

                toAddForSideGoingUp += scoreToAddGoingUp
                toAddForSideGoingDown += scoreToAddGoingDown

                // println(s"score to add for left going up ${scoreToAddGoingUp}")
                // println(s"score to add for left going down ${scoreToAddGoingDown}")

                // println(s"adding additional $scoreToAdd $oddModifier")
                // throw new RuntimeException()

                score
            } else {
                val scoreToAdd = if ((iteration + oddModifier) % 2 == 0) evenIterationAddition else oddIterationAddition

                /*if (oddModifier > 2) {
                    val manualScore = {
                        val topEdgeDistanceAfterIterations =
                            leftEdgeAtTheStart.map(_ + ((iteration - 1) * stepsToAdd)).map(_ + 1)

                        val search = (0 to initialMap.maxY)
                            .map { y =>
                                Point(0, y)
                            }
                            .zip(topEdgeDistanceAfterIterations)
                            .toList

                        val lastPaths = shortestPaths(search, initialMap)
                        pathScore(lastPaths)

                    }

                    if (scoreToAdd != manualScore) {
                        println(
                          s"iteration $iteration oddModifier $oddModifier thisTileScore $scoreToAdd manualScore $manualScore"
                        )
                        throw new RuntimeException()
                    } else {
                        println("identical score")
                    }
                }*/

                loopRightFromTop(
                  iteration + 1,
                  totalStepsTaken + stepsToAdd,
                  score + scoreToAdd,
                  leftEdgeAtTheStart,
                  oddModifier
                )
            }

        @tailrec
        def loopLeft(iteration: Long, totalStepsTaken: Long, score: Long): Long =
            if (totalStepsTaken + stepsToAdd > maxSteps) {
                val topEdgeDistanceAfterIterations = topEdgeDistances.map(_ + ((iteration - 1) * stepsToAdd))
                val topEdgePlusOne                 = topEdgeDistanceAfterIterations.map(_ + 1)

                val trippleMap = Map2DVec.fromLines(lines.map(_.repeat(3)))

                val search = (0 to trippleMap.maxY)
                    .map { y =>
                        Point(trippleMap.maxX, y)
                    }
                    .zip(topEdgePlusOne)
                    .toList

                val lastPaths = shortestPaths(search, trippleMap)
                val lastScore = pathScore(lastPaths)

                score + lastScore
            } else {
                val scoreToAdd = if (iteration % 2 == 0) evenIterationAddition else oddIterationAddition
                loopLeft(iteration + 1, totalStepsTaken + stepsToAdd, score + scoreToAdd)
            }

        @tailrec
        def loopRight(iteration: Long, totalStepsTaken: Long, score: Long): Long =
            if (totalStepsTaken + stepsToAdd > maxSteps) {
                val topEdgeDistanceAfterIterations = topEdgeDistances.map(_ + ((iteration - 1) * stepsToAdd))
                val topEdgePlusOne                 = topEdgeDistanceAfterIterations.map(_ + 1)

                val trippleMap = Map2DVec.fromLines(lines.map(_.repeat(3)))

                val search = (0 to trippleMap.maxY)
                    .map { y =>
                        Point(0, y)
                    }
                    .zip(topEdgePlusOne)
                    .toList

                val lastPaths = shortestPaths(search, trippleMap)
                val lastScore = pathScore(lastPaths)

                score + lastScore
            } else {
                val scoreToAdd = if (iteration % 2 == 0) evenIterationAddition else oddIterationAddition
                loopRight(iteration + 1, totalStepsTaken + stepsToAdd, score + scoreToAdd)
            }

        @tailrec
        def loopUp(iteration: Long, totalStepsTaken: Long, score: Long): Long =
            if (totalStepsTaken + stepsToAdd > maxSteps) {
                val topEdgeDistanceAfterIterations = topEdgeDistances.map(_ + ((iteration - 1) * stepsToAdd))
                val topEdgePlusOne                 = topEdgeDistanceAfterIterations.map(_ + 1)

                // println(s"!!!!!!!!! $topEdgePlusOne")

                val trippleMap = Map2DVec.fromLines {
                    val increasedWidth = lines.map(l => l.repeat(3))
                    (1 to 3).flatMap(_ => increasedWidth).toList
                }

                // val trippleMap = Map2DVec.fromLines(lines.map(_.repeat(3)))

                val topScore = {
                    val search = (0 to initialMap.maxX)
                        .map { x =>
                            Point(x + initialMap.maxX + 1, trippleMap.maxY)
                        }
                        .zip(topEdgePlusOne)
                        .toList

                    val lastPaths = shortestPaths(search, trippleMap)
                    pathScore(lastPaths)
                }

                val bottomScore = {
                    val search = (0 to initialMap.maxX)
                        .map { x =>
                            Point(x + initialMap.maxX + 1, 0)
                        }
                        .zip(topEdgePlusOne)
                        .toList

                    val lastPaths = shortestPaths(search, trippleMap)
                    pathScore(lastPaths)
                }
                toAddForTop = topScore
                toAddForBottom = bottomScore

                score
            } else {
                val thisTileScore = if (iteration % 2 == 0) evenIterationAddition else oddIterationAddition

                // if (iteration == 2) {
                /*val manualScore = {
                    val topEdgeDistanceAfterIterations = topEdgeDistances.map(_ + ((iteration - 1) * stepsToAdd))
                    val topEdgePlusOne                 = topEdgeDistanceAfterIterations.map(_ + 1)

                    val search = (0 to initialMap.maxX)
                        .map { x =>
                            Point(x, initialMap.maxY)
                        }
                        .zip(topEdgePlusOne)
                        .toList

                    val lastPaths = shortestPaths(search, initialMap)
                    pathScore(lastPaths)

                }

                if (thisTileScore != manualScore) {
                    println(s"iteration $iteration thisTileScore $thisTileScore manualScore $manualScore")
                    throw new RuntimeException()
                } else {
                    println("identical score")
                }*/
                // }

                val newStepsTaken = totalStepsTaken + stepsToAdd

                val leftEdgeAtTheStart = leftEdgeDistances.map(_ + ((iteration - 1) * stepsToAdd))
                // val stepsLeft          = loopLeftFromTop(1, newStepsTaken, 0, leftEdgeAtTheStart, iteration - 1)
                val stepsLeft  = loopLeftFromTop(1, newStepsTaken, 0, leftEdgeAtTheStart, iteration)
                val stepsRight = loopRightFromTop(1, newStepsTaken, 0, leftEdgeAtTheStart, iteration)

                val scoreToAdd = thisTileScore + stepsLeft + stepsRight
                loopUp(iteration + 1, newStepsTaken, score + scoreToAdd)
            }

        println(s"${Instant.now()} doing loop up")
        val loopUpScore = loopUp(1, stepsToStartWith, 0)
        println(s"${Instant.now()} finished doing loop up")

        val a = (loopUpScore * 2) + pathScore(centerPaths) + loopLeft(1, stepsToStartWith, 0) + loopRight(
          1,
          stepsToStartWith,
          0
        ) + toAddForTop + toAddForBottom + toAddForSideGoingUp + toAddForSideGoingDown

        println(s"toAddForTop $toAddForTop")
        println(s"toAddForBottom $toAddForBottom")
        println(s"toAddForSideGoingUp $toAddForSideGoingUp")
        println(s"toAddForSideGoingDown $toAddForSideGoingDown")

        a

        /*def loop(a: Long, s: Long, rotation: Long): Long =
            val newS = s + maxStepsGap
            if (newS > maxSteps)
                val remainingSteps = maxSteps - s

                val expandedMap = Map2DVec.fromLines {
                    val repeats        = 5
                    val increasedWidth = lines.map(l => l.repeat(repeats))
                    (1 to repeats).flatMap(_ => increasedWidth).toList
                }
                val startingPoint = Point(expandedMap.maxX / 2, expandedMap.maxY / 2)
                val pq            = mutable.PriorityQueue((startingPoint, 0L))(Ordering.by(_._2 * -1))

                val remainingRes = shortestPaths(pq, expandedMap, Map.empty).count { case (_, s) =>
                    val remainingSteps2 = remainingSteps - s
                    remainingSteps2 % 2 == remainingSteps % 2
                }

                println(
                  s"returning $a steps taken so far $s remaining steps $remainingSteps remaining res $remainingRes; x ${initialMap.maxX} y ${initialMap.maxY} rotations $rotation"
                )
                a
            else
                // println(s"currently $a, adding ${ansGap * rotation}")
                loop(a + ansGap * rotation, newS, rotation + 1)

        val exp = loop(results(0)._2, results(0)._3, 1)

        println(s"ansGap $ansGap maxStepsGap $maxStepsGap")*/
        /* -------------------------------------- */

        /*val requiredAnswers = List(6536 - 1986, 6536 - 3277, 6536 - 1014, 6536 - 4529, 6536 - 5457)
        requiredAnswers.foreach { a =>
            println(s"required answer ${a}")
        }

        (1L to globalMaxSteps).toList
            .map { steps =>
                globalMaxSteps = steps
                cache.clear()
                cache2.clear()
                (steps, mapSearches(Queue(Point(0, 0)), Map.empty, 0))
            }
            .foreach { case (step, ans) =>
                println(s"steps $step and $ans ${requiredAnswers.contains(ans)}")
            }

        requiredAnswers.map { req => }*/
        // exp
    /*val squareToTheLeftPaths = {
            val searches = (0 to initialMap.maxY).map { y =>
                val p             = Point(initialMap.maxX, y)
                val opposingPoint = Point(0, y)
                (p, centerPaths(opposingPoint) + 1)
            }

            val pq = mutable.PriorityQueue(searches: _*)(Ordering.by(_._2 * -1))
            shortestPaths(pq, initialMap, Map.empty, maxSteps)
        }

        val squareToTheRightPaths = {
            val searches = (0 to initialMap.maxY).map { y =>
                val p             = Point(0, y)
                val opposingPoint = Point(initialMap.maxX, y)
                (p, centerPaths(opposingPoint) + 1)
            }

            val pq = mutable.PriorityQueue(searches: _*)(Ordering.by(_._2 * -1))
            shortestPaths(pq, initialMap, Map.empty, maxSteps)
        }

        val squareToUpPaths = {
            val searches = (0 to initialMap.maxX).map { x =>
                val p             = Point(x, initialMap.maxY)
                val opposingPoint = Point(x, 0)
                (p, centerPaths(opposingPoint) + 1)
            }

            val pq = mutable.PriorityQueue(searches: _*)(Ordering.by(_._2 * -1))
            shortestPaths(pq, initialMap, Map.empty, maxSteps)
        }

        val squareToDownPaths = {
            val searches = (0 to initialMap.maxX).map { x =>
                val p             = Point(x, 0)
                val opposingPoint = Point(x, initialMap.maxY)
                (p, centerPaths(opposingPoint) + 1)
            }

            val pq = mutable.PriorityQueue(searches: _*)(Ordering.by(_._2 * -1))
            shortestPaths(pq, initialMap, Map.empty, maxSteps)
        }

        println(s"centerScore ${pathScore(centerPaths)}")
        println(s"squareToTheLeftScore ${pathScore(squareToTheLeftPaths)}")
        println(s"squareToTheRightScore ${pathScore(squareToTheRightPaths)}")
        println(s"squareUpScore ${pathScore(squareToUpPaths)}")
        println(s"squareDownScore ${pathScore(squareToDownPaths)}")

        val squareToTheLeftPaths2 = {
            val searches = (0 to initialMap.maxY).map { y =>
                val p             = Point(initialMap.maxX, y)
                val opposingPoint = Point(0, y)
                (p, squareToTheLeftPaths(opposingPoint) + 1)
            }

            val pq = mutable.PriorityQueue(searches: _*)(Ordering.by(_._2 * -1))
            shortestPaths(pq, initialMap, Map.empty, maxSteps)
        }
        println(s"squareToTheLeftScore2 ${pathScore(squareToTheLeftPaths2)}")*/

    /*val initialMap = Map2DVec.fromLines(lines)

        val results = List(1, 3, 5, 7, 9, 11, 13, 15, 17, 19 /*, 21, 23, 25, 27, 29, 31, 33, 36, 39, 41 */ ).map {
            repeats =>
                val expandedMap = Map2DVec.fromLines {
                    val increasedWidth = lines.map(l => l.repeat(repeats))
                    (1 to repeats).flatMap(_ => increasedWidth).toList
                }
                val startingPoint = Point(expandedMap.maxX / 2, expandedMap.maxY / 2)

                // println("MAP CONSTRUCTION COMPLETE")

                val pq    = mutable.PriorityQueue((startingPoint, 0L))(Ordering.by(_._2 * -1))
                val paths = shortestPaths(pq, expandedMap, Map.empty, maxSteps)
                val ans = paths.count { case (_, s) =>
                    val remainingSteps = maxSteps - s
                    remainingSteps % 2 == maxSteps % 2
                }

                val maxStepsInSearch = paths.maxBy(_._2)._2

                (repeats, ans, maxStepsInSearch, paths)

                // println(s"repeats - $repeats ans - $ans")
        }

        val ansGap: Long      = results(1)._2 - results(0)._2
        val maxStepsGap: Long = results(1)._3 - results(0)._3

        val cornerPoints = List(
          Point(0, 0),
          Point(initialMap.maxX, 0),
          Point(0, initialMap.maxY),
          Point(initialMap.maxX, initialMap.maxY)
        )

        val aaa = cornerPoints.map { p =>
            val s              = results(0)._4(p)
            val remainingSteps = maxSteps - s
            if (remainingSteps % 2 == maxSteps % 2) {
                println(s"CORNER GOOD ${s} ${maxSteps - s} ${(maxSteps - s) / 2}")
            }

            maxSteps - s
        }.sum
        println(s"corner sum $aaa")

        results.zipWithIndex.foreach { case ((repeats, ans, maxSteps, _), i) =>
            val fromPreviousAnd   = if (i == 0) 0 else ans - results(i - 1)._2
            val fromPreviousSteps = if (i == 0) 0 else maxSteps - results(i - 1)._3
            println(s"repeats - $repeats ans - ($ans $fromPreviousAnd) ($maxSteps $fromPreviousSteps)")
        }

        def loop(a: Long, s: Long, rotation: Long): Long =
            val newS = s + maxStepsGap
            if (newS > maxSteps)
                val remainingSteps = maxSteps - s

                val expandedMap = Map2DVec.fromLines {
                    val repeats        = 5
                    val increasedWidth = lines.map(l => l.repeat(repeats))
                    (1 to repeats).flatMap(_ => increasedWidth).toList
                }
                val startingPoint = Point(expandedMap.maxX / 2, expandedMap.maxY / 2)
                val pq            = mutable.PriorityQueue((startingPoint, 0L))(Ordering.by(_._2 * -1))

                val remainingRes = shortestPaths(pq, expandedMap, Map.empty, remainingSteps).count { case (_, s) =>
                    val remainingSteps2 = remainingSteps - s
                    remainingSteps2 % 2 == remainingSteps % 2
                }

                println(
                  s"returning $a steps taken so far $s remaining steps $remainingSteps remaining res $remainingRes; x ${initialMap.maxX} y ${initialMap.maxY} rotations $rotation"
                )
                a
            else
                // println(s"currently $a, adding ${ansGap * rotation}")
                loop(a + ansGap * rotation, newS, rotation + 1)

        return loop(results(0)._2, results(0)._3, 1)

        println(s"ansGap $ansGap maxStepsGap $maxStepsGap")

        val expandedMap = Map2DVec.fromLines {
            val repeats        = 3
            val increasedWidth = lines.map(l => l.repeat(repeats))
            (1 to repeats).flatMap(_ => increasedWidth).toList
        }
        val startingPoint = Point(expandedMap.maxX / 2, expandedMap.maxY / 2)

        // println("MAP CONSTRUCTION COMPLETE")

        val pq = mutable.PriorityQueue((startingPoint, 0L))(Ordering.by(_._2 * -1))
        shortestPaths(pq, expandedMap, Map.empty, maxSteps).count { case (_, s) =>
            val remainingSteps = maxSteps - s
            remainingSteps % 2 == maxSteps % 2
        }*/

// println(initialMap)
// println()
// println(trippledMap)

    /*val startingPoint = (for {
            x <- 0 until initialMap.maxX
            y <- 0 until initialMap.maxY
        } yield Point(x, y)).find(p => initialMap(p) == 'S').get*/

    /*val allShortestPaths = (for {
            x <- 0 until initialMap.maxX
            y <- 0 until initialMap.maxY
        } yield Point(x, y)).par.map { p =>
            p -> shortestPaths(Queue((p, 0)), initialMap, Map.empty)
        }.toMap*/

// val startingPoint = Point(expandedMap.maxX / 2, expandedMap.maxY / 2)

    /*shortestPaths(Queue((startingPoint, 0)), expandedMap, Map.empty, 2).count { case (_, s) =>
            s <= maxSteps
        }*/

    val cache =
        collection.mutable.Map.empty[
          String,
          (Map[Point, (Point, Long)], Long, Long)
        ] // cache key -> map deltas, score, highest stepCount delta
    val cache2      = collection.mutable.Map.empty[String, Long]
    val pointsCache = collection.mutable.Map.empty[(String, Long), Long]
    def shortestPathsCached(
        from: List[(Point, Long)],
        map: Map2DVec[Char]
    ): (Map[Point, Long], Long) =
        val cacheKey =
            if (from.sizeIs == 1) {
                "none"
            } else {
                val minSteps = from.minBy(_._2)._2
                from.map(a => (a._1, a._2 - minSteps)).toString()
            }

        @tailrec
        def searchLoop(
            search: mutable.PriorityQueue[(Point, Point, Long)],
            map: Map2DVec[Char],
            results: Map[Point, (Point, Long)]
        ): Map[Point, (Point, Long)] =
            if (search.isEmpty) results
            else
                val (headPoint, origin, headSteps) = search.dequeue()
                // println(s"running search from $headPoint")
                if (results.contains(headPoint)) searchLoop(search, map, results)
                else
                    val newResults = results + (headPoint -> (origin, headSteps))
                    val newSearches = headPoint.adjacent
                        .filter(_.inBounds(map))
                        .filter(p => map(p) != '#' && map(p) != ' ')
                        .map(p => (p, origin, headSteps + 1))
                    search.enqueue(newSearches: _*)
                    searchLoop(search, map, newResults)

        if (from.sizeIs == 1) {
            val pq     = mutable.PriorityQueue(from.map(a => (a._1, a._1, a._2)): _*)(Ordering.by(_._3 * -1))
            val search = searchLoop(pq, map, Map.empty)
            val score  = pathScore2(search)
            (search.view.mapValues(_._2).toMap, score)
        } else {

            cache.get(cacheKey) match
                case None =>
                    val pq     = mutable.PriorityQueue(from.map(a => (a._1, a._1, a._2)): _*)(Ordering.by(_._3 * -1))
                    val result = searchLoop(pq, map, Map.empty)

                    val deltas = result.map { case (point, (origin, steps)) =>
                        val delta = steps - result(origin)._2
                        (point, (origin, delta))
                    }
                    val highestDelta = deltas.maxBy(_._2._2)._2._2

                    val score = pathScore2(result)

                    val onlyEdgeDeltas = deltas.filter { (k, v) =>
                        k.x == 0 || k.y == 0 || k.x == map.maxX || k.y == map.maxY
                    }
                    if (cacheKey != "none") {
                        cache.put(cacheKey, (onlyEdgeDeltas, score, highestDelta))
                        // println(s"ADDED TO CACHE ${from}")
                        // cache2.put(cacheKey, pathScore2(result))
                    }

                    val onlyEdges = result
                        .filter { (k, v) =>
                            k.x == 0 || k.y == 0 || k.x == map.maxX || k.y == map.maxY
                        }
                        .view
                        .mapValues(_._2)
                        .toMap

                    (onlyEdges, score)
                case Some((cachedDeltas, cachedScore, cachedHighestDelta)) =>
                    val start = System.currentTimeMillis()

                    if (from.map(_._2 + cachedHighestDelta).max >= globalMaxSteps) {
                        // println(s"CANNOT USE CACHE ANYMORE")
                        (Map.empty, 0L)
                    } else {
                        val fromMap = from.toMap
                        val deltas = cachedDeltas.map { case (point, (origin, delta)) =>
                            (point, delta + fromMap(origin))
                        }

                        val end = System.currentTimeMillis()
                        spentUsingCache = spentUsingCache + (end - start)

                        (deltas, cachedScore)
                    }

        }

    def shortestPaths(
        from: List[(Point, Long)],
        map: Map2DVec[Char]
    ): Map[Point, Long] =

        // println("entered shortest paths")
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

        loop(mutable.PriorityQueue(from: _*)(Ordering.by(_._2 * -1)), map, Map.empty)

//    @tailrec
//    def shortestPaths(
//        search: mutable.PriorityQueue[(Point, Long)],
//        map: Map2DVec[Char],
//        results: Map[Point, Long],
//        maxSteps: Long
//    ): Map[Point, Long] =
//        if (search.isEmpty) results
//        else
//            val (headPoint, headSteps) = search.dequeue()
//            // println(s"running search from $headPoint")
//            if (results.contains(headPoint)) shortestPaths(search, map, results, maxSteps)
//            else if (headSteps > maxSteps) shortestPaths(search, map, results, maxSteps)
//            else
//                val newResults = results + (headPoint -> headSteps)
//                val newSearches = headPoint.adjacent
//                    .filter(_.inBounds(map))
//                    .filter(p => map(p) != '#')
//                    .map(p => (p, headSteps + 1))
//                search.enqueue(newSearches: _*)
//                shortestPaths(search, map, newResults, maxSteps)

/*enum EO:
        case Even, Odd

        def flip: EO = this match
            case Even => Odd
            case Odd  => Even

    @tailrec
    def shortestPaths(
        search: mutable.PriorityQueue[(Point, EO, Int)],
        map: Map2DVec[Char],
        results: Map[Point, List[(EO, Int)]],
        maxSteps: Int
    ): Map[Point, List[(EO, Int)]] =
        if (search.isEmpty) results
        else
            val (headPoint, headEO, headSteps) = search.dequeue()
            // println(s"running search from $headPoint")
            if (results.get(headPoint).exists(_.exists(_._1 == headEO))) shortestPaths(search, map, results, maxSteps)
            else if (headSteps > maxSteps) shortestPaths(search, map, results, maxSteps)
            else
                val existing   = results.getOrElse(headPoint, List.empty)
                val newResults = results + (headPoint -> ((headEO, headSteps) +: existing))
                val newSearches = headPoint.adjacent
                    .filter(_.inBounds(map))
                    .filter(p => map(p) != '#')
                    .map(p => (p, headEO.flip, headSteps + 1))
                search.enqueue(newSearches: _*)
                shortestPaths(search, map, newResults, maxSteps)*/
