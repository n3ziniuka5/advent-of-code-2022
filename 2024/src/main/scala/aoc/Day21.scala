package aoc

import aoc.Common.timed
import scala.collection.mutable.PriorityQueue
import language.experimental.namedTuples

object Day21:
    def main(args: Array[String]): Unit =
        val lines = InputUtils.fetchInput(2024, 21)
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    def part1(codes: List[String]): Long =
        codes.map(computeAnswer(_, 2)).sum

    def part2(codes: List[String]): Long =
        codes.map(computeAnswer(_, 25)).sum

    def computeAnswer(code: String, chainCount: Long): Long =
        val length       = shortestSequenceLength(code, chainCount)
        val numericValue = code.dropRight(1).toLong
        numericValue * length

    def shortestSequenceLength(code: String, chainCount: Long): Long =
        def loop(toPress: List[Char], currentPos: Char, pressed: Vector[Char], totalPresses: Long): Long =
            toPress match
                case head :: tail =>
                    val buttonPresses = pathOnMainKeyPad(currentPos, head, chainCount + 1)
                    loop(tail, head, pressed, totalPresses + buttonPresses)
                case Nil =>
                    totalPresses
        loop(code.toList, 'A', Vector.empty, 0L)

    def pathOnMainKeyPad(from: Char, to: Char, chainCount: Long): Long =
        def position(c: Char): Point = c match
            case '7' => Point(0, 0)
            case '8' => Point(1, 0)
            case '9' => Point(2, 0)
            case '4' => Point(0, 1)
            case '5' => Point(1, 1)
            case '6' => Point(2, 1)
            case '1' => Point(0, 2)
            case '2' => Point(1, 2)
            case '3' => Point(2, 2)
            case '0' => Point(1, 3)
            case 'A' => Point(2, 3)

        val solutionFound = Point(9, 9)
        def loop(searches: PriorityQueue[(Point, Option[Char], Long)]): Long =
            val (current, lastDirection, cost) = searches.dequeue()
            if current == solutionFound then cost
            else if current == position(to) then
                searches.enqueue((solutionFound, None, cost + costFun(lastDirection, 'A', chainCount)))
                loop(searches)
            else
                val neighbors = List(('<', current.left), ('>', current.right), ('^', current.up), ('v', current.down))
                val addedSearches = neighbors
                    .filter: (_, p) =>
                        p.x >= 0 && p.x <= 2 && p.y >= 0 && p.y <= 3 && p != Point(0, 3)
                    .map: (dir, p) =>
                        val addedCost = costFun(lastDirection, dir, chainCount)
                        (p, Some(dir), cost + addedCost)
                searches.enqueue(addedSearches*)
                loop(searches)

        loop(PriorityQueue((position(from), None, 0L))(Ordering.by(-_._3)))

    type CostCacheKey = (prevButton: Option[Char], toPress: Char, chain: Long)
    val costCache = collection.mutable.Map[CostCacheKey, Long]()
    def costFun(prevButton: Option[Char], toPress: Char, chain: Long): Long =
        lazy val result =
            if chain == 1 then 1
            else if prevButton.contains(toPress) then 1
            else if prevButton.isEmpty then
                toPress match
                    case 'A' => 1
                    case '<' =>
                        costFun(None, 'v', chain - 1) + costFun(Some('v'), '<', chain - 1) + costFun(
                          Some('<'),
                          '<',
                          chain - 1
                        ) + costFun(
                          Some('<'),
                          'A',
                          chain - 1
                        )
                    case '>' => costFun(None, 'v', chain - 1) + costFun(Some('v'), 'A', chain - 1)
                    case '^' => costFun(None, '<', chain - 1) + costFun(Some('<'), 'A', chain - 1)
                    case 'v' =>
                        val optionA = costFun(None, '<', chain - 1) + costFun(Some('<'), 'v', chain - 1) + costFun(
                          Some('v'),
                          'A',
                          chain - 1
                        )
                        val optionB = costFun(None, 'v', chain - 1) + costFun(Some('v'), '<', chain - 1) + costFun(
                          Some('<'),
                          'A',
                          chain - 1
                        )
                        math.min(optionA, optionB)
            else
                (prevButton, toPress) match
                    case (Some('<'), 'A') =>
                        costFun(None, '>', chain - 1) + costFun(Some('>'), '>', chain - 1) + costFun(
                          Some('>'),
                          '^',
                          chain - 1
                        ) + costFun(Some('^'), 'A', chain - 1)
                    case (Some('<'), '>') =>
                        costFun(None, '>', chain - 1) + costFun(Some('>'), '>', chain - 1) + costFun(
                          Some('>'),
                          'A',
                          chain - 1
                        )
                    case (Some('<'), '^') =>
                        costFun(None, '>', chain - 1) + costFun(Some('>'), '^', chain - 1) + costFun(
                          Some('^'),
                          'A',
                          chain - 1
                        )
                    case (Some('<'), 'v') => costFun(None, '>', chain - 1) + costFun(Some('>'), 'A', chain - 1)
                    case (Some('>'), 'A') => costFun(None, '^', chain - 1) + costFun(Some('^'), 'A', chain - 1)
                    case (Some('>'), '<') =>
                        costFun(None, '<', chain - 1) + costFun(Some('<'), '<', chain - 1) + costFun(
                          Some('<'),
                          'A',
                          chain - 1
                        )
                    case (Some('>'), '^') =>
                        val optionA = costFun(None, '<', chain - 1) + costFun(Some('<'), '^', chain - 1) + costFun(
                          Some('^'),
                          'A',
                          chain - 1
                        )
                        val optionB = costFun(None, '^', chain - 1) + costFun(Some('^'), '<', chain - 1) + costFun(
                          Some('<'),
                          'A',
                          chain - 1
                        )
                        math.min(optionA, optionB)
                    case (Some('>'), 'v') => costFun(None, '<', chain - 1) + costFun(Some('<'), 'A', chain - 1)
                    case (Some('^'), 'A') => costFun(None, '>', chain - 1) + costFun(Some('>'), 'A', chain - 1)
                    case (Some('^'), 'v') =>
                        costFun(None, 'v', chain - 1) + costFun(Some('v'), 'A', chain - 1)
                    case (Some('^'), '<') =>
                        costFun(None, 'v', chain - 1) + costFun(Some('v'), '<', chain - 1) + costFun(
                          Some('<'),
                          'A',
                          chain - 1
                        )
                    case (Some('^'), '>') =>
                        val optionA = costFun(None, 'v', chain - 1) + costFun(Some('v'), '>', chain - 1) + costFun(
                          Some('>'),
                          'A',
                          chain - 1
                        )
                        val optionB = costFun(None, '>', chain - 1) + costFun(Some('>'), 'v', chain - 1) + costFun(
                          Some('v'),
                          'A',
                          chain - 1
                        )
                        math.min(optionA, optionB)
                    case (Some('v'), 'A') =>
                        val optionA = costFun(None, '>', chain - 1) + costFun(Some('>'), '^', chain - 1) + costFun(
                          Some('^'),
                          'A',
                          chain - 1
                        )
                        val optionB = costFun(None, '^', chain - 1) + costFun(Some('^'), '>', chain - 1) + costFun(
                          Some('>'),
                          'A',
                          chain - 1
                        )
                        math.min(optionA, optionB)
                    case (Some('v'), '^') => costFun(None, '^', chain - 1) + costFun(Some('^'), 'A', chain - 1)
                    case (Some('v'), '<') => costFun(None, '<', chain - 1) + costFun(Some('<'), 'A', chain - 1)
                    case (Some('v'), '>') => costFun(None, '>', chain - 1) + costFun(Some('>'), 'A', chain - 1)
                    case _                => ??? // silence exhaustive match warning

        val cacheKey = (prevButton, toPress, chain)
        costCache.getOrElseUpdate(cacheKey, result)
