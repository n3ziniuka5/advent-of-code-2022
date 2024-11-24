package aoc

import aoc.Common.timed

import scala.annotation.tailrec
import scala.io.Source

object Day19:
    def main(args: Array[String]): Unit =
        val lines = Source.fromResource("day19.txt").getLines().toList
        timed("Part 1", part1(lines))
        timed("Part 2", part2(lines))

    case class Part(x: Long, m: Long, a: Long, s: Long)

    sealed trait Rule
    object Rule:
        case object Accepted                         extends Rule
        case object Rejected                         extends Rule
        case class ToOtherWorkflow(workflow: String) extends Rule
        case class Comparison(opPart1: Part => Boolean, opPart2: Long => Boolean, workflow: String, selector: Char)
            extends Rule

    case class Range(from: Long, to: Long)
    case class Search(x: Range, m: Range, a: Range, s: Range, workflow: String)

    def part1(lines: List[String]): Long =
        val (parts, workflows) = parse(lines)
        parts
            .filter(runWorkflow(_, "in", workflows) == Rule.Accepted)
            .map { p =>
                p.x + p.m + p.a + p.s
            }
            .sum

    def part2(lines: List[String]): Long =
        val (_, workflows) = parse(lines)
        val ranges = allAcceptedRanges(
          List(Search(Range(1, 4000), Range(1, 4000), Range(1, 4000), Range(1, 4000), "in")),
          List.empty,
          workflows
        )

        ranges.map { r =>
            val x = r.x.to - r.x.from + 1
            val m = r.m.to - r.m.from + 1
            val a = r.a.to - r.a.from + 1
            val s = r.s.to - r.s.from + 1
            x * m * a * s
        }.sum

    def parse(lines: List[String]): (List[Part], Map[String, List[Rule]]) =
        def parseWorkflow(s: String): (String, List[Rule]) = s match {
            case s"$workflowName{$rulesString}" =>
                val rules = rulesString
                    .split(',')
                    .map { s =>
                        if (s.contains('<') || s.contains('>')) {
                            val selector: (Part => Long) =
                                if (s.startsWith("x")) _.x
                                else if (s.startsWith("m")) _.m
                                else if (s.startsWith("a")) _.a
                                else _.s
                            val valueStr = s.drop(2).takeWhile(_.isDigit)
                            val value    = valueStr.toLong
                            val workflow = s.drop(3 + valueStr.length)
                            s(1) match {
                                case '<' => Rule.Comparison(selector(_) < value, _ < value, workflow, s.head)
                                case '>' => Rule.Comparison(selector(_) > value, _ > value, workflow, s.head)
                            }
                        } else if (s == "R") {
                            Rule.Rejected
                        } else if (s == "A") {
                            Rule.Accepted
                        } else {
                            Rule.ToOtherWorkflow(s)
                        }
                    }
                    .toList

                workflowName -> rules
        }

        val workflows =
            lines.takeWhile(_.nonEmpty).map(parseWorkflow).toMap + ("A" -> List(Rule.Accepted)) + ("R" -> List(
              Rule.Rejected
            ))

        val parts = lines.drop(workflows.size + 1 - 2).map { case s"{x=$x,m=$m,a=$a,s=$s}" =>
            Part(x.toLong, m.toLong, a.toLong, s.toLong)
        }

        (parts, workflows)

    @tailrec
    def runWorkflow(
        part: Part,
        workflow: String,
        workflows: Map[String, List[Rule]]
    ): Rule.Accepted.type | Rule.Rejected.type =
        val rules = workflows(workflow)
        val matchedRule = rules.find {
            case Rule.Accepted                        => true
            case Rule.Rejected                        => true
            case Rule.ToOtherWorkflow(workflow)       => true
            case Rule.Comparison(op1, _, workflow, _) => op1(part)
        }.get
        matchedRule match {
            case Rule.Accepted                      => Rule.Accepted
            case Rule.Rejected                      => Rule.Rejected
            case Rule.ToOtherWorkflow(workflow)     => runWorkflow(part, workflow, workflows)
            case Rule.Comparison(_, _, workflow, _) => runWorkflow(part, workflow, workflows)
        }

    @tailrec
    def allAcceptedRanges(
        searches: List[Search],
        answers: List[Search],
        workflows: Map[String, List[Rule]]
    ): List[Search] =
        if (searches.isEmpty) {
            answers
        } else {
            val head  = searches.head
            val rules = workflows(head.workflow)

            def selector(selectorChar: Char): (Search => Range) =
                if (selectorChar == 'x') _.x
                else if (selectorChar == 'm') _.m
                else if (selectorChar == 'a') _.a
                else _.s

            val matchedRule = rules.find {
                case Rule.Accepted                  => true
                case Rule.Rejected                  => true
                case Rule.ToOtherWorkflow(workflow) => true
                case Rule.Comparison(_, op, workflow, selectorChar) =>
                    (selector(selectorChar)(head).from to selector(selectorChar)(head).to).exists(op(_))

            }.get

            matchedRule match
                case Rule.Accepted => allAcceptedRanges(searches.tail, head +: answers, workflows)
                case Rule.Rejected => allAcceptedRanges(searches.tail, answers, workflows)
                case Rule.ToOtherWorkflow(workflow) =>
                    val newSearch = head.copy(workflow = workflow)
                    allAcceptedRanges(newSearch +: searches.tail, answers, workflows)
                case Rule.Comparison(_, op, workflow, selectorChar) =>
                    val allValues = (selector(selectorChar)(head).from to selector(selectorChar)(head).to).map(op(_))
                    val splitAt   = (1 until allValues.size).find(i => allValues(i - 1) != allValues(i))

                    splitAt match
                        case None =>
                            val newSearch = head.copy(workflow = workflow)
                            allAcceptedRanges(newSearch +: searches.tail, answers, workflows)
                        case Some(i) =>
                            def newRange(range: Range): List[Range] =
                                List(
                                  Range(range.from, range.from + i - 1),
                                  Range(range.from + i, range.to),
                                )

                            val newSearches = if (selectorChar == 'x') newRange(head.x).map { r => head.copy(x = r) }
                            else if (selectorChar == 'm') newRange(head.m).map { r => head.copy(m = r) }
                            else if (selectorChar == 'a') newRange(head.a).map { r => head.copy(a = r) }
                            else newRange(head.s).map { r => head.copy(s = r) }

                            allAcceptedRanges(newSearches ++ searches.tail, answers, workflows)
        }
