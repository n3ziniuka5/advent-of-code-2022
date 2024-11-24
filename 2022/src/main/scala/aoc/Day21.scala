package aoc

import aoc.Common.timed
import aoc.Day21.Job.{Equality, Math, ResolveEquation, Yell}

import scala.io.Source

object Day21:
  def main(args: Array[String]): Unit =
    val lines = Source.fromResource("day21.txt").getLines().toList
    timed("Part 1", part1(lines))
    timed("Part 2", part2(lines))

  enum Job:
    case Yell(num: Long)
    case Equality(a: String, b: String)
    case Math(
      a: String,
      b: String,
      operation: (Long, Long) => Long,                    // 1 is a, 2 is b
      reverseGetAFromDesiredResult: (Long, Long) => Long, // 1 is desired result, 2 is b
      reverseGetBFromDesiredResult: (Long, Long) => Long  // 1 is desired result, 2 is a
    )
    case ResolveEquation(
      resolve: Long => Long
    ) // you pass in what you want the equation to return and receive back the unknown variable

  def parse(lines: List[String]): Map[String, Job] =
    lines.map {
      case s"$monkey: $a + $b" => monkey -> Math(a, b, _ + _, _ - _, _ - _)
      case s"$monkey: $a - $b" => monkey -> Math(a, b, _ - _, _ + _, (c, a) => a - c)
      case s"$monkey: $a * $b" => monkey -> Math(a, b, _ * _, _ / _, _ / _)
      case s"$monkey: $a / $b" => monkey -> Math(a, b, _ / _, _ * _, (c, a) => a / c)
      case s"$monkey: $num"    => monkey -> Yell(num.toInt)
    }.toMap

  def resolveRoot(initialJobs: Map[String, Job]): Job =
    def loop(search: List[String], jobs: Map[String, Job]): Map[String, Job] =
      search match {
        case head :: tail =>
          jobs(head) match
            case Equality(a, b) =>
              (jobs(a), jobs(b)) match
                case (ResolveEquation(resolve), Yell(b)) =>
                  loop(tail, jobs + (head -> Yell(resolve(b))))
                case (Yell(a), ResolveEquation(resolve)) =>
                  loop(tail, jobs + (head -> Yell(resolve(a))))
                case _ =>
                  loop(List(a, b) ++ search, jobs)

            case Math(a, b, op, reverseGetA, reverseGetB) =>
              (jobs(a), jobs(b)) match
                case (Yell(aNum), Yell(bNum)) =>
                  val updatedJob = Yell(op(aNum, bNum))
                  loop(tail, jobs + (head -> updatedJob))
                case (ResolveEquation(resolveChild), Yell(bNum)) =>
                  val updatedJob = ResolveEquation(wantToGet => resolveChild(reverseGetA(wantToGet, bNum)))
                  loop(tail, jobs + (head -> updatedJob))
                case (Yell(aNum), ResolveEquation(resolveChild)) =>
                  val updatedJob = ResolveEquation(wantToGet => resolveChild(reverseGetB(wantToGet, aNum)))
                  loop(tail, jobs + (head -> updatedJob))
                case _ =>
                  loop(List(a, b) ++ search, jobs)

            case _ => loop(tail, jobs)

        case Nil =>
          jobs
      }

    val resolveNode = "root"
    loop(List(resolveNode), initialJobs)(resolveNode)

  def part1(lines: List[String]): Option[Long] =
    resolveRoot(parse(lines)) match
      case Yell(num) => Some(num)
      case _         => None

  def part2(lines: List[String]): Option[Long] =
    val parsed = parse(lines)
    parsed("root") match
      case Math(a, b, _, _, _) =>
        resolveRoot(parsed + ("humn" -> ResolveEquation(identity)) + ("root" -> Equality(a, b))) match
          case Yell(num) => Some(num)
          case _         => None
      case _ => None
