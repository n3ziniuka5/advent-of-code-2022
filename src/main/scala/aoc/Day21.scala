package aoc

import aoc.Common.timed
import aoc.Day21.Job.{DependsOnUnknown, Equality, Lazy, Math, Yell}

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
      op: (Long, Long) => Long,       // 1 is a, 2 is b
      unknownA: (Long, Long) => Long, // 1 is what we want to get, 2 is b
      unknownB: (Long, Long) => Long  // 1 is what we want to get, 2 is a
    )
    case Lazy
    case DependsOnUnknown(resolve: Long => Long)

  def parse(lines: List[String]): Map[String, Job] =
    lines.map {
      case s"$monkey: $a + $b" => monkey -> Math(a, b, _ + _, _ - _, _ - _)
      case s"$monkey: $a - $b" => monkey -> Math(a, b, _ - _, _ + _, (c, a) => a - c)
      case s"$monkey: $a * $b" => monkey -> Math(a, b, _ * _, _ / _, _ / _)
      case s"$monkey: $a / $b" => monkey -> Math(a, b, _ / _, _ * _, (c, a) => a / c)
      case s"$monkey: $num"    => monkey -> Yell(num.toInt)
    }.toMap

  def resolveRoot(initialJobs: Map[String, Job], resolveNode: String): Job =
    def loop(search: List[String], jobs: Map[String, Job]): Map[String, Job] =
      search match {
        case head :: tail =>
          jobs(head) match
            case Yell(_) => loop(tail, jobs)
            case Equality(a, b) =>
              val aResolved = resolveRoot(initialJobs, a)
              val bResolved = resolveRoot(initialJobs, b)

              (aResolved, bResolved) match
                case (DependsOnUnknown(resolve), Yell(b)) =>
                  println(s"need to find A, should equal ${b}")
                  val r = resolve(b)
                  println(s"found A - $r")
                case (Yell(a), DependsOnUnknown(resolve)) =>
                  println(s"need to find B, should equal ${a}")
                  val r = resolve(a)
                  println(s"found B - ${r}")

              println(aResolved)
              println(bResolved)

              ???

            case Lazy => jobs + (resolveNode -> Lazy)

            case Math(a, b, op, unknownA, unknownB) =>
              val needToResolve = List(a, b).filter { monkey =>
                jobs(monkey) match
                  case Yell(_)             => false
                  case Lazy                => false
                  case _: DependsOnUnknown => false
                  case _                   => true
              }

              if needToResolve.isEmpty then
                val resolvedHead = (jobs(a), jobs(b)) match
                  case (Yell(aNum), Yell(bNum)) => Yell(op(aNum, bNum))
                  case (Lazy, Yell(bNum)) =>
                    DependsOnUnknown(wantToGet =>
                      val res = unknownA(wantToGet, bNum)
                      println(s"I wanted to get $wantToGet, bNum is $bNum  JUST RESOLVED LAZY TO $res")
                      res
                    )
                  case (Yell(aNum), Lazy) =>
                    DependsOnUnknown(wantToGet =>
                      val res = unknownB(wantToGet, aNum)
                      println(s"I wanted to get $wantToGet, aNum is $aNum  JUST RESOLVED LAZY TO $res")
                      res
                    )
                  case (DependsOnUnknown(resolveChild), Yell(bNum)) =>
                    // DependsOnUnknown(wantToGet => unknownA(resolveChild(wantToGet), bNum))
                    DependsOnUnknown(wantToGet => resolveChild(unknownA(wantToGet, bNum)))
                  case (Yell(aNum), DependsOnUnknown(resolveChild)) =>
                    // DependsOnUnknown(wantToGet => unknownB(resolveChild(wantToGet), aNum))
                    DependsOnUnknown(wantToGet => resolveChild(unknownB(wantToGet, aNum)))

                /*val aNum    = jobs(a) match
                  case Yell(num) => num
                  case Lazy =>
                val bNum    = jobs(b).asInstanceOf[Yell].num
                val headNum = op(aNum, bNum)*/

                loop(tail, jobs + (head -> resolvedHead))
              else loop(needToResolve ++ search, jobs)

        case Nil =>
          jobs
      }

    loop(List(resolveNode), initialJobs)(resolveNode)

  def part1(lines: List[String]): Long =
    resolveRoot(parse(lines), "root") match
      case Yell(num) => num
      case _         => Long.MinValue

  def part2(lines: List[String]): Long =
    val parsed      = parse(lines)
    val currentRoot = parsed("root").asInstanceOf[Math]

    resolveRoot(parsed + ("humn" -> Lazy) + ("root" -> Equality(currentRoot.a, currentRoot.b)), "root") match
      case Yell(num) => println(s"found plain number? $num")
      case other     => println(other)

    0
