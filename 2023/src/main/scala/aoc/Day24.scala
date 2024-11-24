package aoc

import aoc.Common.timed

import java.nio.file.{Files, Paths}
import scala.io.Source

object Day24:
    def main(args: Array[String]): Unit =
        val lines = Source.fromResource("day24.txt").getLines().toList
        timed("Part 1", part1(lines, 200000000000000L, 400000000000000L))
        timed("Part 2", part2(lines))

    case class Hailstone(x: Long, y: Long, z: Long, velocityX: Long, velocityY: Long, velocityZ: Long):
        def afterIterations(i: Long): Hailstone =
            copy(
              x = x + (velocityX * i),
              y = y + (velocityY * i),
              z = z + (velocityZ * i)
            )

    def part1(lines: List[String], minCoord: Long, maxCoord: Long): Int =
        val hailstones = parse(lines)

        hailstones.combinations(2).count { pair =>
            val a = pair(0)
            val b = pair(1)

            val bi = findBI(a, b)
            bi match
                case Some(value) =>
                    val ai = findAIfromBI(a, b, value)

                    val points = List(
                      a.x + (a.velocityX * ai),
                      a.y + (a.velocityY * ai),
                      b.x + (b.velocityX * value),
                      b.y + (b.velocityY * value)
                    )

                    value >= 0 && ai >= 0 && points.forall(p => p >= minCoord && p <= maxCoord)
                case None => false
        }

    def part2(lines: List[String]): Long =
        val hailstones = parse(lines).take(
          10
        ) // 10 is enough to get the answer, equation solve time grows exponentially with more equations

        val equations = hailstones.zipWithIndex.flatMap { (h, i) =>
            val iString = if i == 0 then "i" else s"(i + h$i)"
            List(
              s"eq${i}_x = Eq(x + (DX * $iString), ${h.x} + (${h.velocityX} * $iString))",
              s"eq${i}_y = Eq(y + (DY * $iString), ${h.y} + (${h.velocityY} * $iString))",
              s"eq${i}_z = Eq(z + (DZ * $iString), ${h.z} + (${h.velocityZ} * $iString))",
            )
        }

        val hVariables = hailstones.zipWithIndex.tail
            .map(e => s"h${e._2}")
            .mkString(", ")
        val symbols = s"x, y, z, DX, DY, DZ, i, $hVariables"
        val equationIds = hailstones.zipWithIndex
            .flatMap { (h, i) =>
                List(
                  s"eq${i}_x",
                  s"eq${i}_y",
                  s"eq${i}_z",
                )
            }
            .mkString(",")

        val pythonScript =
            s"""
               |from sympy import symbols, Eq, solve
               |
               |x, y, z, DX, DY, DZ, i, $hVariables = symbols('${symbols.replace(", ", " ")}')
               |
               |${equations.mkString("\n")}
               |
               |solution = solve(($equationIds))
               |
               |print(solution)
               |print(f'asnwer: {solution[0][0] + solution[0][1] + solution[0][2]}')
               |
               |""".stripMargin

        Files.write(Paths.get("day24.py"), pythonScript.getBytes())

        println("!!! Run day24.py to get the part 2 answer")

        0

    def parse(lines: List[String]): List[Hailstone] =
        lines.map { case s"$x, $y, $z @ $vx, $vy, $vz" =>
            Hailstone(x.toLong, y.toLong, z.toLong, vx.toInt, vy.toInt, vz.toInt)
        }

    // aDY * bX + bDX * aDY * bI - adY * aX = aDX * bY + aDX * bDY * bI - aDX * aY
    def findBI(a: Hailstone, b: Hailstone): Option[Double] =
        val top    = a.velocityX * a.y - a.velocityX * b.y - a.velocityY * a.x + a.velocityY * b.x
        val bottom = a.velocityX * b.velocityY - a.velocityY * b.velocityX

        if bottom == 0 then None
        else Some(top.toDouble / bottom)

    def findAIfromBI(a: Hailstone, b: Hailstone, bi: Double): Double =
        (b.x + (b.velocityX * bi) - a.x) / a.velocityX
