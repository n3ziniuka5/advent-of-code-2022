package aoc

import zio.test.{ZIOSpecDefault, assertTrue}

import scala.collection.mutable.ArrayBuffer

object Day20Test extends ZIOSpecDefault {
  /*val input = List(
    "88",
    "99",
    "77",
    "66",
    "5"
  )*/

  /*val input = List(
    "0",
    "10",
    "2",
    "3",
    "1",
  )*/

  val input = List(
    "1",
    "2",
    "-3",
    "3",
    "-2",
    "0",
    "4"
  )

  override def spec = suite("Day 20")(
    // test("Part 1")(assertTrue(Day20.part1(input) == 3L)),
    test("Part 2")(assertTrue(Day20.part2(input) == 1623178306L)),

    /*test("wrap around logic, positive numbers")(
      assertTrue(Day20.solve(ArrayBuffer.apply[Long](0, 0, 0, 1)).toList == List[Long](0, 1, 0, 0)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](0, 0, 0, 2)).toList == List[Long](0, 0, 2, 0)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](0, 0, 0, 3)).toList == List[Long](0, 0, 0, 3)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](0, 0, 0, 4)).toList == List[Long](0, 4, 0, 0)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](0, 0, 0, 5)).toList == List[Long](0, 0, 5, 0)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](0, 0, 0, 6)).toList == List[Long](0, 0, 0, 6)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](0, 0, 0, 7)).toList == List[Long](0, 7, 0, 0)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](0, 0, 0, 8)).toList == List[Long](0, 0, 8, 0)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](0, 0, 0, 9)).toList == List[Long](0, 0, 0, 9)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](0, 0, 0, 10)).toList == List[Long](0, 10, 0, 0)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](0, 0, 0, 11)).toList == List[Long](0, 0, 11, 0)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](0, 0, 0, 12)).toList == List[Long](0, 0, 0, 12)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](0, 0, 0, 13)).toList == List[Long](0, 13, 0, 0)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](0, 0, 0, 14)).toList == List[Long](0, 0, 14, 0)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](0, 0, 0, 15)).toList == List[Long](0, 0, 0, 15)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](0, 0, 0, 16)).toList == List[Long](0, 16, 0, 0)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](1, 0, 0, 0)).toList == List[Long](0, 1, 0, 0)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](2, 0, 0, 0)).toList == List[Long](0, 0, 2, 0)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](3, 0, 0, 0)).toList == List[Long](0, 0, 0, 3)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](4, 0, 0, 0)).toList == List[Long](0, 4, 0, 0)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](5, 0, 0, 0)).toList == List[Long](0, 0, 5, 0)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](6, 0, 0, 0)).toList == List[Long](0, 0, 0, 6)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](7, 0, 0, 0)).toList == List[Long](0, 7, 0, 0)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](8, 0, 0, 0)).toList == List[Long](0, 0, 8, 0)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](9, 0, 0, 0)).toList == List[Long](0, 0, 0, 9)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](10, 0, 0, 0)).toList == List[Long](0, 10, 0, 0)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](11, 0, 0, 0)).toList == List[Long](0, 0, 11, 0)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](12, 0, 0, 0)).toList == List[Long](0, 0, 0, 12)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](13, 0, 0, 0)).toList == List[Long](0, 13, 0, 0)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](14, 0, 0, 0)).toList == List[Long](0, 0, 14, 0)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](15, 0, 0, 0)).toList == List[Long](0, 0, 0, 15)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](16, 0, 0, 0)).toList == List[Long](0, 16, 0, 0)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](17, 0, 0, 0)).toList == List[Long](0, 0, 17, 0)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](18, 0, 0, 0)).toList == List[Long](0, 0, 0, 18)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](0, 10, 0, 0, 0)).toList == List[Long](0, 0, 0, 10, 0))
    ),
    test("wrap around logic, negative numbers")(
      assertTrue(Day20.solve(ArrayBuffer.apply[Long](0, 0, 0, -1)).toList == List[Long](0, 0, -1, 0)) &&     // 0
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](0, 0, 0, -2)).toList == List[Long](0, -2, 0, 0)) &&   // 0
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](0, 0, 0, -3)).toList == List[Long](-3, 0, 0, 0)) &&   // 0
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](0, 0, 0, -4)).toList == List[Long](0, 0, -4, 0)) &&   // 1
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](0, 0, 0, -5)).toList == List[Long](0, -5, 0, 0)) &&   // 1
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](0, 0, 0, -6)).toList == List[Long](-6, 0, 0, 0)) &&   // 1
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](0, 0, 0, -7)).toList == List[Long](0, 0, -7, 0)) &&   // 2
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](0, 0, 0, -8)).toList == List[Long](0, -8, 0, 0)) &&   // 2
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](0, 0, 0, -9)).toList == List[Long](-9, 0, 0, 0)) &&   // 2
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](0, 0, 0, -10)).toList == List[Long](0, 0, -10, 0)) && // 3
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](0, 0, 0, -11)).toList == List[Long](0, -11, 0, 0)) && // 3
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](0, 0, 0, -12)).toList == List[Long](-12, 0, 0, 0)) && // 3
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](0, 0, 0, -13)).toList == List[Long](0, 0, -13, 0)) && // 4
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](0, 0, 0, -14)).toList == List[Long](0, -14, 0, 0)) && // 4
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](0, 0, 0, -15)).toList == List[Long](-15, 0, 0, 0)) && // 4
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](0, 0, 0, -16)).toList == List[Long](0, 0, -16, 0)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](-1, 0, 0, 0)).toList == List[Long](0, 0, -1, 0)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](-2, 0, 0, 0)).toList == List[Long](0, -2, 0, 0)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](-3, 0, 0, 0)).toList == List[Long](-3, 0, 0, 0)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](-4, 0, 0, 0)).toList == List[Long](0, 0, -4, 0)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](-5, 0, 0, 0)).toList == List[Long](0, -5, 0, 0)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](-6, 0, 0, 0)).toList == List[Long](-6, 0, 0, 0)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](-7, 0, 0, 0)).toList == List[Long](0, 0, -7, 0)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](-8, 0, 0, 0)).toList == List[Long](0, -8, 0, 0)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](-9, 0, 0, 0)).toList == List[Long](-9, 0, 0, 0)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](-10, 0, 0, 0)).toList == List[Long](0, 0, -10, 0)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](-11, 0, 0, 0)).toList == List[Long](0, -11, 0, 0)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](-12, 0, 0, 0)).toList == List[Long](-12, 0, 0, 0)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](-13, 0, 0, 0)).toList == List[Long](0, 0, -13, 0)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](-14, 0, 0, 0)).toList == List[Long](0, -14, 0, 0)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](-15, 0, 0, 0)).toList == List[Long](-15, 0, 0, 0)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](-16, 0, 0, 0)).toList == List[Long](0, 0, -16, 0)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](0, 0, 0, -1, 0, 0)).toList == List[Long](0, 0, -1, 0, 0, 0)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](0, 0, 0, -2, 0, 0)).toList == List[Long](0, -2, 0, 0, 0, 0)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](0, 0, 0, -3, 0, 0)).toList == List[Long](-3, 0, 0, 0, 0, 0)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](0, 0, 0, -4, 0, 0)).toList == List[Long](0, 0, 0, 0, -4, 0)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](0, 0, 0, -5, 0, 0)).toList == List[Long](0, 0, 0, -5, 0, 0)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](0, 0, 0, -6, 0, 0)).toList == List[Long](0, 0, -6, 0, 0, 0)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](0, 0, 0, -7, 0, 0)).toList == List[Long](0, -7, 0, 0, 0, 0)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](0, 0, 0, -8, 0, 0)).toList == List[Long](-8, 0, 0, 0, 0, 0)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](0, 0, 0, -9, 0, 0)).toList == List[Long](0, 0, 0, 0, -9, 0)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](0, 0, 0, -10, 0, 0)).toList == List[Long](0, 0, 0, -10, 0, 0)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](0, 0, 0, -11, 0, 0)).toList == List[Long](0, 0, -11, 0, 0, 0)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](0, 0, 0, -12, 0, 0)).toList == List[Long](0, -12, 0, 0, 0, 0)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](0, 0, 0, -13, 0, 0)).toList == List[Long](-13, 0, 0, 0, 0, 0)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](0, 0, 0, -14, 0, 0)).toList == List[Long](0, 0, 0, 0, -14, 0)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](0, 0, 0, -15, 0, 0)).toList == List[Long](0, 0, 0, -15, 0, 0)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](0, 0, 0, -16, 0, 0)).toList == List[Long](0, 0, -16, 0, 0, 0)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](0, 0, 0, -17, 0, 0)).toList == List[Long](0, -17, 0, 0, 0, 0)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](0, 0, 0, -18, 0, 0)).toList == List[Long](-18, 0, 0, 0, 0, 0)) &&
        assertTrue(Day20.solve(ArrayBuffer.apply[Long](0, 0, 0, -19, 0, 0)).toList == List[Long](0, 0, 0, 0, -19, 0))
    )*/
  )
}
