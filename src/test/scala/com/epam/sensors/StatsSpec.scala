package com.epam.sensors

import zio.test._
import zio.test.Assertion._
import zio.test.junit.JUnitRunnableSpec

class StatsSpec extends JUnitRunnableSpec {
  def spec = suite("Stats")(
    testM("update with single value is correct") {
      val decimal = Gen.int(0, 100).map(BigDecimal(_))
      val decimals = Gen.listOf(decimal).filter(_.nonEmpty)

      check(decimals <&> decimal) { case (xs, x) =>
        val before = statsFromList(xs)
        val after = statsFromList(x :: xs)

        assert(Stats.update(before, x).rounded)(equalTo(after.rounded))
      }
    },

    testM("update with other Stats is correct") {
      val decimal = Gen.int(0, 100).map(BigDecimal(_))
      val decimals = Gen.listOf(decimal).filter(_.nonEmpty)

      check(decimals <&> decimals) { case (xs1, xs2) =>
        val s1 = statsFromList(xs1)
        val s2 = statsFromList(xs2)
        val expected = statsFromList(xs1 ::: xs2)

        assert(Stats.update(s1, s2).rounded)(equalTo(expected.rounded))
      }
    }
  )

  private def statsFromList(xs: List[BigDecimal]): Stats =
    Stats(Min(xs.min), Max(xs.max), Avg(xs.sum / xs.size), xs.size)
}
