package com.epam.sensors

import scala.math.BigDecimal.RoundingMode

case class Min(value: BigDecimal)
case class Max(value: BigDecimal)
case class Avg(value: BigDecimal)

case class Stats(min: Min, max: Max, avg: Avg, count: Int) {
  def rounded(): Stats =
    Stats(
      Min(rounded(min.value)),
      Max(rounded(max.value)),
      Avg(rounded(avg.value)),
      count
    )

  private def rounded(x: BigDecimal): BigDecimal =
    x.setScale(6, RoundingMode.HALF_UP)
}

object Stats {
  def init(value: BigDecimal): Stats =
    Stats(Min(value), Max(value), Avg(value), 1)

  def update(stats: Stats, value: BigDecimal): Stats = {
    val cnt = stats.count + 1
    val min = Set(stats.min.value, value).min
    val max = Set(stats.max.value, value).max
    val avg = stats.avg.value * (BigDecimal(cnt - 1) / cnt) + (value / cnt)

    Stats(Min(min), Max(max), Avg(avg), cnt)
  }

  def update(stats: Stats, other: Stats): Stats = {
    val cnt = stats.count + other.count
    val min = Set(stats.min.value, other.min.value).min
    val max = Set(stats.max.value, other.max.value).max
    val avg = stats.avg.value * (BigDecimal(stats.count)/cnt) + other.avg.value * (BigDecimal(other.count)/cnt)

    Stats(Min(min), Max(max), Avg(avg), cnt)
  }
}