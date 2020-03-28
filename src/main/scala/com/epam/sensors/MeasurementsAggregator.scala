package com.epam.sensors

object MeasurementsAggregator {
  def mergeAggregates(aggs: List[Map[SensorId, MeasurementAggregate]]): Map[SensorId, MeasurementAggregate] =
    aggs.flatMap(_.toList)
      .groupBy(_._1)
      .mapValues(_.map(_._2))
      .mapValues(_.reduce(update(_, _)))

  def aggregate(measurements: SensorMeasurements): Map[SensorId, MeasurementAggregate] =
    measurements.value.foldLeft(Map[SensorId, MeasurementAggregate]()) {
      case (aggs, SensorMeasurement(sensorId, measurement)) =>
        aggs.get(sensorId) match {
          case Some(agg) =>
            aggs.updated(sensorId, update(agg, measurement))
          case None =>
            val agg =
              measurement.valueOpt match {
                case Some(v) => MeasurementAggregate(Some(Stats.init(v)), 0)
                case None    => MeasurementAggregate(None, 1)
              }
            aggs.updated(sensorId, agg)
        }
    }

  def update(agg: MeasurementAggregate, measurement: Measurement): MeasurementAggregate =
    (agg.statsOpt, measurement.valueOpt) match {
      case (Some(stats), Some(value)) => agg.copy(statsOpt = Some(Stats.update(stats, value)))
      case (None, Some(value))        => agg.copy(statsOpt = Some(Stats.init(value)))
      case (Some(_), None)            => agg.copy(failedMeasurements = agg.failedMeasurements + 1)
      case (None, None)               => agg.copy(failedMeasurements = agg.failedMeasurements + 1)
    }

  def update(agg: MeasurementAggregate, other: MeasurementAggregate): MeasurementAggregate = {
    val statsOpt =
      (agg.statsOpt, other.statsOpt) match {
        case (Some(as), Some(bs)) => Some(Stats.update(as, bs))
        case (aOpt, bOpt)         => aOpt.orElse(bOpt)
      }

    MeasurementAggregate(statsOpt, agg.failedMeasurements + other.failedMeasurements)
  }
}