package com.epam.sensors

case class Measurement(valueOpt: Option[BigDecimal])
case class MeasurementAggregate(statsOpt: Option[Stats], failedMeasurements: Int)

case class SensorId(value: String)
case class SensorMeasurement(id: SensorId, value: Measurement)
case class SensorMeasurements(value: Iterator[SensorMeasurement])