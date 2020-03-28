package com.epam.sensors

import zio.test._
import zio.test.Assertion._
import zio.test.junit.JUnitRunnableSpec

class MeasurementsAggregatorSpec extends JUnitRunnableSpec {
  val sensorDataGen = Gen.alphaNumericString <&> Gen.listOf(Gen.option(Gen.int(0, 100)))
  val sensorsDataGen = Gen.listOf(sensorDataGen)

  val sensorMeasurementsGen = sensorsDataGen.map { data =>
      data.flatMap { case (id, values) =>
        values.map(value => SensorMeasurement(SensorId(id), Measurement(value.map(BigDecimal.apply))))
      }
  }

  override def spec = suite("MeasurementsAggregator")(
    testM("aggregate") {
      check(sensorMeasurementsGen) { measurements =>
        val got = MeasurementsAggregator.aggregate(SensorMeasurements(measurements.iterator))
        val expected = aggregateInPlace(measurements)

        assert(got.mapValues(_.statsOpt.map(_.rounded)))(equalTo(expected.mapValues(_.statsOpt.map(_.rounded))))
      }
    },

    testM("mergeAggregates") {
      check(Gen.listOf(sensorMeasurementsGen)) { measurements =>
        val aggregates = measurements.map(_.iterator).map(SensorMeasurements).map(MeasurementsAggregator.aggregate)
        val got = MeasurementsAggregator.mergeAggregates(aggregates)
        val expected = aggregateInPlace(measurements.flatten)

        assert(got.mapValues(_.statsOpt.map(_.rounded)))(equalTo(expected.mapValues(_.statsOpt.map(_.rounded))))
      }
    }
  )

  def aggregateInPlace(measurements: List[SensorMeasurement]): Map[SensorId, MeasurementAggregate] =
    measurements.groupBy(_.id).map { case (id, ms) =>
      val valid = ms.flatMap(_.value.valueOpt)
      val failed = ms.count(_.value.valueOpt.isEmpty)

      val statsOpt =
        if (valid.nonEmpty) Some(Stats(Min(valid.min), Max(valid.max), Avg(valid.sum/valid.size), valid.size))
        else None

      id -> MeasurementAggregate(statsOpt, failed)
    }
}
