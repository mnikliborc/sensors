package com.epam.sensors

import zio.test._
import zio.test.Assertion._
import zio.test.junit.JUnitRunnableSpec

class MainSpec extends JUnitRunnableSpec {
  override def spec = suite("Main")(
    testM("process sample data") {
      val expected = ProgramResult(
        processedFiles = 2,
        processedMeasurements = 7,
        failedMeasurements = 2,
        sensorsByAvgDesc = List(
          SensorId("s2") -> Some(Stats(Min(78), Max(88), Avg(82), 3)),
          SensorId("s1") -> Some(Stats(Min(10), Max(98), Avg(54), 2)),
          SensorId("s3") -> None,
        )
      )

      assertM(Main.program("src/test/resources/data").map(rounded))(equalTo(rounded(expected)))
    }
  )

  def rounded(result: ProgramResult): ProgramResult =
    result.copy(sensorsByAvgDesc = result.sensorsByAvgDesc.map { case (id, stats) => id -> stats.map(_.rounded) })

}
