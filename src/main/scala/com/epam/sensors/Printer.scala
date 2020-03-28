package com.epam.sensors

import zio.ZIO
import zio.console.{Console, putStrLn}

object Printer {
  def print(r: ProgramResult): ZIO[Console, Nothing, Unit] =
    for {
      _ <- putStrLn(s"Num of processed files: ${r.processedFiles}")
      _ <- putStrLn(s"Num of processed measurements: ${r.processedMeasurements}")
      _ <- putStrLn(s"Num of failed measurements: ${r.failedMeasurements}")
      _ <- putStrLn("")
      _ <- putStrLn("Sensors with highest avg humidity:")
      _ <- putStrLn("")
      _ <- putStrLn("sensor-id,min,avg,max")

      _ <- ZIO.foreach(r.sortedSensors)((printAggregate _).tupled)
    } yield ()

  def printAggregate(id: SensorId, statsOpt: Option[Stats]): ZIO[Console, Nothing, Unit] = {
    val min = statsOpt.map(_.min.value.toInt).getOrElse("NaN")
    val max = statsOpt.map(_.max.value.toInt).getOrElse("NaN")
    val avg = statsOpt.map(_.avg.value.toInt).getOrElse("NaN")

    putStrLn(s"${id.value},$min,$avg,$max")
  }
}