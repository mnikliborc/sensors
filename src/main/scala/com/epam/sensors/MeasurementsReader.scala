package com.epam.sensors

import java.nio.file.{Files, Path, Paths}

import zio.{Task, ZIO}

import scala.util.Try

import scala.collection.JavaConverters._

object MeasurementsReader {
  def readFromCsvFiles(dir: String): Task[List[SensorMeasurements]] =
    for {
      paths <- ZIO.effect(Files.list(Paths.get(dir)))
        .map(_.iterator().asScala.toList)
        .map(_.filter(_.toFile.getName.endsWith(".csv")))
      xs <- ZIO.foreach(paths)(readFromFile)
    } yield xs

  def readFromFile(file: Path): Task[SensorMeasurements] =
    ZIO.succeed {
      io.Source.fromFile(file.toFile).getLines()
        .drop(1)
        .flatMap(toSensorMeasurement)
    }.map(SensorMeasurements.apply)

  def toSensorMeasurement(line: String): Option[SensorMeasurement] = Try {
    line.split(",").toList match {
      case id :: value :: Nil =>
        if (value == "NaN") {
          Some(SensorMeasurement(SensorId(id), Measurement(None)))
        } else {
          val intValue = value.toInt
          if (intValue >= 0 && intValue <= 100) Some(SensorMeasurement(SensorId(id), Measurement(Some(intValue))))
          else                                  None
        }
      case _ =>
        None
    }
  }.toOption.flatten
}