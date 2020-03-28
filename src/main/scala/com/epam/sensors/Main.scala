package com.epam.sensors

import zio._
import zio.console._

case class ProgramResult(processedFiles: Int, processedMeasurements: Int, failedMeasurements: Int, sensorsByAvgDesc: List[(SensorId, Option[Stats])])

object Main extends zio.App {
  override def run(args: List[String]) = {
    for {
      inputDir <- ZIO.effect(args(0)).orElse(printAndFail("Usage: ./sensor_stats.sh <input_dir>"))
      result   <- program(inputDir).onError(err => printAndFail(err.prettyPrint).flip)
      _        <- Printer.print(result)
    } yield ()
  }.fold(_ => 1, _ => 0)

  def program(inputDir: String): ZIO[Any, Throwable, ProgramResult] =
    for {
      fileMeasurements     <- MeasurementsReader.readFromCsvFiles(inputDir)
      fileAggregates       <- ZIO.foreachPar(fileMeasurements)(ms => ZIO.succeed(MeasurementsAggregator.aggregate(ms)))
      globalAggregates     <- ZIO.succeed(MeasurementsAggregator.mergeAggregates(fileAggregates))

      failedMeasurements    = globalAggregates.values.map(_.failedMeasurements).sum
      processedMeasurements = globalAggregates.values.flatMap(_.statsOpt).map(_.count).sum + failedMeasurements
      sensorsByAvgDesc      = globalAggregates.mapValues(_.statsOpt).toList.sortBy(-_._2.map(_.avg.value).getOrElse(BigDecimal(-1)))
    } yield ProgramResult(fileAggregates.size, processedMeasurements, failedMeasurements, sensorsByAvgDesc)

  def printAndFail(msg: String) =
    putStrLn(msg) *> ZIO.fail(())
}