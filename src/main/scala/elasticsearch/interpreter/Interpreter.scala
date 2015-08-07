package elaticsearch.interpreter

import java.io.File
import java.text.SimpleDateFormat
import java.util.Calendar

import csl.ast.{CsvFile, Detector}
import csl.interpreter.{Interpreter => CSLInterpreter}
import elasticsearch._
import elasticsearch.collector.{RequestDefinitionCollector, RelationCollector}
import elasticsearch.detector.PatternDetector
import elasticsearch.result.Csv

object Interpreter {

  def main(args: Array[String]) {

    val detectorStartTime = System.nanoTime()

    if (args.length == 0) {
      println("Please input a csl file while running the program.")
      System.exit(0)
    } else {
      if (!checkFile(args(0))) {
        System.exit(0)
      }
    }

    val detector = CSLInterpreter.fromFile(args(0))

    Storage.init

    val requestDefinitionCollectorStartTime = System.nanoTime()
    val requestDefinitionCollector = new RequestDefinitionCollector(detector)
    if (requestDefinitionCollector.collect) {
      print("Request Definition Collector completed. ")
      printTimeElapsed(requestDefinitionCollectorStartTime, "RequestDefinitionCollector")
    }


    val relationCollectorStartTime = System.nanoTime()
    val relationCollector = new RelationCollector(detector)
    print("Relation Collector started. ")
    if (relationCollector.collect) {
      print("Relation Collector completed. ")
      printTimeElapsed(relationCollectorStartTime, "RelationCollector")
    }


    val patternDetectorStartTime = System.nanoTime()
    val patternDetector = new PatternDetector(detector)
    print("Pattern Detector started. ")
    val documents = patternDetector.detect
    print(patternDetector.totalMatches + " matches found existing of a total of " + documents.length + " documents. ")
    printTimeElapsed(patternDetectorStartTime, "PatternDetector")

    val resultExportStartTime = System.nanoTime()
    print("Result export started. ")
    detector.result.export match {
      case CsvFile(keys) =>
        val csv = new Csv(documents, keys)
        val path = Config.setting("settings.result.exportPath")
        csv.save(path + filename(detector, ".csv"))
        print("Csv export completed. ")
        printTimeElapsed(resultExportStartTime, "Result export to CSV")
      case e => throw new Exception(s"Export type '$e' not supported.")
    }
    Storage.close

    println("")
    printTimeElapsed(detectorStartTime, "Detector: " + detector.label)

    System.exit(0)
  }

  private def checkFile(path: String): Boolean = {
      val f = new File(path)
      if (f.exists && f.canRead) {
        true
      } else {
        if (!f.exists) {
          println("File \"" + path + "\" doesn't exists")
        } else {
          if (!f.canRead) {
            println(s"Can't read file: $path")
          }
        }
        false
      }
  }

  private def filename(detector: Detector, extention: String = ""): String = {
    val today = Calendar.getInstance.getTime
    val dateFormat = new SimpleDateFormat("ddMMyyyy_HHmmss");
    val label = detector.label.replace(" ", "_")
    s"${label}_${dateFormat.format(today)}${extention}"
  }

  private def printTimeElapsed(start: Long, label: String): Unit = {
    println("Elapsed time: " + (System.nanoTime() - start) / 1000000000 + " seconds" + " (" + label + ")")
  }
}
