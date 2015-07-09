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

  val startTime = System.nanoTime()

  def main(args: Array[String]) {

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

    val startTime = System.nanoTime()

    val requestDefinitionCollector = new RequestDefinitionCollector(detector)
    if (requestDefinitionCollector.collect) {
      println("Variable Collector completed")
    }
    printTimeElapsed

    val relationCollector = new RelationCollector(detector)
    if (relationCollector.collect) {
      println("Relation Collector completed")
    }
    printTimeElapsed

    val patternDetector = new PatternDetector(detector)
    val documents = patternDetector.detect
    println(patternDetector.totalMatches + " matches found existing of a total of " + documents.length + " documents.")
    printTimeElapsed

    detector.result.export match {
      case CsvFile(keys) =>
        val csv = new Csv(documents, keys)
        val path = Config.setting("settings.result.exportPath")
        csv.save(path + filename(detector, ".csv"))
        println("Csv export completed")
      case e => throw new Exception(s"Export type '$e' not supported.")
    }
    printTimeElapsed

    Storage.close
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

  private def printTimeElapsed: Unit = {
    val endTime = System.nanoTime()
    println("Elapsed time: " + (endTime - startTime) / 1000000000 + " seconds")
  }
}
