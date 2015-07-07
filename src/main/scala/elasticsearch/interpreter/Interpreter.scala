package elaticsearch.interpreter

import java.text.SimpleDateFormat
import java.util.Calendar

import csl.ast.{CsvFile, Detector}
import csl.interpreter.{Interpreter => CSLInterpreter}
import elasticsearch.result.Csv
import elasticsearch.{PatternDetector, RelationCollector, RequestDefinitionCollector, Storage}

object Interpreter {

  def main(args: Array[String]) {
    val detector = CSLInterpreter.fromFile("./src/main/resources/csl/example_1.csl")

    Storage.init

    val requestDefinitionCollector = new RequestDefinitionCollector(detector)
    if (requestDefinitionCollector.collect) {
      println("Variable Collector completed")
    }

    val relationCollector = new RelationCollector(detector)
    if (relationCollector.collect) {
      println("Relation Collector completed")
    }

    val patternDetector = new PatternDetector(detector)
    val documents = patternDetector.detect
    println(patternDetector.totalMatches + " matches found existing of a total of " + documents.length + " documents.")

    detector.result.export match {
      case CsvFile(keys) =>
        val csv = new Csv(documents, keys)
        csv.save(s"./src/main/resources/exports/${filename(detector)}.csv");
      case e => throw new Exception(s"Export type '$e' not supported.")
    }

    Storage.close
    System.exit(0)
  }

  private def filename(detector: Detector): String = {
    val today = Calendar.getInstance.getTime
    val dateFormat = new SimpleDateFormat("ddMMyyyy_HHmmss");
    val label = detector.label.replace(" ", "_")
    s"${label}_${dateFormat.format(today)}"
  }
}
