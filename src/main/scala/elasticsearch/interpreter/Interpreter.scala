package elaticsearch.interpreter

import java.text.SimpleDateFormat
import java.util.Calendar

import csl.ast.Detector
import csl.interpreter.{Interpreter => CSLInterpreter}
import elasticsearch.result.Csv
import elasticsearch.{PatternDetector, RelationCollector, RequestDefinitionCollector, Storage}

object Interpreter {

  def main(args: Array[String]) {
    val detector = CSLInterpreter.fromFile("./src/main/resources/csl/example_1.csl")
    println(detector)

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

    // TODO: Handle result / export result (CSV, ES-QUERY)
    val keys: List[String] = List(
      "request.timestamp",
      "request.cookies.Coolblue-Session"
    )

    val csv = new Csv(documents, keys)
    csv.save(s"./src/main/resources/exports/${filename(detector)}");

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
