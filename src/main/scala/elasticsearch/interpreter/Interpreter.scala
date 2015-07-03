package elaticsearch.interpreter

import csl.interpreter.{Interpreter => CSLInterpreter}
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
    val matchedDocuments = patternDetector.detect
    println(patternDetector.totalMatches + " matches found existent of a total of " + matchedDocuments.length + " documents.")


    // TODO: Handle result / export result (CSV, ES-QUERY)
    matchedDocuments.foreach(d => println(d.source("request.cookies.Coolblue-Session").getOrElse("Property not found")))


    Storage.close
    System.exit(0)

  }
}
