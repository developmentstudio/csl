package csl.interpreter

import csl.ast.Detector
import csl.elasticsearch.{PatternDetector, RelationCollector, RequestDefinitionCollector, ResponseStorage}
import csl.parser.DetectorParser
import csl.typechecker.{Error, TypeChecker, Warning}

import scala.io.Source

object Interpreter {

  def main(args: Array[String]) {
    val source = Source.fromFile("./src/main/resources/test_3.csl").mkString

    parseSource(source) match {
      case Some(detector) =>

        val start = System.nanoTime()

        val (errors, warnings) = typeChecker(detector)
        errors.foreach(println)
        warnings.foreach(println)

        if (errors.nonEmpty) {
          System.exit(0)
        }

        println(detector)

        ResponseStorage.clear

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
        println(patternDetector.totalMatches + " matches found existing of a total of " + matchedDocuments.length + " documents.")


        // TODO: Handle result / export result (JSON, XML, CSV, ES-QUERY, HTML TABLE)
        matchedDocuments.foreach(d => {
          println(d.source("request.timestamp").getOrElse(""))
        })

        val end = System.nanoTime()
        println("Elapsed time: " + (end - start) / 1000000000 + " seconds")

        System.exit(0)

      case None => println("Parser failed.")
    }
  }

  def parseSource(source: String): Option[Detector] = {
    val parser = new DetectorParser()

    parser.parseAll[Detector](parser.detector, source) match {
      case parser.Success(ast: Detector, _) => Some(ast)
      case parser.Failure(msg, next) => println("Parse failure at line " + next.pos + ": " + msg); None
      case parser.Error(msg, next) => println("Parse error at line " + next.pos + ": " + msg); None
    }
  }

  def typeChecker(ast: Detector): (List[Error], List[Warning]) = {
    val checker = new TypeChecker
    checker.check(ast)

    (checker.errors, checker.warnings)
  }
}
