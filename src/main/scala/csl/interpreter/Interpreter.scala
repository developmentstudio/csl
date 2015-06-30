package csl.interpreter

import csl.ast.Detector
import csl.elasticsearch.{PatternDetector, RelationCollector, RequestDefinitionCollector}
import csl.parser.DetectorParser
import csl.storage.ResponseStorage
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
          System.exit(500)
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
        println(matchedDocuments)

        val end = System.nanoTime()
        println("Elapsed time: " + (end - start) / 1000000000 + " seconds")

        System.exit(200)

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
