package csl.interpreter

import csl.ast.Detector
import csl.elasticsearch.ScrollSearch
import csl.parser.DetectorParser
import csl.typechecker.{Error, TypeChecker, Warning}

import scala.io.Source

object Interpreter {

  def main(args: Array[String]) {
    val source = Source.fromFile("./src/main/resources/test_3.csl").mkString

    parseSource(source) match {
      case Some(detector) =>
        val (errors, warnings) = typeChecker(detector)
        errors.foreach(println)
        warnings.foreach(println)

        if (errors.nonEmpty) {
          System.exit(0)
        }

        // Idea: detector.detect instead of the lines below?
        val search = new ScrollSearch(detector)
        search.search()


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
