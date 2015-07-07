package csl.interpreter

import csl.ast.Detector
import csl.parser.DetectorParser
import csl.typechecker.{Error, TypeChecker}

import scala.io.Source

object Interpreter {

  def fromFile(path: String): Detector = {
    val source = Source.fromFile(path).mkString

    parseSource(source) match {
      case Some(detector) =>
        val errors = typeChecker(detector)
        errors.foreach(println)
        if (errors.nonEmpty) {
          System.exit(0)
        }
        detector
      case None => throw new Exception("Parser failed!")
    }
  }

  private def parseSource(source: String): Option[Detector] = {
    val parser = new DetectorParser()
    parser.parseAll[Detector](parser.detector, source) match {
      case parser.Success(ast: Detector, _) => Some(ast)
      case parser.Failure(msg, next) => throw new Exception("Parse failure at line " + next.pos + ": " + msg); None
      case parser.Error(msg, next) => throw new Exception("Parse error at line " + next.pos + ": " + msg); None
    }
  }

  private def typeChecker(ast: Detector): List[Error] = {
    val checker = new TypeChecker
    checker.check(ast)
    checker.errors
  }
}
