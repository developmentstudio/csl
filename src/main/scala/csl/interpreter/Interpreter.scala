package csl.interpreter

import csl.ast.{Detector}
import csl.parser.DetectorParser
import csl.typechecker.{Error, Warning, TypeChecker}
import scala.io.Source

object Interpreter {

  def main(args: Array[String]) {
    val source = Source.fromFile("./src/main/resources/test_3.csl").mkString

    parseSource(source) match {
      case Some(ast) => {

        val (errors, warnings) = typeChecker(ast)
        errors.map(println)
        warnings.map(println)

        if (errors.size > 0) {
          System.exit(0)
        }

        println(ast)


//        val search = new ScrollSearch(ast)
//        search.search()
      }
      case None => println("Parser Failed.")
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
