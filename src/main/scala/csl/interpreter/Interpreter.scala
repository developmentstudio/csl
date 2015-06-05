package csl.interpreter

import csl.ast.{Detector, Variable}
import csl.parser.{DetectorParser, VariableParser}
import csl.elasticsearch.ScrollSearch
import scala.io.Source
import com.mysql.jdbc.jdbc2
import scala.concurrent.ExecutionContext.Implicits.global

object Interpreter {

  def main(args: Array[String]) {
    val source = Source.fromFile("./src/main/resources/test_3.csl").mkString

    parseSource(source) match {
      case Some(ast) => {

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
}
