package csl.parser

import csl.ast._

import scala.util.parsing.combinator.JavaTokenParsers

class Parser extends JavaTokenParsers {

  override val whiteSpace = """(\s|//.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r

  def variable: Parser[Variable] = (ident <~ "=") ~ request ~ ("=>" ~> response) ^^ {
    case v ~ req ~ res  => Variable(v, req, res)
  }

  // Request related parsers.
  def request: Parser[ObjectValue] = "request" ~> objectValue

  // Response related parsers.
  def response: Parser[ObjectValue] = "response" ~> objectValue

  // Property related parsers.
  def property: Parser[Property] = key ~ value ^^ {
    case k ~ v => Property(k, v)
  }

  def key: Parser[String] = """[^= {}]*""".r

  def value: Parser[Value] = objectValue | ("=" ~> (regexValue | dateValue | stringValue | numberValue))

  def regexValue: Parser[RegexValue] = ("\"\"\"" + """.*""" + "\"\"\"").r ^^ {
    case s: String => RegexValue(s.substring(3, s.length - 3).replace("\\", ""))
  }

  def stringValue: Parser[StringValue] = ("\"" + """.*""").r ^^ {
    case s: String => StringValue(removeQuotes(s))
  }

  def numberValue: Parser[NumberValue] = (decimalNumber | wholeNumber) ^^ {
    case n => NumberValue(n.toDouble)
  }

  def dateValue: Parser[DateValue] = ("\"" + """[1-9][0-9]{3}-(0[0-9]|1[0-2])-(3[0-1]|[1-2][0-9]|0[0-9])T(2[0-4]|1[0-9]|0[0-9]):([0-5][0-9]|60):([0-5][0-9]|60)[+-](0[1-9]|1[0-2]):(00|15|30|45)""" + "\"").r ^^ {
    case s: String => DateValue(removeQuotes(s))
  }

  def objectValue: Parser[ObjectValue] = "{" ~> rep(property) <~ "}" ^^ ObjectValue

  private def removeQuotes(s: String): String = {
    s.substring(1, s.length - 1).replace("\\", "")
  }

}
