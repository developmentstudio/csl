package csl.parser

import csl.ast._

import scala.util.parsing.combinator.JavaTokenParsers

class Parser extends JavaTokenParsers {

  override val whiteSpace = """(\s|//.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r

  def protocol = "protocol =" ~> value

  def value: Parser[Value] = (regexValue | stringValue | numberValue)

  def stringValue: Parser[StringValue] = ("\"" + """.*""" + "\"").r ^^ StringValue

  def numberValue: Parser[NumberValue] = (wholeNumber | decimalNumber) ^^ NumberValue

  def regexValue: Parser[RegexValue] = ("\"\"\"" + """.*""" + "\"\"\"").r ^^ RegexValue

// ("\""+"""([^"\p{Cntrl}\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*+"""+"\"").r


//  def ltrim(s: String) = s.replaceAll("^\\s+", "")
//  def rtrim(s: String) = s.replaceAll("\\s+$", "")




//  def detector: Parser[Detector] = "detector" ~> stringLiteral <~ "{" ~> rep(variable) <~ "}" ^^ {
//    case l ~ vs => Detector(l, vs)
//  }

//  def variable: Parser[Variable] = ident ~ ("{" ~> request ~ response <~ "}") ^^ {
//    case name ~ (req ~ res) => Variable(name, req, res)
//  }
//
//  def request: Parser[Request] = "request" ~> properties ^^ Request
//
//  def response: Parser[Response] = "response" ~> properties ^^ Response
//
//  def properties: Parser[List[Property]] = "{" ~> rep(property) <~ "}"
//
//  def property: Parser[Property] = name ~ value ^^ {
//    case n ~ v => Property(n, v)
//  }
//
//  def name: Parser[String] = """[a-zA-Z]+[a-zA-Z0-9\.-]*""".r <~ "="
//
//  def value: Parser[String] = ident | stringLiteral | wholeNumber // | regex

}
