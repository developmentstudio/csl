package csl.parser

import csl.ast._

import scala.util.parsing.combinator.JavaTokenParsers

class Parser extends JavaTokenParsers {

  override val whiteSpace = """(\s|//.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r


  // Request related parsers.

  def headers: Parser[Headers] = "headers" ~ "{" ~> rep(property) <~ "}" ^^ Headers

  def cookies: Parser[Cookies] = "cookies" ~ "{" ~> rep(property) <~ "}" ^^ Cookies

  def method: Parser[Method] = "method" ~> value ^^ Method

  def protocol: Parser[Protocol] = "protocol" ~> value ^^ Protocol

  def remoteAddress: Parser[RemoteAddress] = "remote-address" ~> value ^^ RemoteAddress

  def url = "url" ~ "{" ~> rep(host | uri | query) <~ "}" ^^ Url

  def host = "host" ~> value ^^ Host

  def uri = "uri" ~> value ^^ Uri

  def query = "query" ~ "{" ~> rep(property) <~ "}" ^^ Query

  // Property related parsers.

  def property: Parser[Property] = key ~ value ^^ {
    case k ~ v => Property(k, v)
  }

  def key: Parser[String] = """[^= ]*""".r

  def value: Parser[Value] = "=" ~> (regexValue | stringValue | numberValue)

  def stringValue: Parser[StringValue] = ("\"" + """.*""" + "\"").r ^^ {
    case s => StringValue(s.substring(1, s.length - 1).replace("\\", ""))
  }

  def numberValue: Parser[NumberValue] = (wholeNumber | decimalNumber) ^^ NumberValue

  def regexValue: Parser[RegexValue] = ("\"\"\"" + """.*""" + "\"\"\"").r ^^ RegexValue



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
