package csl.parser

import csl.ast._
import org.specs2.matcher.ParserMatchers
import org.specs2.mutable.Specification

import scala.util.matching.Regex



//  def test: Variable = Variable("name", Request(Object(List(
//    Property("url", Object(List(
//      Property("host", StringValue("www.pdashop.nl"))
//    )))
//  ))), Response(Object(List.empty)))

class ParserSpec extends Specification with ParserMatchers {

  val parsers = new Parser

  val LB: String = "\n"

  "Key parser" should {
    "parse property key" in {
      val property = "d:/internet/sites/us/sewse/jabber/comment2.jse"
      val result = "d:/internet/sites/us/sewse/jabber/comment2.jse"

      parsers.key must succeedOn(property).withResult(result)
    }
  }

  "Value parser" should {
    "parse string value" in {
      val property = "\"string\""
      val result = StringValue("string")

      parsers.stringValue must succeedOn(property).withResult(result)
    }

    "parse number value" in {
      val property = "42"
      val result = NumberValue("42")

      parsers.numberValue must succeedOn(property).withResult(result)
    }

    "parse regex value" in {
      val property = "\"\"\".*\"\"\""
      val result = RegexValue("\"\"\".*\"\"\"")

      parsers.regexValue must succeedOn(property).withResult(result)
    }

    "parse object value" in {
      val property = "{}"
      val result = Object()

      parsers.objectValue must succeedOn(property).withResult(result)
    }

    "parse object value with nested object properties" in {
      val property = "{" +
        "A {}" + LB +
        "B {" + LB +
          "C {}" + LB +
        "}" + LB +
      "}"
      val result = Object(List(
        Property("A", Object()),
        Property("B", Object(List(
          Property("C", Object())
        )))
      ))

      parsers.objectValue must succeedOn(property).withResult(result)
    }

    "parse date value" in {
      val property = "\"2014-10-16T03:33:49+02:00\""
      val result = DateValue("2014-10-16T03:33:49+02:00")

      parsers.dateValue must succeedOn(property).withResult(result)
    }
  }

  "Property parser" should {
    "parse key with string value" in {
      val property = "prop = \"string\""
      val result = Property("prop", StringValue("string"))

      parsers.property must succeedOn(property).withResult(result)
    }

    "parse key with regex value" in {
      val property = "prop = \"\"\".*\"\"\""
      val result = Property("prop", RegexValue("\"\"\".*\"\"\""))

      parsers.property must succeedOn(property).withResult(result)
    }

    "parse key with number value" in {
      val property = "prop = 42"
      val result = Property("prop", NumberValue("42"))

      parsers.property must succeedOn(property).withResult(result)
    }

    "parse key with date value" in {
      val property = "date = \"2014-10-16T03:33:49+02:00\""
      val result = Property("date", DateValue("2014-10-16T03:33:49+02:00"))

      parsers.property must succeedOn(property).withResult(result)
    }

    "parse key with object value" in {
      val property = "prop {}"
      val result = Property("prop", Object())

      parsers.property must succeedOn(property).withResult(result)
    }
  }


  "Request parser" should {
    "parse empty body" in {
      val property = "request {}"
      val result = Request(Object())

      parsers.request must succeedOn(property).withResult(result)
    }

    "parse body with subsections" in {
      val property = "request {" +
        "date = \"2014-10-16T03:33:49+02:00\"" + LB +
        "method = \"POST\"" + LB +
        "cookies {}" + LB +
        "url {}" + LB +
        "protocol = \"HTTP/1.1\"" + LB +
        "headers {}" + LB +
      "}"
      val result = Request(Object(List(
        Property("date", DateValue("2014-10-16T03:33:49+02:00")),
        Property("method", StringValue("POST")),
        Property("cookies", Object()),
        Property("url", Object()),
        Property("protocol", StringValue("HTTP/1.1")),
        Property("headers", Object())
      )))

      parsers.request must succeedOn(property).withResult(result)
    }
  }

  "Response parser" should {
    "parse empty body" in {
      val property = "response {}"
      val result = Response(Object())

      parsers.response must succeedOn(property).withResult(result)
    }

    "parse body with subsections" in {
      val property = "response {" +
        "status = 200" + LB +
        "bytes-sent = 45053" + LB +
        "processing-time = 0.228" + LB +
        "headers {}" + LB +
      "}"
      val result = Response(Object(List(
        Property("status", NumberValue("200")),
        Property("bytes-sent", NumberValue("45053")),
        Property("processing-time", NumberValue("0.228")),
        Property("headers", Object())
      )))

      parsers.response must succeedOn(property).withResult(result)
    }
  }

  "Variable parser" should {
    "parse empty request and response" in {
      val property = "name = request {} => response {}"
      val result = Variable("name", Request(Object()), Response(Object()))

      parsers.variable must succeedOn(property).withResult(result)
    }
  }

}
