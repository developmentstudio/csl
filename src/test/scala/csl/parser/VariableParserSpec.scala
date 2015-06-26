package csl.parser

import csl.ast._
import org.specs2.matcher.ParserMatchers
import org.specs2.mutable.Specification

class VariableParserSpec extends Specification with ParserMatchers {

  val parsers = new DetectorParser

  val LB: String = "\n"

  "Key parser" should {
    "parse property key" in {
      val property = "A.B.C.D.E-7.F.G"
      val result = "A.B.C.D.E-7.F.G"

      parsers.propertyKey must succeedOn(property).withResult(result)
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
      val result = NumberValue(42)

      parsers.numberValue must succeedOn(property).withResult(result)
    }

    "parse regex value" in {
      val property = "\"\"\".*\"\"\""
      val result = RegexValue(".*")

      parsers.regexValue must succeedOn(property).withResult(result)
    }

    "parse object value" in {
      val property = "{}"
      val result = ObjectValue()

      parsers.objectValue must succeedOn(property).withResult(result)
    }

    "parse object value with nested object properties" in {
      val property = "{" +
        "A {}" + LB +
        "B {" + LB +
          "C {}" + LB +
        "}" + LB +
      "}"
      val result = ObjectValue(List(
        Property("A", ObjectValue()),
        Property("B", ObjectValue(List(
          Property("C", ObjectValue())
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
      val property = "prop: \"string\""
      val result = Property("prop", StringValue("string"))

      parsers.property must succeedOn(property).withResult(result)
    }

    "parse key with regex value" in {
      val property = "prop: \"\"\".*\"\"\""
      val result = Property("prop", RegexValue(".*"))

      parsers.property must succeedOn(property).withResult(result)
    }

    "parse key with number value" in {
      val property = "prop: 42"
      val result = Property("prop", NumberValue(42))

      parsers.property must succeedOn(property).withResult(result)
    }

    "parse key with date value" in {
      val property = "date: \"2014-10-16T03:33:49+02:00\""
      val result = Property("date", DateValue("2014-10-16T03:33:49+02:00"))

      parsers.property must succeedOn(property).withResult(result)
    }

    "parse key with object value" in {
      val property = "prop {}"
      val result = Property("prop", ObjectValue())

      parsers.property must succeedOn(property).withResult(result)
    }
  }


  "Request parser" should {
    "parse empty body" in {
      val property = "request {}"
      val result = ObjectValue()

      parsers.request must succeedOn(property).withResult(result)
    }

    "parse body with subsections" in {
      val property = "request {" +
        "date: \"2014-10-16T03:33:49+02:00\"" + LB +
        "method: \"POST\"" + LB +
        "cookies {}" + LB +
        "url {}" + LB +
        "protocol: \"HTTP/1.1\"" + LB +
        "headers {}" + LB +
      "}"
      val result = ObjectValue(List(
        Property("date", DateValue("2014-10-16T03:33:49+02:00")),
        Property("method", StringValue("POST")),
        Property("cookies", ObjectValue()),
        Property("url", ObjectValue()),
        Property("protocol", StringValue("HTTP/1.1")),
        Property("headers", ObjectValue())
      ))

      parsers.request must succeedOn(property).withResult(result)
    }
  }

  "Response parser" should {
    "parse empty body" in {
      val property = "response {}"
      val result = ObjectValue()

      parsers.response must succeedOn(property).withResult(result)
    }

    "parse body with subsections" in {
      val property = "response {" +
        "status: 200" + LB +
        "bytes-sent: 45053" + LB +
        "processing-time: 0.228" + LB +
        "headers {}" + LB +
      "}"
      val result = ObjectValue(List(
        Property("status", NumberValue(200)),
        Property("bytes-sent", NumberValue(45053)),
        Property("processing-time", NumberValue(0.228)),
        Property("headers", ObjectValue())
      ))

      parsers.response must succeedOn(property).withResult(result)
    }
  }

  "Variable parser" should {
    "parse empty request and response" in {
      val property = "name = request {} => response {}"
      val result = RequestDefinition("name", ObjectValue(), ObjectValue())

      parsers.variable must succeedOn(property).withResult(result)
    }
  }

}
