package csl.parser

import csl.ast._
import org.specs2.matcher.ParserMatchers
import org.specs2.mutable.Specification

import scala.util.matching.Regex


class ParserSpec extends Specification with ParserMatchers {

  val parsers = new Parser

  val LineBreak: String = "\n"

  "Property Key parser" should {
    "parse property key" in {
      val property = "d:/internet/sites/us/sewse/jabber/comment2.jse"
      val result = "d:/internet/sites/us/sewse/jabber/comment2.jse"

      parsers.key must succeedOn(property).withResult(result)
    }
  }

  "Value parser" should {
    "parse string value" in {
      val property = "\"string\""
      val result = StringValue("\"string\"")

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
  }

  "Protocol parser" should {
    "parse string value" in {
      val property = "protocol = \"string\""
      val result = Protocol(StringValue("\"string\""))

      parsers.protocol must succeedOn(property).withResult(result)
    }

    "parse number value" in {
      val property = "protocol = 42"
      val result = Protocol(NumberValue("42"))

      parsers.protocol must succeedOn(property).withResult(result)
    }

    "parse regex value" in {
      val property = "protocol = \"\"\".*\"\"\""
      val result = Protocol(RegexValue("\"\"\".*\"\"\""))

      parsers.protocol must succeedOn(property).withResult(result)
    }
  }

  "Method parser" should {
    "parse string value" in {
      val property = "method = \"string\""
      val result = Method(StringValue("\"string\""))

      parsers.method must succeedOn(property).withResult(result)
    }

    "parse number value" in {
      val property = "method = 42"
      val result = Method(NumberValue("42"))

      parsers.method must succeedOn(property).withResult(result)
    }

    "parse regex value" in {
      val property = "method = \"\"\".*\"\"\""
      val result = Method(RegexValue("\"\"\".*\"\"\""))

      parsers.method must succeedOn(property).withResult(result)
    }
  }

  "RemoteAddress parser" should {
    "parse string value" in {
      val property = "remote-address = \"string\""
      val result = RemoteAddress(StringValue("\"string\""))

      parsers.remoteAddress must succeedOn(property).withResult(result)
    }

    "parse number value" in {
      val property = "remote-address = 42"
      val result = RemoteAddress(NumberValue("42"))

      parsers.remoteAddress must succeedOn(property).withResult(result)
    }

    "parse regex value" in {
      val property = "remote-address = \"\"\".*\"\"\""
      val result = RemoteAddress(RegexValue("\"\"\".*\"\"\""))

      parsers.remoteAddress must succeedOn(property).withResult(result)
    }
  }

  "Host parser" should {
    "parse string value" in {
      val property = "host = \"string\""
      val result = Host(StringValue("\"string\""))

      parsers.host must succeedOn(property).withResult(result)
    }

    "parse number value" in {
      val property = "host = 42"
      val result = Host(NumberValue("42"))

      parsers.host must succeedOn(property).withResult(result)
    }

    "parse regex value" in {
      val property = "host = \"\"\".*\"\"\""
      val result = Host(RegexValue("\"\"\".*\"\"\""))

      parsers.host must succeedOn(property).withResult(result)
    }
  }

  "Uri parser" should {
    "parse string value" in {
      val property = "uri = \"string\""
      val result = Uri(StringValue("\"string\""))

      parsers.uri must succeedOn(property).withResult(result)
    }

    "parse number value" in {
      val property = "uri = 42"
      val result = Uri(NumberValue("42"))

      parsers.uri must succeedOn(property).withResult(result)
    }

    "parse regex value" in {
      val property = "uri = \"\"\".*\"\"\""
      val result = Uri(RegexValue("\"\"\".*\"\"\""))

      parsers.uri must succeedOn(property).withResult(result)
    }
  }

  "Parameter parser" should {
    "parse query key with string value" in {
      val property = "key = \"string\""
      val result = Parameter("key", StringValue("\"string\""))

      parsers.parameter must succeedOn(property).withResult(result)
    }

    "parse query key with number value" in {
      val property = "key = 42"
      val result = Parameter("key", NumberValue("42"))

      parsers.parameter must succeedOn(property).withResult(result)
    }

    "parse query key with regex value" in {
      val property = "key = \"\"\".*\"\"\""
      val result = Parameter("key", RegexValue("\"\"\".*\"\"\""))

      parsers.parameter must succeedOn(property).withResult(result)
    }
  }

  "Query parser" should {
    "parse empty query section" in {
      val property = "query {}"
      val result = Query(List())

      parsers.query must succeedOn(property).withResult(result)
    }

    "parse query section with single parameter" in {
      val property = "query {" +
        "key = 42" +
      "}"
      val result = Query(List(Parameter("key", NumberValue("42"))))

      parsers.query must succeedOn(property).withResult(result)
    }

    "parse query section with multiple parameters" in {
      val property = "query {" +
        "key = 42" +
        "key = \"\"\".*\"\"\"" +
      "}"
      val result = Query(List(Parameter("key", NumberValue("42")), Parameter("key", RegexValue("\"\"\".*\"\"\""))))

      parsers.query must succeedOn(property).withResult(result)
    }
  }

  "Url parser" should {
    "parse empty url section" in {
      val property = "url {}"
      val result = Url(List())

      parsers.url must succeedOn(property).withResult(result)
    }

    "parse url section with single property" in {
      val property = "url {" +
        "host = \"www.DOMEIN.nl\"" +
      "}"
      val result = Url(List(Host(StringValue("\"www.DOMEIN.nl\""))))

      parsers.url must succeedOn(property).withResult(result)
    }

    "parse url section with multiple properties" in {
      val property = "url {" + LineBreak +
        "host = \"www.DOMEIN.nl\"" + LineBreak +
        "uri = \"/\"" + LineBreak +
        "query {}" + LineBreak +
      "}"
      val result = Url(List(
        Host(StringValue("\"www.DOMEIN.nl\"")),
        Uri(StringValue("\"/\"")),
        Query(List())
      ))

      parsers.url must succeedOn(property).withResult(result)
    }
  }



}
