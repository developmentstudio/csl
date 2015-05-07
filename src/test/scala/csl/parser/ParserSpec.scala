package csl.parser

import csl.ast._
import org.specs2.matcher.ParserMatchers
import org.specs2.mutable.Specification

import scala.util.matching.Regex


class ParserSpec extends Specification with ParserMatchers {

  val parsers = new Parser

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
      val result = StringValue("\"string\"")

      parsers.protocol must succeedOn(property).withResult(result)
    }

    "parse number value" in {
      val property = "protocol = 42"
      val result = NumberValue("42")

      parsers.protocol must succeedOn(property).withResult(result)
    }

    "parse regex value" in {
      val property = "protocol = \"\"\".*\"\"\""
      val result = RegexValue("\"\"\".*\"\"\"")

      parsers.protocol must succeedOn(property).withResult(result)
    }
  }

}
