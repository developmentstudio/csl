package csl.parser

import csl.ast._
import org.specs2.matcher.ParserMatchers
import org.specs2.mutable.Specification

class DetectorParserSpec extends Specification with ParserMatchers {

  val parsers = new DetectorParser

  val LB: String = "\n"

  "patternBlock parser" should {
    "parse block with variable names" in {
      val property = "pattern { A -> C -> N }"
      val result = List("A", "C", "N")

      parsers.patternDescription must succeedOn(property).withResult(result)
    }
  }

  "relationBlock parser" should {
    "parse block with property keys" in {
      val property = "with relation on { A.B.C and C and N }"
      val result = List("A.B.C", "C", "N")

      parsers.relationDescription must succeedOn(property).withResult(result)
    }

    "parse block empty block" in {
      val property = "with relation on { }"
      val result = List("")

      parsers.relationDescription must succeedOn(property).withResult(result)
    }
  }

  "pattern parser" should {
    "parse pattern description" in {
      val property = "pattern { A -> C -> N } with relation on { A.B.C and C and N }"
      val result = Pattern(List("A", "C", "N"), List("A.B.C", "C", "N"))

      parsers.pattern must succeedOn(property).withResult(result)
    }
  }

  "find parser" should {
    "parse find description" in {
      val property = "find { pattern { } with relation on { } }"
      val result = Find(Pattern(List.empty, List.empty))

      parsers.find must succeedOn(property).withResult(result)
    }
  }

  "detector parser" should {
    "parse empty body" in {
      val property = "detector \"LABEL\" { find { pattern { } with relation on { } } }"
      val result = Detector("LABEL", List(Find(Pattern(List.empty, List.empty))))

      parsers.detector must succeedOn(property).withResult(result)
    }
  }

}
