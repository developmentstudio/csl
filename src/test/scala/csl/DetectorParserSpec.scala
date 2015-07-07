package csl

import csl.ast._
import csl.parser.DetectorParser
import org.specs2.matcher.ParserMatchers
import org.specs2.mutable.Specification

class DetectorParserSpec extends Specification with ParserMatchers {

  val parsers = new DetectorParser

  val LB: String = "\n"

  "patternBlock parser" should {
    "parse block with variable names" in {
      val property = "pattern { A -> C -> N }"
      val result = Pattern(List(Identifier("A"), Identifier("C"), Identifier("N")))

      parsers.pattern must succeedOn(property).withResult(result)
    }
  }

  "relationBlock parser" should {
    "parse block with property keys" in {
      val property = "with relation on { A.B.C and C and N }"
      val result = Relation(List("A.B.C", "C", "N"))

      parsers.relation must succeedOn(property).withResult(result)
    }

    "parse block empty block" in {
      val property = "with relation on { }"
      val result = Relation(List.empty)

      parsers.relation must succeedOn(property).withResult(result)
    }
  }

  "helper parser" should {
    "parse 'in' helper" in {
      val property = "in(A, B, C)"
      val result = In(List(Identifier("A"), Identifier("B"), Identifier("C")))

      parsers.in must succeedOn(property).withResult(result)
    }

    "parse 'not' helper" in {
      val property = "not(A, B, C)"
      val result = Not(List(Identifier("A"), Identifier("B"), Identifier("C")))

      parsers.not must succeedOn(property).withResult(result)
    }

    "parse 'repeat' helper" in {
      val property = "repeat(A, 5)"
      val result = Repeat(Identifier("A"), 5)

      parsers.repeat must succeedOn(property).withResult(result)
    }

    "parse wildcard '?' helper" in {
      val property = "?"
      val result = SingleWildcard()

      parsers.wildcard must succeedOn(property).withResult(result)
    }

    "parse wildcard '*' helper" in {
      val property = "*"
      val result = MultiWildcard()

      parsers.wildcard must succeedOn(property).withResult(result)
    }

  }

  "find parser" should {
    "parse find description" in {
      val property = "find { pattern { } with relation on { } }"
      val result = Find(Pattern(List.empty), Relation(List.empty))

      parsers.find must succeedOn(property).withResult(result)
    }
  }

  "detector parser" should {
    "parse empty body" in {
      val property = "detector \"LABEL\" { find { pattern { } with relation on { } } }"
      val result = Detector("LABEL", List(Find(Pattern(List.empty), Relation(List.empty))))

      parsers.detector must succeedOn(property).withResult(result)
    }
  }

  "csv parser" should {
    "parse csv definition" in {
      val property = "csv {" +
        "request.remoteAddress," +
        "response.status" +
        "}"
      val result = CsvFile(List("request.remoteAddress", "response.status"))

      parsers.csv must succeedOn(property).withResult(result)
    }
  }

  "result parser" should {
    "parse result block" in {
      val property = "result {" +
        "csv {" +
          "request.remoteAddress," +
          "response.status" +
        "}" +
      "}"
      val result = Result(CsvFile(List("request.remoteAddress", "response.status")))

      parsers.result must succeedOn(property).withResult(result)
    }
  }

}
