package csl.elasticsearch

import csl.ast._
import csl.elasticsearch.ast.{Match, Filter}

sealed trait QueryGenerator {
  def generate(v: Variable): String
}


// TODO: MatchQueryGenerator is not used so far.
class MatchQueryGenerator extends QueryGenerator {

  def generate(v: Variable): Query = {
    """{
      |   "query": {
      |     "bool": {
      |       "must": [
    """.stripMargin +
      (generate(v.request, "request.") ++ generate(v.response, "response.")).map(_.toString).mkString(",") +
    """       ]
      |     }
      |   },
      |   "size": 100
      |}""".stripMargin
  }

  private def generate(value: ObjectValue, prefix: String = ""): List[Match] = {
    value.properties.flatMap(p => generate(p, prefix))
  }

  private def generate(p: Property, prefix: String): List[Match] = {
    p.value match {
      case StringValue(_) => List(Match(prefix, p))
      case NumberValue(_) => List(Match(prefix, p))
      case DateValue(_) => List(Match(prefix, p))
      case RegexValue(_) => List(Match(prefix, p))
      case ObjectValue(ps) => ps.flatMap(p => generate(p, prefix + p.key + "."))
    }
  }

}

class FilterQueryGenerator extends QueryGenerator{

  def generate(v: Variable): Query = {
    """{
      |   "filter": {
      |     "bool": {
      |       "must": [
    """.stripMargin +
      (generate(v.request, "request.") ++ generate(v.response, "response.")).map(_.toString).mkString(",") +
    """       ]
      |     }
      |   },
      |   "size": 100
      |}
    """.stripMargin
  }

  private def generate(value: ObjectValue, prefix: String = ""): List[Filter] = {
    value.properties.flatMap(p => generate(p, prefix))
  }

  private def generate(p: Property, prefix: String): List[Filter] = {
    p.value match {
      case StringValue(_) => List(Filter(prefix, p))
      case NumberValue(_) => List(Filter(prefix, p))
      case DateValue(_) => List(Filter(prefix, p))
      case RegexValue(_) => List(Filter(prefix, p))
      case ObjectValue(ps) => ps.flatMap(p => generate(p, prefix + p.key + "."))
    }
  }

}
