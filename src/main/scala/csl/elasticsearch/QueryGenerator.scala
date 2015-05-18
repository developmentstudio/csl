package csl.search

import csl.elasticsearch.Match
import csl.ast._

class QueryGenerator {

  type Query = String

  def generateMatchQuery(v: Variable): Query = {
    """{ "query": { "bool": { "must": [""" +
      (generateMatchQuery(v.req) ++ generateMatchQuery(v.res)).map(_.toString).mkString(",") +
    """] } } }"""
  }

  def generateMatchQuery(req: Request): List[Match] = {
    req.o.ps.flatMap(p => generateMatchQuery(p, "request."))
  }

  def generateMatchQuery(res: Response): List[Match] = {
    res.o.ps.flatMap(p => generateMatchQuery(p, "response."))
  }

  def generateMatchQuery(p: Property, prefix: String): List[Match] = {

    p.value match {
      case StringValue(_) => List(Match(prefix, p))
      case NumberValue(_) => List(Match(prefix, p))
      case DateValue(_) => List(Match(prefix, p))
      case RegexValue(_) => List(Match(prefix, p))
      case Object(ps) => {
        val _prefix = prefix + p.key + "."
        ps.flatMap(p => generateMatchQuery(p, _prefix))
      };
    }
  }

}
