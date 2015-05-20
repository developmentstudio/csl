package csl.elasticsearch

import csl.ast._

class FilterQueryGenerator {

    type Query = String

    def generateFilterQuery(v: Variable): Query = {
      """{ "filter": { "bool": { "must": [""" +
        (generateFilterQuery(v.req) ++ generateFilterQuery(v.res)).map(_.toString).mkString(",") +
        """] } } }"""
    }

    def generateFilterQuery(req: Request): List[Filter] = {
      req.o.ps.flatMap(p => generateFilterQuery(p, "request."))
    }

    def generateFilterQuery(res: Response): List[Filter] = {
      res.o.ps.flatMap(p => generateFilterQuery(p, "response."))
    }

    def generateFilterQuery(p: Property, prefix: String): List[Filter] = {

      p.value match {
        case StringValue(_) => List(Filter(prefix, p))
        case NumberValue(_) => List(Filter(prefix, p))
        case DateValue(_) => List(Filter(prefix, p))
        case RegexValue(_) => List(Filter(prefix, p))
        case Object(ps) => {
          val _prefix = prefix + p.key + "."
          ps.flatMap(p => generateFilterQuery(p, _prefix))
        };
      }
    }

}
