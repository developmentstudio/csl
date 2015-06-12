package csl.elasticsearch

import csl.ast._
import csl.elasticsearch.ast.Filter

sealed trait QueryGenerator {
  def generate(v: Variable): String
}

class FilterQueryGenerator extends QueryGenerator
{
  def generate(v: Variable): Query = {
    val filters = generate(v.properties)
    val jsonFilter = filters.map(_.toString).mkString(",")

    """{
      |   "filter": {
      |     "bool": {
      |       "must": [
    """.stripMargin + jsonFilter +
    """       ]
      |     }
      |   },
      |   "size": 100
      |}
    """.stripMargin
  }

  private def generate(properties: List[Property]): List[Filter] = properties map(createFilter)

  private def createFilter(p: Property): Filter = Filter(p)
}
