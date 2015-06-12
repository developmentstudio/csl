package csl.elasticsearch

import csl.ast._
import csl.elasticsearch.ast.Filter

sealed trait QueryGenerator {
  def generate(properties: List[Property]): String
}

class FilterQueryGenerator extends QueryGenerator
{
  def generate(properties: List[Property]): Query = {
    val filters = properties map(createFilter)
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

  private def createFilter(p: Property): Filter = Filter(p)
}
