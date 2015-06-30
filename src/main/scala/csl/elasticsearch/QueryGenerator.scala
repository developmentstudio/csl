package csl.elasticsearch

import csl.ast.Property
import csl.elasticsearch.ast.Filter

class FilterQueryGenerator
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
