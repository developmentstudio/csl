package elasticsearch

import csl.ast.Property
import elasticsearch.ast.Filter

class FilterQueryGenerator(from: Option[String] = None, till: Option[String] = None)
{
  def generate(properties: List[Property]): Query = {
    val filters = properties map(p => Filter(p))
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

}
