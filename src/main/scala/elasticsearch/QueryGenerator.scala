package elasticsearch

import csl.ast.Property
import elasticsearch.ast.{DateRangeFilter, Filter}

class FilterQueryGenerator(from: Option[String] = None, till: Option[String] = None) {
  def generate(properties: List[Property]): Query = {
    val filters = properties map (p => Filter(p))
    var jsonFilter = filters.map(_.toString).mkString(",")
    if (from.isDefined || till.isDefined) {
      jsonFilter = jsonFilter + "," + DateRangeFilter(from, till).toString
    }

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
