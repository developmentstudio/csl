package elasticsearch.collector

import csl.ast._
import elasticsearch._
import elasticsearch.ast.{DateRangeFilter, Filter, RegexFilter, TermFilter}

class FilterQueryGenerator(from: Option[String] = None, till: Option[String] = None) {
  def generate(properties: List[Property]): Query = {
    val filters = properties map (createFilter)
    var jsonFilter = filters.map(_.toString).mkString(",")
    if (from.isDefined || till.isDefined) {
      jsonFilter = jsonFilter + "," + new DateRangeFilter(from, till).toString
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

  private def createFilter(property: Property): Filter = {
    property.value match {
      case StringValue(_) => new TermFilter(property)
      case NumberValue(_) => new TermFilter(property)
      case DateValue(_) => new TermFilter(property)
      case RegexValue(_) => new RegexFilter(property)
      case ObjectValue(_) => throw new Exception("No filter implemented for 'Object Value'.")
    }
  }

}
