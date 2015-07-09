package elasticsearch.ast

sealed trait RangeFilter

class DateRangeFilter(from: Option[String], till: Option[String]) extends RangeFilter {
  private val gte: String = from match {
    case Some(f) => "\"gte\" : \"" + f + "\""
    case None => ""
  }
  private val lte: String = till match {
    case Some(t) => "\"lte\" : \"" + t + "\""
    case None => ""
  }
  private val format = "\"format\" : \"dd-MM-yyyy\""

  override def toString: String = {
    val filterProperties: List[String] = List(gte, lte, format)
    if (from.isDefined || till.isDefined) {
      "{ \"range\" : { \"request.timestamp\" : { " + filterProperties.filterNot(_.forall(_.equals(""))).mkString(",") + " } } }"
    } else {
      ""
    }
  }
}
