package elasticsearch.ast

import csl.ast.{DateValue, NumberValue, ObjectValue, Property, RegexValue, StringValue}

case class Filter(p: Property) {

  private val field: String = "\"" + this.p.key + "\": " + p.value.toString

  private def term(field: String): String = "{ \"term\": {" + field + "} }"

  private def regex(field: String): String = "{ \"regexp\": {" + field + "} }"

  override def toString: String = p.value match {
    case StringValue(_) => term(this.field)
    case NumberValue(_) => term(this.field)
    case DateValue(_) => term(this.field)
    case RegexValue(_) => regex(this.field)
    case ObjectValue(_) => throw new Exception("Object Value is not implemented")
  }
}

case class DateRangeFilter(from: Option[String], till: Option[String]) {
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
