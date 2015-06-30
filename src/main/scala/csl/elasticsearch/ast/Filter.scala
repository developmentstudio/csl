package csl.elasticsearch.ast

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
