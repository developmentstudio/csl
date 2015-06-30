package csl.elasticsearch.ast

import csl.ast.{DateValue, NumberValue, ObjectValue, Property, RegexValue, StringValue}

case class Filter(p: Property) {

  private val field: String = "\"" + this.p.key + "\": " + p.value.toString

  override def toString: String = p.value match {
    case StringValue(_) => "{ \"term\": {" + field + "} }"
    case NumberValue(_) => "{ \"term\": {" + field + "} }"
    case DateValue(_) => "{ \"term\": {" + field + "} }"
    case RegexValue(_) => "{ \"regexp\": {" + field + "} }"
    case ObjectValue(_) => throw new Exception("Object Value is not implemented")
  }
}
