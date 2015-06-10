package csl.elasticsearch.ast

import csl.ast.{DateValue, NumberValue, ObjectValue, Property, RegexValue, StringValue, Value}

case class Filter(prefix: String, p: Property) {
  def field(prefix: String, p: Property) = "\"" + prefix + p.key + "\"" + ":" + value(p.value)
  def value(v: Value): String = v.toString
  override def toString: String = p.value match {
    case StringValue(_) => "{ \"term\": {" + field(prefix, p) + "} }"
    case NumberValue(_) => "{ \"term\": {" + field(prefix, p) + "} }"
    case DateValue(_) => "{ \"term\": {" + field(prefix, p) + "} }"
    case RegexValue(_) => "{ \"regexp\": {" + field(prefix, p) + "} }"
    case ObjectValue(_) => throw new Exception("Object Value is not implemented")
  }
}
