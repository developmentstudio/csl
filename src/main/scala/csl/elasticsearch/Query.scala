package csl.elasticsearch

import csl.ast._

case class Match(prefix: String, p: Property) {
  def field(prefix: String, p: Property) = "\"" + prefix + p.key + "\"" + ":" + value(p.value)
  def value(v: Value): String = v match {
    case v: StringValue => v.toString
    case v: NumberValue => v.toString
    case v: DateValue => v.toString
    case v: RegexValue => throw new Exception("Regex Value is not implemented")
    case ObjectValue(_) => throw new Exception("Object Value is not implemented")
  }
  override def toString: String = "{ \"match\": {" + field(prefix, p) + "} }"
}

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
