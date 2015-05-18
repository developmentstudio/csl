package csl.elasticsearch

import csl.ast._

case class Match(prefix: String, p: Property) {
  def field(prefix: String, p: Property) = "\"" + prefix + p.key + "\"" + ":" + value(p.value)
  def value(v: Value): String = v match {
    case v: StringValue => v.toString
    case v: NumberValue => v.toString
    case v: DateValue => v.toString
    case v: RegexValue => throw new Exception("Regex Value is not implemented")
    case Object(_) => throw new Exception("Object Value is not implemented")
  }
  override def toString: String = "{ \"match\": {" + field(prefix, p) + "} }"
}
