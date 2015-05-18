package csl.ast

sealed trait Value
case class StringValue(v: String) extends Value {
  override def toString: String = "\"" + v + "\""
}
case class NumberValue(v: String) extends Value
case class DateValue(v: String) extends Value
case class RegexValue(v: String) extends Value
case class Object(ps: List[Property] = List.empty) extends Value
