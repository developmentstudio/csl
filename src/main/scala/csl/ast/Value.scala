package csl.ast

import scala.util.parsing.input.Positional

sealed trait Value extends Positional
case class StringValue(value: String) extends Value {
  override def toString: String = "\"" + value + "\""
}
case class NumberValue(value: Double) extends Value {
  override def toString: String = value.toString
}
case class DateValue(value: String) extends Value {
  override def toString: String = "\"" + value + "\""
}
case class RegexValue(value: String) extends Value {
  override def toString: String = "\"" + value + "\""
}
case class ObjectValue(properties: List[Property] = List.empty) extends Value
