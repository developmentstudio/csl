package csl.ast

case class Property(key: String, value: Value)

sealed trait Value
case class StringValue(v: String) extends Value
case class NumberValue(v: String) extends Value
case class RegexValue(v: String) extends Value
