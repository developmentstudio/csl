package csl.ast

import scala.util.parsing.input.Positional

case class Property(key: String, value: Value) extends Positional
