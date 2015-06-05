package csl.ast

import scala.util.parsing.input.Positional

sealed trait DetectorElement extends Positional

case class Detector(label: String, body: List[DetectorElement]) extends Positional

case class Variable(name: String, request: ObjectValue, response: ObjectValue) extends DetectorElement
case class Find(pattern: Pattern) extends DetectorElement

case class Pattern(variables: List[String], relations: List[String]) extends Positional
