package csl.ast

import csl.typechecker.Error

import scala.util.parsing.input.Positional

sealed trait DetectorElement extends Positional

case class Detector(label: String, body: List[DetectorElement]) extends Positional {
  def variables: List[Variable] = body.collect{ case v: Variable => v }
  def find: Find = body.collectFirst{ case f: Find => f } match {
    case Some(f) => f
    case None => throw new Exception("Typechecker failed on checking 'Find'.")
  }
}

case class Variable(name: String, request: ObjectValue, response: ObjectValue) extends DetectorElement
case class Find(pattern: Pattern) extends DetectorElement

case class Pattern(variables: List[String], relationKeys: List[String]) extends Positional
