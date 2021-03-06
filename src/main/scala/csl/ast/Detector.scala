package csl.ast

import scala.util.parsing.input.Positional

sealed trait DetectorElement extends Positional

case class Detector(label: String, body: List[DetectorElement]) extends Positional {
  def definitions: List[RequestDefinition] = body.collect { case v: RequestDefinition => v }

  def definition(name: String): Option[RequestDefinition] = definitions.find({
    case rd: RequestDefinition if rd.name == name => true
    case _ => false
  })

  def find: Find = body.collectFirst { case f: Find => f } match {
    case Some(f) => f
    case None => throw new Exception("Typechecker failed on checking 'Find'.")
  }

  def result: Result = body.collectFirst { case r: Result => r } match {
    case Some(r) => r
    case None => throw new Exception("Typechecker failed on checking 'Result'.")
  }
}

case class RequestDefinition(name: String, request: ObjectValue, response: ObjectValue) extends DetectorElement {
  val properties: List[Property] = flatten(request, "request.") ++ flatten(response, "response.")

  private def flatten(value: ObjectValue, prefix: String = ""): List[Property] = {
    value.properties.flatMap(p => flatten(p, prefix))
  }

  private def flatten(p: Property, prefix: String): List[Property] = {
    p.value match {
      case StringValue(_) => List(Property(prefix + p.key, p.value))
      case NumberValue(_) => List(Property(prefix + p.key, p.value))
      case DateValue(_) => List(Property(prefix + p.key, p.value))
      case RegexValue(_) => List(Property(prefix + p.key, p.value))
      case ObjectValue(ps) => ps.flatMap(p => flatten(p, prefix + p.key + "."))
    }
  }
}

case class Find(pattern: Pattern, relation: Relation, from: Option[String], till: Option[String], timesBeforeMatch: Option[TimesBeforeMatch], interval: Option[Interval]) extends DetectorElement

case class Pattern(elements: List[PatternElement]) extends Positional {
  def isDefined: Boolean = elements.nonEmpty

  def hasMoreThanOneElement: Boolean = elements.length > 1 || hasMultiWildcard || hasRepeatLongerThanOne

  private def hasRepeatLongerThanOne: Boolean = {
    elements.exists({
      case Repeat(_, times) if times > 1 => true
      case _ => false
    })
  }

  private def hasMultiWildcard: Boolean = {
    elements.exists({
      case _: MultiWildcard => true
      case _ => false
    })
  }

  def getRequestDefinitionIdentifiers: List[Identifier] = {
    var identifiers: List[Identifier] = List.empty
    elements.foreach({
      case id: Identifier => identifiers = identifiers :+ id
      case In(ids) => identifiers = identifiers ::: ids
      case Not(_) =>
      case Repeat(id, _) => identifiers = identifiers :+ id
      case SingleWildcard() =>
      case MultiWildcard() =>
    })
    identifiers
  }
}

case class Relation(keys: List[String]) extends Positional {
  def isDefined: Boolean = keys.nonEmpty
}

sealed trait TimesBeforeMatchOperator
case class BiggerThanOperator() extends TimesBeforeMatchOperator
case class SmallerThanOperator() extends TimesBeforeMatchOperator

case class TimesBeforeMatch(times: Int, operator: Option[TimesBeforeMatchOperator]) extends Positional

sealed trait IntervalUnit {
  val inMillis: Long = 0
}
case class IntervalInSeconds() extends IntervalUnit {
  override val inMillis: Long = 1000
}
case class IntervalInMinutes() extends IntervalUnit {
  override val inMillis: Long = 60000
}
case class IntervalInHours() extends IntervalUnit {
  override val inMillis: Long = 3600000
}
case class IntervalInDays() extends IntervalUnit {
  override val inMillis: Long = 86400000
}

case class Interval(number: Int, unit: IntervalUnit) {
  def inMillis: Long = number * unit.inMillis
}

sealed trait PatternElement extends Positional

case class Identifier(name: String) extends PatternElement {
  override def toString: String = name
}

case class In(identifiers: List[Identifier]) extends PatternElement {
  override def toString: String = "in(" + identifiers.mkString(", ") + ")"
}

case class Not(identifiers: List[Identifier]) extends PatternElement {
  override def toString: String = "not(" + identifiers.mkString(", ") + ")"
}

case class Repeat(identifier: Identifier, times: Int) extends PatternElement {
  override def toString: String = "repeat(" + identifier + ", " + times + ")"
}

sealed trait Wildcard extends PatternElement

case class SingleWildcard() extends PatternElement with Wildcard {
  override def toString: String = "?"
}

case class MultiWildcard() extends PatternElement with Wildcard {
  override def toString: String = "*"
}


case class Result(export: ExportType) extends DetectorElement

sealed trait ExportType

case class CsvFile(keys: List[String] = List()) extends ExportType
