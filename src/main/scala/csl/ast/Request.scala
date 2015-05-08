package csl.ast

case class Protocol(value: Value)
case class Method(value: Value)
case class RemoteAddress(value: Value)
case class Url(properties: List[UrlProperty])

sealed trait UrlProperty
case class Host(value: Value) extends UrlProperty
case class Uri(value: Value) extends UrlProperty
case class Query(parameters: List[Parameter]) extends UrlProperty
case class Parameter(key: String, value: Value)
