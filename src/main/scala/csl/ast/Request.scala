package csl.ast

//sealed trait RequestSection

case class Protocol(value: Value)
case class Method(value: Value)
case class RemoteAddress(value: Value)
case class Url(sections: List[UrlSection])
case class Headers(properties: List[Property])
case class Cookies(properties: List[Property])

sealed trait UrlSection
case class Host(value: Value) extends UrlSection
case class Uri(value: Value)  extends UrlSection
case class Query(parameters: List[Property])  extends UrlSection
