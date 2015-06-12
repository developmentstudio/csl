package csl.elasticsearch.parser

import csl.ast.{Property, StringValue}
import csl.elasticsearch.ast.Relation
import org.json4s.DefaultFormats
import org.json4s.jackson.JsonMethods.parse

object RelationParser
{
  implicit val formats = DefaultFormats

  def parseJSON(json: String): Relation = {
    val parsedJson = parse(json).extract[Map[String, Any]]
    val properties = for ((key, value) <- parsedJson) yield Property(key, StringValue(value.toString))
    Relation(properties.toList)
  }
}
