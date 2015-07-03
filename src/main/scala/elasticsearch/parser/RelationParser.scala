package elasticsearch.parser

import csl.ast.{Property, StringValue}
import elasticsearch.ast.Relation
import org.json4s.DefaultFormats
import org.json4s.jackson.JsonMethods.parse

object RelationParser
{
  implicit val formats = DefaultFormats

  def parseJSON(json: String): Relation = {
    val parsedJson = parse(json).extract[Map[String, Any]]
    val ps = for ((key, value) <- parsedJson) yield Property(key, StringValue(value.toString))
    Relation(properties = ps.toList, rawJson = json)
  }
}
