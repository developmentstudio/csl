package csl.elasticsearch

import csl.storage.{connection => MySQLConnection}
import org.json4s._
import org.json4s.jackson.JsonMethods._
import java.sql.{Timestamp, PreparedStatement}
import javax.xml.bind.DatatypeConverter

class ResponseParser
{
  implicit val formats = DefaultFormats

  def parseJSON(json: String): Response = {
    parse(json).extract[Response]
  }
}

case class Response(_scroll_id: String, took: String, timed_out: Boolean, _shards: Shards, hits: Hits)
case class Shards(total: Int, successful: Int, failed: Int)
case class Hits(total: Int, max_score: Double, hits: List[Result])

case class Result(_index: String, _type: String, _id: String, _score: Double, _source: JObject)
{
  var properties = Map[String, Any]()

  def source(key: String): Option[Any] =
  {
    flatten(this._source.values)
    properties.get(key)
  }

  def sourceAsJson: String = compact(render(_source))

  private def flattenProperty(key: String, value: Any, prefix: String = ""): Unit = {
    value match {
      case v: String => properties = properties + (prefix -> v)
      case v: BigInt => properties = properties + (prefix -> v)
      case v: Double => properties = properties + (prefix -> v)
      case v: Map[_, _] =>
        val m = v.asInstanceOf[Map[String, Any]]
        flatten(m, prefix + ".")
      case v: List[_] => properties = properties + (prefix -> v)
      case v: Boolean => properties = properties + (prefix -> v)
      case v => throw new Exception(s"This type in not implemented and can therefore not be flattened. Class: ${v.getClass} Value: $v")
    }
  }

  private def flatten(m: Map[String, Any], prefix: String = ""): Unit = {
    for {
      (k, v) <- m
    } flattenProperty(k, v, prefix + k)
  }
}

class ResultCollection(val results: List[Result] = List.empty, relationKeys: List[String])
{
  override def toString: String = s"ResultCollection(${results.size})"

  def hasResults: Boolean = results.nonEmpty

  def save(label: String = ""): Unit = {
    val sqlQuery = "INSERT INTO rel (_index, _type, _id, relations, label, timestamp) VALUES (?, ?, ?, ?, ?, ?) ON DUPLICATE KEY UPDATE _id = _id, label = label"
    val statement = MySQLConnection.prepareStatement(sqlQuery)
    for {
      result <- results
    } addResultToBatchStatement(statement, result, label)
    statement.executeBatch()
    statement.close()
  }

  def addResultToBatchStatement(statement: PreparedStatement, result: Result, label: String) = {
    val date = DatatypeConverter.parseDateTime(result.source("request.timestamp").get.toString)

    statement.setString(1, result._index)
    statement.setString(2, result._type)
    statement.setString(3, result._id)
    statement.setString(4, getRelationDefinition(result, relationKeys))
    statement.setString(5, label)
    statement.setTimestamp(6, new Timestamp(date.getTime.getTime))
    statement.addBatch()
  }

  private def getRelationDefinition(result: Result, relationKeys: List[String]): String = {
    var rel: List[(String, JValue)] = List.empty

    relationKeys.sorted.foreach(key => {
      result.source(key) match {
        case Some(v) => rel = rel :+(key, JString(v.toString))
        case None =>
      }
    })

    compact(render(new JObject(rel)))
  }
}
