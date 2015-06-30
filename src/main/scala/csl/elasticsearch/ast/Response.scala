package csl.elasticsearch.ast

import org.json4s.JObject
import org.json4s.jackson.JsonMethods.{compact, render}

case class Response(_scroll_id: String, took: String, timed_out: Boolean, _shards: Shards, hits: Hits)
{
  def hasHits: Boolean = hits.hits.nonEmpty
}

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

  @throws(classOf[Exception])
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
