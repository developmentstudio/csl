package elasticsearch.ast

import org.json4s.DefaultFormats
import org.json4s.jackson.JsonMethods.parse

case class Document(_index: String, _type: String, _id: String, _timestamp: String, var labels: List[String], body: String) {
  implicit val formats = DefaultFormats
  private var properties = Map[String, Any]()

  def source(key: String): Option[Any] = {
    val parsedJson = parse(body).extract[Map[String, Any]]
    flatten(parsedJson)

    properties.get(key)
  }

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



  def addLabel(label: String): Unit = this.labels = this.labels :+ label

  def hasLabel(label: String): Boolean = this.labels contains(label)
}
case class Body()
