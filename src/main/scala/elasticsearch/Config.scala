package elasticsearch

import org.json4s.DefaultFormats
import org.json4s.jackson.JsonMethods.parse

import scala.io.Source

object Config {

  implicit val formats = DefaultFormats

  private var settings = Map[String, Any]()
  private val config = parse(jsonFile("config.json")).extract[Map[String, Any]]

  def setting(key: String): String = {
    flatten(this.config)
    settings.get(key) match {
      case Some(value) => value.toString
      case None => throw new Exception(s"No setting defined for $key in config.")
    }
  }

  private def jsonFile(path: String): String = {
    val file = Source.fromFile(path)
    val content = file.mkString
    file.close
    content
  }

  private def flattenSetting(key: String, value: Any, prefix: String = ""): Unit = {
    value match {
      case v: String => settings = settings + (prefix -> v)
      case v: BigInt => settings = settings + (prefix -> v)
      case v: Double => settings = settings + (prefix -> v)
      case v: Map[_, _] =>
        val m = v.asInstanceOf[Map[String, Any]]
        flatten(m, prefix + ".")
      case v: List[_] => settings = settings + (prefix -> v)
      case v: Boolean => settings = settings + (prefix -> v)
      case v => throw new Exception(s"This type in not implemented and can therefore not be flattened. Class: ${v.getClass} Value: $v")
    }
  }

  private def flatten(m: Map[String, Any], prefix: String = ""): Unit = {
    for {
      (k, v) <- m
    } flattenSetting(k, v, prefix + k)
  }
}
