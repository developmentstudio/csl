package csl.elasticsearch.parser

import csl.elasticsearch.ast.Response
import org.json4s.DefaultFormats
import org.json4s.jackson.JsonMethods.parse

object ResponseParser
{
  implicit val formats = DefaultFormats

  val TimeToWaitInMilliseconds = 100

  def parseJSON(json: String): Response = {
    Thread.sleep(TimeToWaitInMilliseconds)
    parse(json).extract[Response]
  }
}
