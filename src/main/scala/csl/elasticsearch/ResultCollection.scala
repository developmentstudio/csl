package csl.elasticsearch

import java.sql.{Timestamp, PreparedStatement}
import javax.xml.bind.DatatypeConverter
import csl.storage.{connection => MySQLConnection}

import org.json4s._
import org.json4s.jackson.JsonMethods._

class ResultCollection(val results: List[Result] = List.empty, relationKeys: List[String]) {

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
