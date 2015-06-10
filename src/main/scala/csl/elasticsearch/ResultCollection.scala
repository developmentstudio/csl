package csl.elasticsearch

import java.sql.{PreparedStatement, Timestamp}
import javax.xml.bind.DatatypeConverter

import csl.elasticsearch.ast.Result
import csl.storage.{connection => MySQLConnection}
import org.json4s.JsonAST.JObject
import org.json4s.jackson.JsonMethods.{compact, render}
import org.json4s.{JString, JValue}

class ResultCollection(val results: List[Result] = List.empty, relationKeys: List[String])
{
  override def toString: String = s"ResultCollection(${results.size})"

  private val resultSetQuery = MySQLConnection.prepareStatement(
    "INSERT INTO raw_result_set (_index, _type, _id, relation, timestamp) " +
      "VALUES (?, ?, ?, ?, ?) " +
      "ON DUPLICATE KEY UPDATE _id = _id"
  )

  private val documentLabelQuery = MySQLConnection.prepareStatement(
    "INSERT INTO document_label (_id, variable_name) " +
      "VALUES (?, ?) " +
      "ON DUPLICATE KEY UPDATE _id = _id, variable_name = variable_name"
  )

  def hasResults: Boolean = results.nonEmpty

  def save(label: String = ""): Unit = {
    generateQueries(results, label)
    executeQuery(this.resultSetQuery)
    executeQuery(this.documentLabelQuery)
  }

  private def generateQueries(results: List[Result], label: String): Unit = {
    results.foreach(r => {
      addDocument(r)
      addLabel(r, label)
    })
  }

  private def addDocument(result: Result): Unit = {
    val date = DatatypeConverter.parseDateTime(result.source("request.timestamp").get.toString)

    resultSetQuery.setString(1, result._index)
    resultSetQuery.setString(2, result._type)
    resultSetQuery.setString(3, result._id)
    resultSetQuery.setString(4, generateRelation(result, relationKeys))
    resultSetQuery.setTimestamp(5, new Timestamp(date.getTime.getTime))
    resultSetQuery.addBatch()
  }

  private def addLabel(result: Result, label: String): Unit = {
    documentLabelQuery.setString(1, result._id)
    documentLabelQuery.setString(2, label)
    documentLabelQuery.addBatch()
  }

  private def executeQuery(statement: PreparedStatement): Unit = {
    statement.executeBatch()
    statement.close()
  }

  private def generateRelation(result: Result, relationKeys: List[String]): String = {
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
