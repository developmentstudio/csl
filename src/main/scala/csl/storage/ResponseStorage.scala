package csl.storage

import java.sql.{PreparedStatement, Timestamp}
import javax.xml.bind.DatatypeConverter

import csl.elasticsearch.ast.{Response, Result}
import org.json4s.JsonAST.JObject
import org.json4s.{JValue, JString}
import org.json4s.jackson.JsonMethods.{compact, render}

object ResponseStorage {

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

  def save(response: Response, label: Option[String], relationKeys: List[String]): Unit = {
    if (response.hasHits) {
      generateQueries(response.hits.hits, label, relationKeys)
      executeQuery(this.resultSetQuery)
      executeQuery(this.documentLabelQuery)
    }
  }

  private def generateQueries(results: List[Result], label: Option[String] = None, relationKeys: List[String]): Unit = {
    results.foreach(r => {
      val relationInformation = generateRelation(r, relationKeys)
      addDocument(r, relationInformation)
      addLabel(r, label)
    })
  }

  private def addDocument(result: Result, relationInformation: String): Unit = {
    val date = DatatypeConverter.parseDateTime(result.source("request.timestamp").get.toString)

    this.resultSetQuery.setString(1, result._index)
    this.resultSetQuery.setString(2, result._type)
    this.resultSetQuery.setString(3, result._id)
    this.resultSetQuery.setString(4, relationInformation)
    this.resultSetQuery.setTimestamp(5, new Timestamp(date.getTime.getTime))
    this.resultSetQuery.addBatch()
  }

  private def addLabel(result: Result, label: Option[String]): Unit = {
    label match {
      case Some(l) =>
        this.documentLabelQuery.setString(1, result._id)
        this.documentLabelQuery.setString(2, l)
        this.documentLabelQuery.addBatch()
      case None =>
    }
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
