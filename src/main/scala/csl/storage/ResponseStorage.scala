package csl.storage

import java.sql._
import javax.xml.bind.DatatypeConverter

import csl.ast.Pattern
import csl.elasticsearch.ast.{Relation, Response, Result}
import csl.elasticsearch.parser.RelationParser
import org.json4s.JsonAST.JObject
import org.json4s.jackson.JsonMethods.{compact, render}
import org.json4s.{JString, JValue}

object ResponseStorage {

  val driver = "com.mysql.jdbc.Driver"
  val url = "jdbc:mysql://127.0.0.1:3306/csl"
  val username = "root"
  val password = "root"

  Class.forName(driver).newInstance()
  val connection = DriverManager.getConnection(url, username, password)

  def clear: Unit = {
    val deleteSetQuery = connection.prepareStatement("DELETE FROM raw_result_set")
    deleteSetQuery.execute()

    val deleteLabelQuery = connection.prepareStatement("DELETE FROM document_label")
    deleteLabelQuery.execute()
  }

  def getRelations(pattern: Pattern): List[Relation] = {
    val statement: PreparedStatement = {
      val variables = pattern.variables.distinct
      var query  = "SELECT DISTINCT(relation) FROM raw_result_set"
      if (variables.nonEmpty) {
        query += " WHERE "
        var parts: List[String] = List.empty
        variables.foreach(_ => {
          parts = parts :+ "_id in (SELECT _id FROM document_label WHERE variable_name = ?)"
        })
        query += parts.mkString(" AND ")
      }
      val statement = MySQLConnection.prepareStatement(query)
      if (variables.nonEmpty) {
        for((variableName, i) <- variables.view.zipWithIndex) statement.setString(i + 1, variableName)
      }
      statement
    }
    val result = statement.executeQuery()

    var relations: List[Relation] = List.empty
    while(result.next()) {
      relations = relations :+ RelationParser.parseJSON(result.getString("relation"))
    }

    relations
  }

  def save(response: Response, label: Option[String], relationKeys: List[String]): Unit = {
    if (response.hasHits) {

      val driver = "com.mysql.jdbc.Driver"
      val url = "jdbc:mysql://127.0.0.1:3306/csl"
      val username = "root"
      val password = "root"
      Class.forName(driver).newInstance()
      val connection = DriverManager.getConnection(url, username, password)

      val resultSetQuery = connection.prepareStatement(
        "INSERT INTO raw_result_set (_index, _type, _id, relation, timestamp) " +
          "VALUES (?, ?, ?, ?, ?) " +
          "ON DUPLICATE KEY UPDATE _id = _id"
      )

      val documentLabelQuery = connection.prepareStatement(
        "INSERT INTO document_label (_id, variable_name) " +
          "VALUES (?, ?) " +
          "ON DUPLICATE KEY UPDATE _id = _id, variable_name = variable_name"
      )

      response.hits.hits.foreach(r => {
        val relationInformation = generateRelation(r, relationKeys)

        val date = DatatypeConverter.parseDateTime(r.source("request.timestamp").get.toString)
        resultSetQuery.setString(1, r._index)
        resultSetQuery.setString(2, r._type)
        resultSetQuery.setString(3, r._id)
        resultSetQuery.setString(4, relationInformation)
        resultSetQuery.setTimestamp(5, new Timestamp(date.getTime.getTime))
        resultSetQuery.addBatch()

        label match {
          case Some(l) =>
            documentLabelQuery.setString(1, r._id)
            documentLabelQuery.setString(2, l)
            documentLabelQuery.addBatch()
          case None =>
        }
      })

      resultSetQuery.executeBatch()
      documentLabelQuery.executeBatch()
    }
  }

  def close(): Unit = {
    if (!connection.isClosed) {
      connection.close()
    }
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
