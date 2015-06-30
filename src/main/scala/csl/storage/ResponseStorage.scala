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

      val identifiers = pattern.getRequestDefinitionIdentifiers
      var query  = "SELECT DISTINCT(relation) FROM raw_result_set"
      if (identifiers.nonEmpty) {
        query += " WHERE "
        var patternPartQuery: List[String] = List.empty
        identifiers.foreach(_ => {
          patternPartQuery = patternPartQuery :+ "_id in (SELECT _id FROM document_label WHERE variable_name = ?)"
        })
        query += patternPartQuery.mkString(" AND ")
      }

      val statement = connection.prepareStatement(query)
      if (identifiers.nonEmpty) {
        val definitionNames = for {
          id <- identifiers
        } yield id.name

        for((name, i) <- definitionNames.view.zipWithIndex) statement.setString(i + 1, name)
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

  def getDocumentsBy(relation: Relation): List[Document] = {
    val statement = connection.prepareStatement(
      "SELECT raw_result_set._index, raw_result_set._type, raw_result_set._id, raw_result_set.relation, raw_result_set.timestamp, document_label.variable_name " +
      "FROM raw_result_set " +
      "LEFT JOIN document_label ON raw_result_set._id = document_label._id " +
      "WHERE raw_result_set.relation = ?" +
      "ORDER BY raw_result_set.relation ASC, raw_result_set.timestamp ASC"
    )
    statement.setString(1, relation.rawJson)
    val result = statement.executeQuery()

    var docMap: Map[String, Document] = Map.empty
    while(result.next())
    {
      val _index = result.getString("_index")
      val _type = result.getString("_type")
      val _id = result.getString("_id")
      val relation = result.getString("relation")
      val timestamp = result.getString("timestamp")
      val variable_name = result.getString("variable_name")

      docMap get _id match {
        case Some(d) => {
          d.addLabel(variable_name)
          docMap = docMap + (_id -> d)
        }
        case None => {
          if (variable_name != null) {
            docMap = docMap + (_id -> Document(_index, _type, _id, timestamp, List(variable_name)))
          } else {
            docMap = docMap + (_id -> Document(_index, _type, _id, timestamp, List.empty))
          }
        }
      }
    }
    result.close()
    statement.close()

    docMap.values.toList.sortBy(_._timestamp)
  }

  def save(response: Response, label: Option[String], relationKeys: List[String]): Unit = {
    if (response.hasHits) {
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

case class Document(_index: String, _type: String, _id: String, _timestamp: String, var labels: List[String])
{
  def addLabel(label: String): Unit = this.labels = this.labels :+ label

  def hasLabel(label: String): Boolean = this.labels contains(label)
}
