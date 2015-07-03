package elasticsearch

import java.sql._
import javax.xml.bind.DatatypeConverter

import csl.ast.Pattern
import elasticsearch.ast.{Document, Relation, Response, Result}
import elasticsearch.parser.RelationParser
import org.json4s.JsonAST.JObject
import org.json4s._
import org.json4s.jackson.JsonMethods._

object Storage {

  val driver = "com.mysql.jdbc.Driver"
  val url = "jdbc:mysql://127.0.0.1:3306/csl"
  val username = "root"
  val password = "root"

  Class.forName(driver).newInstance()
  val connection = DriverManager.getConnection(url, username, password)

  def init: Unit = {
    val deleteSetQuery = getConnection.prepareStatement("DELETE FROM raw_result_set")
    deleteSetQuery.execute()
    val deleteLabelQuery = getConnection.prepareStatement("DELETE FROM document_label")
    deleteLabelQuery.execute()
  }

  private def getConnection: Connection = connection

  def getRelations(pattern: Pattern): List[Relation] = {
    def getResultSetFromDatabase: ResultSet = {
      var query = "SELECT DISTINCT(relation) FROM raw_result_set"

      val identifiers = pattern.getRequestDefinitionIdentifiers
      if (identifiers.nonEmpty) {
        query += " WHERE "
        var patternPartQuery: List[String] = List.empty
        identifiers.foreach(_ => {
          patternPartQuery = patternPartQuery :+ "_id in (SELECT _id FROM document_label WHERE variable_name = ?)"
        })
        query += patternPartQuery.mkString(" AND ")
      }

      val statement = getConnection.prepareStatement(query)

      if (identifiers.nonEmpty) {
        val definitionNames = for {
          id <- identifiers
        } yield id.name

        for ((name, i) <- definitionNames.view.zipWithIndex) statement.setString(i + 1, name)
      }

      statement.executeQuery()
    }
    var relations: List[Relation] = List.empty
    val result = getResultSetFromDatabase
    while (result.next()) {
      relations = relations :+ RelationParser.parseJSON(result.getString("relation"))
    }
    result.close()
    relations
  }

  def getDocumentsBy(relation: Relation): List[Document] = {
    def documentFromResultSet(result: ResultSet): Document = {
      val _index = result.getString("_index")
      val _type = result.getString("_type")
      val _id = result.getString("_id")
      val relation = result.getString("relation")
      val timestamp = result.getString("timestamp")
      val variable_name = result.getString("variable_name")
      val body = result.getString("body")

      if (variable_name != null) {
        Document(_index, _type, _id, timestamp, List(variable_name), body)
      } else {
        Document(_index, _type, _id, timestamp, List.empty, body)
      }
    }
    def getResultSetFromDatabase(relation: Relation): ResultSet = {
      val statement = getConnection.prepareStatement(
        "SELECT raw_result_set._index, raw_result_set._type, raw_result_set._id, raw_result_set.relation, " +
          "       raw_result_set.timestamp, raw_result_set.body, document_label.variable_name " +
          "FROM raw_result_set " +
          "LEFT JOIN document_label ON raw_result_set._id = document_label._id " +
          "WHERE raw_result_set.relation = ?" +
          "ORDER BY raw_result_set.relation ASC, raw_result_set.timestamp ASC"
      )
      statement.setString(1, relation.rawJson)
      statement.executeQuery()
    }

    var documents: Map[String, Document] = Map.empty
    val result = getResultSetFromDatabase(relation)
    while (result.next()) {
      val document = documentFromResultSet(result)
      documents get document._id match {
        case Some(d) =>
          document.labels.foreach(l => d.addLabel(l))
          documents = documents + (document._id -> d)
        case None => documents = documents + (document._id -> document)
      }
    }
    result.close()
    documents.values.toList.sortBy(_._timestamp)
  }

  def save(response: Response, label: Option[String], relationKeys: List[String]): Unit = {
    if (response.hasHits) {
      val resultSetQuery = getConnection.prepareStatement(
        "INSERT INTO raw_result_set (_index, _type, _id, relation, timestamp, body) " +
          "VALUES (?, ?, ?, ?, ?, ?) " +
          "ON DUPLICATE KEY UPDATE _id = _id"
      )

      val documentLabelQuery = getConnection.prepareStatement(
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
        resultSetQuery.setString(6, r.sourceAsJson)
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

  def close: Unit = {
    if (!getConnection.isClosed) {
      getConnection.close()
    }
  }

}
