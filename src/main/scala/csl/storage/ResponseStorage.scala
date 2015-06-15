package csl.storage

import java.sql._
import javax.xml.bind.DatatypeConverter

import csl.elasticsearch.ast.{Response, Result}
import org.json4s.JsonAST.JObject
import org.json4s.jackson.JsonMethods.{compact, render}
import org.json4s.{JString, JValue}

object ResponseStorage {

  def clear: Unit = {
    val driver = "com.mysql.jdbc.Driver"
    val url = "jdbc:mysql://127.0.0.1:3306/csl"
    val username = "root"
    val password = "root"
    Class.forName(driver).newInstance()
    val connection = DriverManager.getConnection(url, username, password)

    val deleteSetQuery = connection.prepareStatement("DELETE FROM raw_result_set")
    val deleteLabelQuery = connection.prepareStatement("DELETE FROM document_label")

    deleteSetQuery.execute()
    deleteLabelQuery.execute()
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

      if (!connection.isClosed) {
        connection.close()
      }
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
