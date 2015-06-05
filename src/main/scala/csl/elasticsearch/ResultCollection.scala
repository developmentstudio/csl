package csl.elasticsearch

import java.sql.PreparedStatement
import csl.storage.{connection => MySQLConnection}

class ResultCollection(val results: List[Result] = List.empty) {

  override def toString: String = s"ResultCollection(${results.size})"

  def hasResults: Boolean = results.size > 0

  def save(label: String = ""): Unit = {
    def addDoc(result: Result, statement: PreparedStatement) = {
      statement.setString(1, result._index)
      statement.setString(2, result._type)
      statement.setString(3, result._id)
      statement.setString(4, result.sourceAsJson)
      statement.setString(5, label)
      statement.addBatch()
    }

    val sqlQuery = "INSERT INTO result (_index, _type, _id, _source, label) VALUES (?, ?, ?, ?, ?) ON DUPLICATE KEY UPDATE _id = _id"
    val statement = MySQLConnection.prepareStatement(sqlQuery)
    for {
      result <- results
    } addDoc(result, statement)
    statement.executeBatch()
    statement.close()
  }

}
