package csl.elasticsearch

import java.sql.PreparedStatement

import csl.ast._
import csl.storage.{connection => MySQLConnection}
import org.json4s.JsonAST._
import org.json4s.jackson.JsonMethods.{parse => parseJSON, _}
import scala.collection.Map


import scala.concurrent.ExecutionContext.Implicits.global
import wabisabi.{Scan, SearchUriParameters}

import scala.concurrent.Future
import scala.util.{Failure, Success}

class ScrollSearch(variable: Variable, generator: FilterQueryGenerator = new FilterQueryGenerator)
{
  private var scrollId: String = ""
  private val _index: String = "20141016"
  private val query: String = generator.generate(variable)
  private val responseParser: ResponseParser = new ResponseParser

  def search(): Unit =  {
    val req = client.search(_index, query, uriParameters = SearchUriParameters(searchType = Some(Scan), scroll = Some("10m")))
    req onComplete {
      case Success(response) => {
        val body = response.getResponseBody()

        println(responseParser.parseJSON(body))

        //val parsedBody = parseJSON(body)
        //getDocuments(filterScrollId(parsedBody))
      }
      case Failure(e) => {
        println("An error has occured: " + e.getMessage)
      }
    }
  }

//  private def getDocuments(scrollId: String): Unit = Future {
//    client.scroll("10m", scrollId) onComplete {
//      case Success(response) => {
//        val body = response.getResponseBody()
//        val parsedBody = parseJSON(body)
//        val collection = createResultCollection(parsedBody \ "hits" \ "hits")
//        if (collection.hasResults) {
//          collection.store(variable.name)
//          getDocuments(filterScrollId(parsedBody))
//        } else {
//          println("NO RESULT!")
//        }
//      }
//      case Failure(e) => {
//        println("An error has occured: " + e.getMessage)
//      }
//    }
//  }

  private def filterScrollId(json: JValue): String = (json \ "_scroll_id").values.toString()

  private def setScrollId(newScrollId: String): Unit = scrollId = newScrollId

}

//class ResultCollection(val results: List[Result] = List.empty) {
//
//  override def toString: String = s"ResultCollection(${results.size})"
//
//  def hasResults: Boolean = results.size > 0
//
//  def store(label: String = ""): Unit = {
//    def addDoc(result: Result, statement: PreparedStatement) = {
//      statement.setString(1, result._index)
//      statement.setString(2, result._type)
//      statement.setString(3, result._id)
//      statement.setString(4, result._source)
//      statement.setString(5, label)
//      statement.addBatch()
//    }
//
//    val sqlQuery = "INSERT INTO result (_index, _type, _id, _source, label) VALUES (?, ?, ?, ?, ?) ON DUPLICATE KEY UPDATE _id = _id"
//    val statement = MySQLConnection.prepareStatement(sqlQuery)
//    for {
//      result <- results
//    } addDoc(result, statement)
//
//
//    statement.executeBatch()
//    statement.close()
//  }
//
//}
//
//class Result(val _index: String, val _type: String, val _id: String, val _source: String) {
//
//  override def toString: String = s"Result(${_index}, ${_type}, ${_id}, ${_source})"
//
//}
