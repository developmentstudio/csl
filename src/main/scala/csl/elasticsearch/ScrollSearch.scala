package csl.elasticsearch

import java.sql.PreparedStatement
import java.util.concurrent.TimeUnit

import csl.ast.{Detector, Pattern, Variable}
import csl.elasticsearch.ast.Relation
import csl.elasticsearch.parser.ResponseParser
import csl.storage.ResponseStorage
import wabisabi.{Scan, SearchUriParameters}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

// TODO: ScrollSearch is not the right name.
class ScrollSearch(detector: Detector)
{
  type ESIndex = String

  private val generator: FilterQueryGenerator = new FilterQueryGenerator
  private val pattern: Pattern = this.detector.find.pattern
  private var finishedVariables: List[String] = List.empty

  private def waitForDocumentCollectionToComplete: Unit =
  {
    while (this.finishedVariables.distinct.size != this.pattern.variables.distinct.size) {
      print("Waiting.. ") // TODO: Remove
      print(this.finishedVariables.distinct + " ") // TODO: Remove
      println(this.pattern.variables.distinct) // TODO: Remove
      TimeUnit.SECONDS.sleep(1);
    }
  }

  def search(index: ESIndex = "20141016"): Unit =
  {
    this.pattern.variables.distinct foreach(v => {
      this.detector.variable(v) match {
        case Some(variable) => this.collectAllDocumentsMatchingVariable(variable, index)
        case None => throw new Exception(s"Type checker failed: Variabel $v is not defined.")
      }
    })

    this.waitForDocumentCollectionToComplete
    //this.collectAllRelatedDocuments

    println("Completed! I am the last message you will receive!!") // TODO: Remove
    System.exit(0)
  }

  private def collectAllDocumentsMatchingVariable(variable: Variable, index: ESIndex): Unit =
  {
    val query = this.generator.generate(variable.properties)
    val request = client.search(index, query, uriParameters = SearchUriParameters(searchType = Some(Scan), scroll = Some("10m")))
    request onComplete {
      case Success(r) =>
        val response = ResponseParser.parseJSON(r.getResponseBody)
        this.collectNextPageForVariable(response._scroll_id, variable)
      case Failure(e) => println("An error has occured: " + e.getMessage)
    }
  }

  private def collectNextPageForVariable(scroll_id: String, variable: Variable): Unit =
  {
    val request = client.scroll("10m", scroll_id)
    request.onComplete {
      case Success(r) =>
        val response = ResponseParser.parseJSON(r.getResponseBody)

        if (response.hasHits) {
          ResponseStorage.save(response, Some(variable.name), this.pattern.relationKeys)
          this.collectNextPageForVariable(response._scroll_id, variable)
        } else {
          println("-> " + variable.name + " " + r.getResponseBody) // TODO: Remove
          this.finishedVariables = this.finishedVariables :+ variable.name
        }
      case Failure(e) => throw new Exception(e)
    }
  }

//  private def collectAllRelatedDocuments: Unit = {
//    val statement: PreparedStatement = {
//      val variables = this.pattern.variables.distinct
//      var query  = "SELECT DISTINCT(relation) FROM raw_result_set"
//      if (variables.nonEmpty) {
//        query += " WHERE "
//        var parts: List[String] = List.empty
//        variables.foreach(_ => {
//          parts = parts :+ "_id in (SELECT _id FROM document_label WHERE variable_name = ?)"
//        })
//        query += parts.mkString(" AND ")
//      }
//      val statement = MySQLConnection.prepareStatement(query)
//      if (variables.nonEmpty) {
//        for((variableName, i) <- variables.view.zipWithIndex) statement.setString(i + 1, variableName)
//      }
//      statement
//    }
//
//    val result = statement.executeQuery()
//
//    while(result.next()) {
//      // TODO: Create ES request.
//      // TODO: Parse Response from ES.
//
//      val relation = RelationParser.parseJSON(result.getString("relation"))
//      val query = this.generator.generate(relation.properties)
//
//      println(query)
//
//
//    }
//  }
}







//class RelationCollector(detector: Detector)
//{
//  def collect(relation: Relation): Boolean = {
//    true
//  }
//}


/* TODO: Brainstorm thingy:

Detector.collect(List[Variable])
Detector.collect(List[Relation])
Detector.report()


Variables.collect(detector: Detector)
Relation.collect(detector: Detector)

*/
