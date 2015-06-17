package csl.elasticsearch

import java.util.concurrent.TimeUnit

import csl.ast.Detector
import csl.elasticsearch.ast.Relation
import csl.elasticsearch.parser.ResponseParser
import csl.storage.ResponseStorage
import wabisabi.{Scan, SearchUriParameters}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

class RelationCollector(detector: Detector)
{
  val index: String = "20141016"

  private val generator: FilterQueryGenerator = new FilterQueryGenerator

  private val relations = ResponseStorage.getRelations(detector.find.pattern)

  private val status = new CollectorStatus("relation_request_status.log")

  def collect: Boolean = {
    println("Start Relation Collector!")
    this.relations.distinct foreach(search)
    this.waitForDocumentCollectionToComplete
    println("Collection of Related Documents completed!") // TODO: Remove
    true
  }

  private def search(relation: Relation): Unit =
  {
    val query = this.generator.generate(relation.properties)
    try {
      val request = client.search(index, query, uriParameters = SearchUriParameters(searchType = Some(Scan), scroll = Some("10m")))
      request onComplete {
        case Success(r) =>
          val response = ResponseParser.parseJSON(r.getResponseBody)
          this.scrollNextPage(response._scroll_id, relation)
        case Failure(e) => println("An error has occured: " + e.getMessage)
      }
    } catch {
      case e: Throwable => println("ERROR?!")
    }

  }

  private def scrollNextPage(scroll_id: String, relation: Relation): Unit =
  {
    try {
      val request = client.scroll("10m", scroll_id)
      request.onComplete {
        case Success(r) =>
          val response = ResponseParser.parseJSON(r.getResponseBody)
          if (response.hasHits) {
            ResponseStorage.save(response, None, this.detector.find.pattern.relationKeys)
            this.scrollNextPage(response._scroll_id, relation)
          } else {
            status.setCompleted(relation.toString)
          }
        case Failure(e) => throw new Exception(e)
      }
    } catch {
      case e: Throwable => println("?!RORRE")
    }

  }

  private def waitForDocumentCollectionToComplete: Unit =
  {
    //print("Waiting") // TODO: Remove
    while (!this.status.isCompleted(this.relations map(_.toString))) {
      //print(".") // TODO: Remove
      TimeUnit.SECONDS.sleep(1);
    }
    //println(".") // TODO: Remove
    this.status.clear
  }

}
