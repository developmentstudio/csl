package elasticsearch

import java.util.concurrent.TimeUnit

import csl.ast.Detector
import elasticsearch.ast.Relation
import elasticsearch.parser.ResponseParser
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
    if (relation.properties.length > 0 && this.detector.find.pattern.elements.length > 1) {
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
    } else {
      status.setCompleted(relation.toString)
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
            ResponseStorage.save(response, None, this.detector.find.relation.keys)
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
    val parts = this.relations map(_.toString)
    while (!this.status.isCompleted(parts)) {
      TimeUnit.SECONDS.sleep(1);
    }
    this.status.clear
  }

}

