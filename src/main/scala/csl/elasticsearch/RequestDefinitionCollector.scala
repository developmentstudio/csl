package csl.elasticsearch

import java.util.concurrent.TimeUnit

import csl.ast.{Identifier, Detector, RequestDefinition}
import csl.elasticsearch.parser.ResponseParser
import csl.storage.ResponseStorage
import wabisabi.{Scan, SearchUriParameters}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

class RequestDefinitionCollector(detector: Detector)
{
  val index: String = "20141016"

  private val generator: FilterQueryGenerator = new FilterQueryGenerator

  private val requestDefinitions: List[RequestDefinition] = {
    val identifiers: List[Identifier] = detector.find.pattern.getRequestDefinitionIdentifiers.distinct
    for (id <- identifiers) yield detector.definition(id.name).get
  }

  private val status = new CollectorStatus("variable_request_status.log")

  def collect: Boolean = {
    requestDefinitions foreach(search)
    this.waitForDocumentCollectionToComplete
    println("Collection of Variables Completed!") // TODO: Remove
    true
  }

  private def search(definition: RequestDefinition): Unit =
  {
    val query = this.generator.generate(definition.properties)
    println(query)

    val request = client.search(index, query, uriParameters = SearchUriParameters(searchType = Some(Scan), scroll = Some("10m")))
    request onComplete {
      case Success(r) =>
        val response = ResponseParser.parseJSON(r.getResponseBody)

        println("Request definition " + definition.name + " exists " + response.hits.total + " times in the ES index.")
        this.scrollNextPage(response._scroll_id, definition)
      case Failure(e) => println("An error has occured: " + e.getMessage)
    }
  }

  private def scrollNextPage(scroll_id: String, variable: RequestDefinition): Unit =
  {
    val request = client.scroll("10m", scroll_id)
    request.onComplete {
      case Success(r) =>
        val response = ResponseParser.parseJSON(r.getResponseBody)
        if (response.hasHits) {
          ResponseStorage.save(response, Some(variable.name), detector.find.relation.keys)
          this.scrollNextPage(response._scroll_id, variable)
        } else {
          status.setCompleted(variable.name)
        }
      case Failure(e) => throw new Exception(e)
    }
  }

  private def waitForDocumentCollectionToComplete: Unit =
  {
//    print("Waiting") // TODO: Remove

    val parts = this.detector.find.pattern.getRequestDefinitionIdentifiers.map(_.name)
    while (!this.status.isCompleted(parts)) {
//      print(".") // TODO: Remove
      TimeUnit.SECONDS.sleep(1);
    }
//    println(".") // TODO: Remove
    this.status.clear
  }
}