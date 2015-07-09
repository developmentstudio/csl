package elasticsearch.collector

import java.util.concurrent.TimeUnit

import csl.ast.{Detector, Identifier, RequestDefinition}
import elasticsearch._
import elasticsearch.parser.ResponseParser
import wabisabi.{Scan, SearchUriParameters}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

class RequestDefinitionCollector(detector: Detector) {
  private val index: String = Config.setting("settings.elasticsearch.index")
  private val _type: Option[String] = {
    val t = Config.setting("settings.elasticsearch.type")
    if (t.isEmpty) {
      None
    } else {
      Some(t)
    }
  }

  private val generator: FilterQueryGenerator = new FilterQueryGenerator(detector.find.from, detector.find.till)

  private val requestDefinitions: List[RequestDefinition] = {
    val identifiers: List[Identifier] = detector.find.pattern.getRequestDefinitionIdentifiers.distinct
    for (id <- identifiers) yield detector.definition(id.name).get
  }

  private val status = new CollectorStatus

  def collect: Boolean = {
    requestDefinitions foreach (search)
    this.waitForDocumentCollectionToComplete
    true
  }

  private def search(definition: RequestDefinition): Unit = {
    val query = this.generator.generate(definition.properties)
    val request = client.search(index, query, _type, SearchUriParameters(searchType = Some(Scan), scroll = Some("10m")))
    request onComplete {
      case Success(r) =>
        val response = ResponseParser.parseJSON(r.getResponseBody)
        println("Request definition " + definition.name + " exists " + response.hits.total + " times in the ES index.") // TODO: Remove
        this.scrollNextPage(response._scroll_id, definition)
      case Failure(e) => println("An error has occured: " + e.getMessage)
    }
  }

  private def scrollNextPage(scroll_id: String, variable: RequestDefinition): Unit = {
    val request = client.scroll("10m", scroll_id)
    request.onComplete {
      case Success(r) =>
        val response = ResponseParser.parseJSON(r.getResponseBody)
        if (response.hasHits) {
          Storage.save(response, Some(variable.name), detector.find.relation.keys)
          this.scrollNextPage(response._scroll_id, variable)
        } else {
          status.setCompleted(variable.name)
        }
      case Failure(e) => throw new Exception(e)
    }
  }

  private def waitForDocumentCollectionToComplete: Unit = {
    val parts = this.detector.find.pattern.getRequestDefinitionIdentifiers.map(_.name)
    while (!this.status.isCompleted(parts)) {
      TimeUnit.SECONDS.sleep(1);
    }
    this.status.clear
  }
}
