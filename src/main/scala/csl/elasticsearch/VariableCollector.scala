package csl.elasticsearch

import java.util.concurrent.TimeUnit

import csl.ast.{Detector, Variable}
import csl.elasticsearch.parser.ResponseParser
import csl.storage.ResponseStorage
import wabisabi.{Scan, SearchUriParameters}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

class VariableCollector(detector: Detector)
{
  val index: String = "20141016"

  private val generator: FilterQueryGenerator = new FilterQueryGenerator

  private val variables: List[Variable] = {
    val variableNames: List[String] = detector.find.pattern.variables.distinct
    for (name <- variableNames) yield detector.variable(name).get
  }

  private val status = new CollectorStatus("variable_request_status.log")

  def collect: Boolean = {
    this.variables.distinct foreach(search)
    this.waitForDocumentCollectionToComplete
    println("Collection of Variables Completed!") // TODO: Remove
    true
  }

  private def search(variable: Variable): Unit =
  {
    val query = this.generator.generate(variable.properties)
    val request = client.search(index, query, uriParameters = SearchUriParameters(searchType = Some(Scan), scroll = Some("10m")))
    request onComplete {
      case Success(r) =>
        val response = ResponseParser.parseJSON(r.getResponseBody)
        this.scrollNextPage(response._scroll_id, variable)
      case Failure(e) => println("An error has occured: " + e.getMessage)
    }
  }

  private def scrollNextPage(scroll_id: String, variable: Variable): Unit =
  {
    val request = client.scroll("10m", scroll_id)
    request.onComplete {
      case Success(r) =>
        val response = ResponseParser.parseJSON(r.getResponseBody)
        if (response.hasHits) {
          ResponseStorage.save(response, Some(variable.name), this.detector.find.pattern.relationKeys)
          this.scrollNextPage(response._scroll_id, variable)
        } else {
          status.setCompleted(variable.name)
        }
      case Failure(e) => throw new Exception(e)
    }
  }

  private def waitForDocumentCollectionToComplete: Unit =
  {
    print("Waiting") // TODO: Remove
    while (!this.status.isCompleted(this.detector.find.pattern.variables.distinct)) {
      print(".") // TODO: Remove
      TimeUnit.SECONDS.sleep(1);
    }
    println(".") // TODO: Remove
    this.status.clear
  }
}
