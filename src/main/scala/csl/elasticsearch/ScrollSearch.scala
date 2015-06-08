package csl.elasticsearch

import csl.ast.{Detector, Variable}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

import wabisabi.{Scan, SearchUriParameters}

class ScrollSearch(detector: Detector, generator: FilterQueryGenerator = new FilterQueryGenerator)
{
  type Index = String

  private val responseParser: ResponseParser = new ResponseParser

  def search(_index: Index = "20141016"): Unit =  {
    for (v <- detector.variables) searchVariable(v, _index)
  }

  def searchVariable(variable: Variable, _index: Index): Unit =  {

    val query = generator.generate(variable)

    val req = client.search(_index, query, uriParameters = SearchUriParameters(searchType = Some(Scan), scroll = Some("10m")))
    req onComplete {
      case Success(body) =>
        val response = responseParser.parseJSON(body.getResponseBody)
        scroll(response._scroll_id, variable)
        println(s"Total hits for ${variable.name}: ${response.hits.total}")
      case Failure(e) => println("An error has occured: " + e.getMessage)
    }
  }

  private def scroll(scroll_id: String, variable: Variable): Unit = Future {
    client.scroll("10m", scroll_id) onComplete {
      case Success(body) =>
        val response = responseParser.parseJSON(body.getResponseBody)
        val collection = new ResultCollection(response.hits.hits, detector.find.pattern.relationKeys)
        if (collection.hasResults) {
          collection.save(variable.name)
          scroll(response._scroll_id, variable)
        } else {
          println(s"${variable.name} Completed!")
        }
      case Failure(e) => println("An error has occured: " + e.getMessage)
    }
  }

}
