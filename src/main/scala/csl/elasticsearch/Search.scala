package csl.elasticsearch

import csl.ast.Variable

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

import wabisabi.{Scan, SearchUriParameters}

class ScrollSearch(variable: Variable, generator: FilterQueryGenerator = new FilterQueryGenerator)
{
  private val query: String = generator.generate(variable)
  private val responseParser: ResponseParser = new ResponseParser

  def search(_index: String = "20141016"): Unit =  {
    val req = client.search(_index, query, uriParameters = SearchUriParameters(searchType = Some(Scan), scroll = Some("10m")))
    req onComplete {
      case Success(body) => {
        val response = responseParser.parseJSON(body.getResponseBody())
        scroll(response._scroll_id)
      }
      case Failure(e) => {
        println("An error has occured: " + e.getMessage)
      }
    }
  }

  private def scroll(scroll_id: String): Unit = Future {
    client.scroll("10m", scroll_id) onComplete {
      case Success(body) => {

        val response = responseParser.parseJSON(body.getResponseBody())

        val collection = new ResultCollection(response.hits.hits)

        if (collection.hasResults) {
          collection.save(variable.name)
          scroll(response._scroll_id)
        } else {
          println("No Results!")
        }
      }
      case Failure(e) => {
        println("An error has occured: " + e.getMessage)
      }
    }
  }

}
