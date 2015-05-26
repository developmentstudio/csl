package csl.elasticsearch

import csl.ast._

import org.json4s.DefaultFormats
import org.json4s.JsonAST._
import org.json4s.jackson.JsonMethods.{parse => parseJSON, _}


import scala.concurrent.ExecutionContext.Implicits.global

class Search(generator: QueryGenerator = new FilterQueryGenerator, storage: Storage = new Storage) {

  def search(variable: Variable): Unit = {
    val query = generator.generate(variable)
    println(query)
    client.search("20141016v2", query).onSuccess{
      case ground => {
        val body = ground.getResponseBody()
        val result = parseResponseBody(body)

        println(result)
      }
    }
  }

  private def parseResponseBody(json: String): ResultCollection = {

      // HIER VERDER!


    new ResultCollection()
  }

}

class ResultCollection(results: List[Result] = List.empty)

class Result(_index: String, _type: String, _id: String, _source: String)






//case class Result(took: Int, time_out: String)




class SearchResult(json: String) {

  private val parsed = parseJSON(json)

  val took: Int = compact(render(parsed \ "took")).toInt
  val timedOut: Boolean = compact(render(parsed \ "timed_out")).toBoolean
  val hits: List[Hit] = List.empty

  case class Hit(_index: String, _type: String, _id: String, _score: Double, _source: String)

  def u(jo: JValue) = for {
    JObject(f) <- jo
  } println(f)


  val _hits = pretty(parsed \ "hits" \ "hits")
  //println(parseJSON(_hits))
  val k = for {
    JArray(h) <- parseJSON(_hits)
  } yield h



//  val testje = for {
//    JArray(u) <- parseJSON(_hits)
//  } yield u map t
//
//  println(testje)


//  for {
//    JArray(xs) <- parseJSON(_hits)
//  } yield JString(xs map t)


//  for {
//    JObject(s) <- _hits
//  } println(s)

//  val servers = for {
//    te @ JArray(hits) <- parsed \ "hits" \ "hits"
//    hit <- te \ "_index"
//
//    //index <- hit \ "_index"
////    JField("ip", JString(ip)) <- server
////    JField("uptime", JInt(uptime)) <- server
//  } println(hit)



//  (parsed \ "hits" \ "hits") match {
//    case JArray(s) => println(s); for {
//      h <- s
//      x <- h \ "_index"
//    } println(x)//println(compact(render(h)))
//  }

//  for {
//    JObject(hit) <- parsed
//  } yield println(hit)

//  println(took)
//  println(timedOut)
//  println(hits)

}

class Storage {


}

