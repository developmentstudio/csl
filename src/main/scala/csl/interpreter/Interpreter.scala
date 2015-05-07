package csl.interpreter

import scala.concurrent.ExecutionContext.Implicits.global

import wabisabi._
import org.json4s._
import org.json4s.jackson.JsonMethods._


import com.mysql.jdbc.jdbc2




object Interpreter {
  def main (args: Array[String]) {



    // Elastic Search
//    val client = new Client("http://localhost:9200")
//    client.search("20141016", "").onSuccess { case ground =>
//      val body = ground.getResponseBody()
//      val result = parse(body)
//      println(result)
//    }

    // MySQL
//    val url = "jdbc:mysql://127.0.0.1/local_system_dashboard?autoReconnect=true&useUnicode=true&characterEncoding=UTF-8"
//    val user = "root"
//    val driverString = "com.mysql.jdbc.Driver"
//    val db = Database.forURL(url, user, driver=driverString)
//    println(db)
//
//
//
//    class Customers(tag: Tag) extends Table[(Int, String)](tag, "CUSTOMERS") {
//      def id = column[Int]("customer_id", O.PrimaryKey, O.AutoInc)
//      def name = column[String]("name")
//      def * = (id, name)
//    }
//    val customers = TableQuery[Customers]

//    println("Coffees:")
//    db.run(customers.result).map(_.foreach {
//      case (name, supID, price, sales, total) =>
//        println("  " + name + "\t" + supID + "\t" + price + "\t" + sales + "\t" + total)
//    })


    //for (c <- customers) println(c)
    //println(customers)


//
//
//
//    for {
//      c <- customers
//    } println(c)

  }
}
