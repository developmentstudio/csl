package csl.interpreter

import csl.ast.Variable
import csl.elasticsearch.FilterQueryGenerator
import csl.parser.Parser
import com.mysql.jdbc.jdbc2
import csl.search.QueryGenerator
import scala.io.Source


object Interpreter {

  def main(args: Array[String]) {
    val source = Source.fromFile("./src/main/resources/variable_1.csl").mkString

    parseSource(source) match {
      case Some(ast) =>

//        val generator = new QueryGenerator
//        val query = generator.generateMatchQuery(ast)
//        println(query)

        val filterGenerator = new FilterQueryGenerator
        val filter = filterGenerator.generateFilterQuery(ast)
        println(filter)


      case None => println("File not found.")
    }
  }

  def parseSource(source: String): Option[Variable] = {
    val parser = new Parser()

    parser.parseAll[Variable](parser.variable, source) match {
      case parser.Success(ast: Variable, _) => Some(ast)
      case parser.Failure(msg, next) => println("Parse failure at line " + next.pos + ": " + msg); None
      case parser.Error(msg, next) => println("Parse error at line " + next.pos + ": " + msg); None
    }
  }


//  def main (args: Array[String]) {



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

  //}
}
