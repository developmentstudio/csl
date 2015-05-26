package csl

import wabisabi.Client

package object elasticsearch {

  type Query = String

  val client = new Client("http://localhost:9200")

}
