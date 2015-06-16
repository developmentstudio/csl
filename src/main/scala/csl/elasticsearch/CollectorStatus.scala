package csl.elasticsearch

import java.io.{File, FileWriter}

import scala.io.Source

class CollectorStatus(file: String) {

  private val filePath: String = "./src/main/resources/" + file

  def init = {
    val writer = new FileWriter(filePath, true)
    writer.close()
  }

  def setCompleted(identifier: String) = {
    val writer = new FileWriter(filePath, true)
    writer.write(s"$identifier\n")
    writer.close()
  }

  def isCompleted(collectionParts: List[String]): Boolean = {
    val lineIterator = Source.fromFile(filePath).getLines
    val completed = (for(line <- lineIterator) yield line).toList
    completed.distinct.size == collectionParts.distinct.size
  }

  def clear: Unit = new File(filePath).delete()

  init
}
