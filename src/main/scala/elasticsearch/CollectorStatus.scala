package elasticsearch

import java.io.{File, FileWriter}

import scala.io.Source

class CollectorStatus(file: String) {

  private val filePath: String = "./src/main/resources/" + file

  def init: Unit = {
    this.clear

    val writer = new FileWriter(filePath, true)
    writer.close()
  }

  def setCompleted(identifier: String): Unit = {
    val writer = new FileWriter(filePath, true)
    writer.write(s"$identifier\n")
    writer.close()
  }

  def isCompleted(parts: List[String]): Boolean = {
    val file = Source.fromFile(filePath)
    val completed = (for(line <- file.getLines) yield line).toList
    file.close
    completed.distinct.size >= parts.distinct.size
  }

  def clear: Unit = new File(filePath).delete()

  init
}
