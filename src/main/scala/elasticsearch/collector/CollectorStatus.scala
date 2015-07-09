package elasticsearch.collector

import java.io.{File, FileWriter}

import scala.io.Source

class CollectorStatus {

  private val file: File = File.createTempFile("collector-status-", ".tmp")

  def init: Unit = {
    this.clear
    val writer = new FileWriter(file, true)
    writer.close()
  }

  def setCompleted(identifier: String): Unit = {
    val writer = new FileWriter(file, true)
    writer.write(s"$identifier\n")
    writer.close()
  }

  def isCompleted(parts: List[String]): Boolean = {

    val f = Source.fromFile(file)
    val completed = (for(line <- f.getLines) yield line).toList
    f.close
    completed.distinct.size >= parts.distinct.size
  }

  def clear: Unit = file.delete()

  init
}
