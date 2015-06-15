package csl.elasticsearch

import java.io.{BufferedReader, File, FileReader, FileWriter}

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

  def isCompleted(identifier: List[String]): Boolean = {
    var completed: List[String] = List.empty

    val reader = new BufferedReader(new FileReader(filePath));
    var line = reader.readLine();
    while (line != null) {
      completed = completed :+ line
      line = reader.readLine();
    }
    reader.close();

    completed.distinct.size == identifier.distinct.size
  }

  def clear: Unit = new File(filePath).delete()

  init
}
