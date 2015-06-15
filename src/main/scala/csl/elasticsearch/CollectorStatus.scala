package csl.elasticsearch

import java.io.{BufferedReader, File, FileReader, FileWriter}

import csl.ast.Variable

class CollectorStatus {

  private val filePath: String = "./src/main/resources/log.txt"

  def init = {
    val writer = new FileWriter(filePath, true)
    writer.close()
  }

  def setCompleted(variable: Variable) = {
    val writer = new FileWriter(filePath, true)
    writer.write(s"${variable.name}\n")
    writer.close()
  }

  def isCompleted(variables: List[String]): Boolean = {
    var completed: List[String] = List.empty

    val reader = new BufferedReader(new FileReader(filePath));
    var line = reader.readLine();
    while (line != null) {
      completed = completed :+ line
      line = reader.readLine();
    }
    reader.close();

    completed.distinct.size == variables.distinct.size
  }

  def clear: Unit = new File(filePath).delete()

  init
}
