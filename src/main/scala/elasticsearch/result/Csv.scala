package elasticsearch.result

import java.io.FileWriter

import elasticsearch.ast.Document

class Csv(documents: List[Document], keys: List[String]) {

  type Row = String

  private val ColumnSeperator = ","
  private val LineSeperator = "\n"

  private var rows: List[String] = List.empty

  private def createHeader(keys: List[String]): Row = {
    val k = "Document ID" :: keys
    k.mkString(ColumnSeperator)
  }

  private def createDocumentRow(document: Document, keys: List[String]): Row = {
    var row: List[String] = List(document._id)
    keys.foreach(k => {
      document.source(k) match {
        case Some(v) => row = row :+ v.toString
        case None => row = row :+ ""
      }
    })
    row.mkString(ColumnSeperator)
  }

  def save(path: String): Unit = {
    rows = rows :+ createHeader(keys)
    rows = rows ::: documents.map(d => createDocumentRow(d, keys).toString)

    val writer = new FileWriter(path, true)
    writer.write(rows.mkString(LineSeperator) + "\n")
    writer.close()
  }
}
