package csl.elasticsearch

import csl.ast.Detector
import csl.storage.{Document, ResponseStorage}

class PatternDetector(detector: Detector) {

  private val relations = ResponseStorage.getRelations(detector.find.pattern)

  private var totalMatches = 0
  private var matchedDocuments: List[Document] = List.empty


  def detect: Unit =
  {
    this.relations.foreach(relation => {
      val documents = ResponseStorage.getDocumentsBy(relation)
      println(documents.length) // Debug: Totaal van al deze lengtes is totaal 115553 en komt overeen met het totaal aantal documenten in de database. Probleem ligt dus niet hier.
      detect(documents)
    })
    println("Found pattern " + detector.find.pattern.variables.mkString(" -> ") + ": " + totalMatches + " times.")
    println("Total documents related to result: " + matchedDocuments.distinct.length)
  }

  def detect(document: List[Document]): Unit =
  {
    val part = detector.find.pattern.variables
    var di = 0
    var pi = 0
    var startPatternMatch = -1

    while (di <= document.length - 1) {

      if (document(di).hasLabel(part(pi))) {
        if (startPatternMatch == -1) {
          startPatternMatch = di
        }
        if (pi == part.length - 1) {
          totalMatches += 1

          var endPatternMatch = di
          // Set indices.
          di = startPatternMatch + 1
          pi = 0
          // Add matched documents.
          while (startPatternMatch <= endPatternMatch) {
            matchedDocuments = matchedDocuments :+ document(startPatternMatch)
            startPatternMatch += 1
          }
          // Reset pattern match starting position.
          startPatternMatch = -1
          endPatternMatch = -1
        } else {
          di += 1
          pi += 1
        }
      } else {
        if (startPatternMatch > 0) {
          di = startPatternMatch + 1
        } else {
          di += 1
        }
        pi = 0
        startPatternMatch = -1
      }

    }
  }



}

