package csl.elasticsearch

import csl.ast._
import csl.storage.{Document, ResponseStorage}

class PatternDetector(detector: Detector) {

  private val pattern = detector.find.pattern


//  private var totalMatches = 0
//  private var matchedDocuments: List[Document] = List.empty

//  def detect: Unit = {
//    val relations = ResponseStorage.getRelations(pattern)
//    relations.foreach(relation => {
//      val documents = ResponseStorage.getDocumentsBy(relation)
//      println(documents)
//      detect(documents)
//    })
//    println("Found pattern " + pattern.elements.mkString(" -> ") + ": " + totalMatches + " times.")
//    println("Total documents related to result: " + matchedDocuments.distinct.length)
//  }

//  var documents: List[Document] = List.empty

  def detect(documents: List[Document]): Unit = {

    var documentIndex = 0
    var elementIndex = 0
    var startMatch = -1
    var endMatch = -1

    var multiWildcardActive = false




    while(isValidDocumentIndex(documents, documentIndex + 1)) {

      val patternElement = pattern.elements(elementIndex)
      if (matchesPatternElement(documents, documentIndex, patternElement)) {
        if (startMatch == -1) {
          startMatch = documentIndex
        }

        documentIndex += 1

        // TODO: Check end of pattern.

        if (isMultiWildcard(patternElement)) {
          // TODO: Implement method isThereARemainingNonWildcardInPattern()
          // TODO: Implement method getNextNonWildcardIndex()
          if (isThereARemainingNonWildcardInPattern(pattern.elements, elementIndex)) {
            elementIndex = getNextNonWildcardIndex(elementIndex)
            multiWildcardActive = true
          } else {
            // Match!!
            // TODO: Save matching documents.
            // TODO: Reset counters and flags.
          }
        } else {
          if (isThereARemainingNonWildcardInPattern(pattern.elements, elementIndex)) {
            elementIndex += 1
            multiWildcardActive = false
          } else {
            // Match!!
            // TODO: Save matching documents.
            // TODO: Reset counters and flags.
          }
        }

      } else
      if (multiWildcardActive) {
        documentIndex += 1
      } else {
        if (startMatch >= 0) {
          documentIndex = startMatch + 1
        } else {
          documentIndex += 1
        }
        elementIndex = 0
        startMatch = -1
        endMatch = -1
      }

    }
  }

  def matchesPatternElement(documents: List[Document], documentIndex: Int, element: PatternElement): Boolean = {
    element match {
      case e: Identifier => matchesIdentifier(documents(documentIndex), e)
      case e: In => matchesIn(document, e)
      case e: Not => matchesNot(document, e)
      case e: Repeat => matchesRepeat(documents, documentIndex, e)
      case e: SingleWildcard => true
      case e: MultiWildcard => true
    }
  }


  def matchesIdentifier(document: Document, element: Identifier): Boolean = {
    document.hasLabel(element.name)
  }

  def matchesIn(document: Document, element: In): Boolean = {
    element.identifiers.exists(id => matchesIdentifier(document, id))
  }

  def matchesNot(document: Document, element: Not): Boolean = {
    !element.identifiers.exists(id => matchesIdentifier(document, id))
  }

  def matchesRepeat(documents: List[Document], documentIndex: Int, element: Repeat): Boolean = {
    val firstIndex = documentIndex
    val lastIndex = firstIndex + element.times

    List.range(firstIndex, lastIndex).forall (i => {
      if (isValidDocumentIndex(documents, i)) {
        matchesIdentifier(documents(i), element.identifier)
      } else {
        false
      }
    })
  }

  def isValidDocumentIndex(documents: List[Document], documentIndex: Int): Boolean = documentIndex < documents.length

  def isValidPatternElementIndex(elements: List[PatternElement], elementIndex: Int): Boolean = elementIndex < elements.length

  def isMultiWildcard(element: PatternElement): Boolean = element match {
    case MultiWildcard() =>  true
    case _ => false
  }

}
