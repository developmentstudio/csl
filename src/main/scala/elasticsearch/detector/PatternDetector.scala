package elasticsearch.detector

import csl.ast.{Detector, Identifier, In, MultiWildcard, Not, PatternElement, Repeat, SingleWildcard}
import elasticsearch.Storage
import elasticsearch.ast.Document

class PatternDetector(detector: Detector) {

  private val pattern = detector.find.pattern
  private var totalMatchesFound = 0

  def detect: List[Document] = {
    val relations = Storage.getRelations(pattern)
    if (detector.find.pattern.hasMoreThanOneElement) {
      relations.flatMap(relation => detect(Storage.getDocumentsBy(relation)))
    } else {
      val documents = relations.flatMap(relation => Storage.getDocumentsBy(relation))
      totalMatchesFound += documents.length
      documents
    }
  }

  def totalMatches: Int = totalMatchesFound

  def detect(documents: List[Document]): List[Document] = {

    var documentIndex = 0
    var elementIndex = 0
    var startMatch = -1
    var multiWildcardActive = false
    var result: List[Document] = List.empty
    var matchCount= 0

    while(isValidDocumentIndex(documents, documentIndex)) {

      val patternElement = pattern.elements(elementIndex)
      if (matchesPatternElement(documents, documentIndex, patternElement)) {
        if (startMatch == -1) {
          startMatch = documentIndex
        }

        if (isMultiWildcard(patternElement)) {
          if (isThereARemainingNonWildcardInPattern(pattern.elements, elementIndex)) {
            elementIndex = getNextNonWildcardIndex(pattern.elements, elementIndex)
            multiWildcardActive = true
          } else {
            result = (result ::: getPatternMatchRelatedDocuments(documents, startMatch, documentIndex)).distinct
            matchCount += 1

            documentIndex = startMatch
            elementIndex = 0
            startMatch = -1
            multiWildcardActive = false
          }
        } else {
          if (isThereARemainingNonWildcardInPattern(pattern.elements, elementIndex)) {
            elementIndex += 1
            multiWildcardActive = false
          } else {
            result = (result ::: getPatternMatchRelatedDocuments(documents, startMatch, documentIndex)).distinct
            matchCount += 1

            documentIndex = startMatch
            elementIndex = 0
            startMatch = -1
            multiWildcardActive = false
          }
        }
        documentIndex += 1
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
      }

    }
    totalMatchesFound += matchCount
    result
  }

  def matchesPatternElement(documents: List[Document], documentIndex: Int, element: PatternElement): Boolean = {
    element match {
      case e: Identifier => matchesIdentifier(documents(documentIndex), e)
      case e: In => matchesIn(documents(documentIndex), e)
      case e: Not => matchesNot(documents(documentIndex), e)
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

  def getPatternMatchRelatedDocuments(documents: List[Document], firstIndex: Int, lastIndex: Int): List[Document] = {
    List.range(firstIndex, lastIndex + 1).map(i => documents(i))
  }

  def isThereARemainingNonWildcardInPattern(elements: List[PatternElement], elementIndex: Int): Boolean = {
    List.range(elementIndex + 1, elements.length).exists(i => elements(i) match {
      case e: Identifier => true
      case e: In => true
      case e: Not => true
      case e: Repeat => true
      case e: SingleWildcard => false
      case e: MultiWildcard => false
    })
  }

  def getNextNonWildcardIndex(elements: List[PatternElement], elementIndex: Int): Int = {
    List.range(elementIndex + 1, elements.length).find(i => elements(i) match {
      case e: Identifier => true
      case e: In => true
      case e: Not => true
      case e: Repeat => true
      case e: SingleWildcard => false
      case e: MultiWildcard => false
    }).getOrElse(throw new Exception(
      "No non wildcard pattern element found. Please use method isThereARemainingNonWildcardInPattern to check before calling this method."
    ))
  }

}
