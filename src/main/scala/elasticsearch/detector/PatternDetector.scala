package elasticsearch.detector

import java.sql.Timestamp

import csl.ast._
import elasticsearch.Storage
import elasticsearch.ast.Document

class PatternDetector(detector: Detector) {

  case class PatternMatch(relatedDocuments: List[Document], durationInMillis: Long, startDate: Timestamp, endDate: Timestamp)

  private val pattern = detector.find.pattern
  private var totalMatchesFound = 0

  def detect: List[Document] = {
    val relations = Storage.getRelations(pattern)
    relations.flatMap(relation => detect(Storage.getDocumentsBy(relation)))
  }

  def totalMatches: Int = totalMatchesFound

  def detect(documents: List[Document]): List[Document] = {
    val patternMatches = collectDocumentsMatchingPattern(documents)
    val result = filterDocumentsMatchingTimesPerInterval(patternMatches)

    totalMatchesFound += result.length

    convert(result)
  }

  def collectDocumentsMatchingPattern(documents: List[Document]): List[PatternMatch] = {
    var documentIndex = 0
    var elementIndex = 0
    var startMatch = -1
    var multiWildcardActive = false
    var result: List[PatternMatch] = List.empty

    def matchFound(fromDocumentIndex: Int, toDocumentIndex: Int): Unit = {
      // Add match to results.
      result = result :+ createPatternMatch(documents, fromDocumentIndex, toDocumentIndex)

      // Initialize counters for next search.
      documentIndex = startMatch
      elementIndex = 0
      startMatch = -1
      multiWildcardActive = false
    }

    while (isValidDocumentIndex(documents, documentIndex)) {
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
            matchFound(startMatch, documentIndex)
          }
        } else {
          if (isThereARemainingNonWildcardInPattern(pattern.elements, elementIndex)) {
            elementIndex += 1
            multiWildcardActive = false
          } else {
            matchFound(startMatch, documentIndex)
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

    List.range(firstIndex, lastIndex).forall(i => {
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
    case MultiWildcard() => true
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

  def createPatternMatch(documents: List[Document], fromDocumentIndex: Int, toDocumentIndex: Int): PatternMatch = {
    val durationInMillis: Long = {
      val start = documents(fromDocumentIndex)._timestamp
      val end = documents(toDocumentIndex)._timestamp
      end.getTime - start.getTime
    }
    val relatedDocuments = getPatternMatchRelatedDocuments(documents, fromDocumentIndex, toDocumentIndex)

    PatternMatch(
      relatedDocuments,
      durationInMillis,
      documents(fromDocumentIndex)._timestamp,
      documents(toDocumentIndex)._timestamp
    )
  }

  def filterDocumentsMatchingTimesPerInterval(patternMatches: List[PatternMatch]): List[PatternMatch] = {
    val interval = detector.find.interval
    val timesBeforeMatch = detector.find.timesBeforeMatch

    if (interval.isDefined && timesBeforeMatch.isDefined) {
      val intervalInMillis = interval.get.inMillis

      var patternMatchIndex = 0
      var startIntervalIndex = -1
      var startTimeInMillis: Long = 0
      var matchesInInterval = 0

      var result: List[PatternMatch] = List.empty

      def matchFound(fromIndex: Int, toIndex: Int): Unit = {
        result = (result ::: List.range(fromIndex, toIndex + 1).map(i => patternMatches(i))).distinct
      }

      def resetCounters(): Unit = {
        var nextIndex = startIntervalIndex + 1


        while (patternMatches(nextIndex).startDate.getTime == patternMatches(startIntervalIndex).startDate.getTime) {
          nextIndex += 1
        }

        patternMatchIndex = nextIndex
        startIntervalIndex = -1
        startTimeInMillis = 0
        matchesInInterval = 0
      }

      def checkMatchesTimesBeforeMatch(): Unit = {
        if (timesBeforeMatch.get.operator.isDefined) {
          timesBeforeMatch.get.operator.get match {
            case BiggerThanOperator() =>
              if (matchesInInterval > timesBeforeMatch.get.times) {
                matchFound(startIntervalIndex, patternMatchIndex - 1)
              }
            case SmallerThanOperator() =>
              if (matchesInInterval < timesBeforeMatch.get.times) {
                matchFound(startIntervalIndex, patternMatchIndex - 1)
              }
          }
        } else {
          if (matchesInInterval == timesBeforeMatch.get.times) {
            matchFound(startIntervalIndex, patternMatchIndex - 1)
          }
        }
      }

      while (isValidPatternMatchIndex(patternMatches, patternMatchIndex)) {
        val patternMatch = patternMatches(patternMatchIndex)

        if (startIntervalIndex == -1) {
          startIntervalIndex = patternMatchIndex
          startTimeInMillis = patternMatch.startDate.getTime
          matchesInInterval += 1
          patternMatchIndex += 1
        } else {
          if ((patternMatch.startDate.getTime - startTimeInMillis) <= intervalInMillis) {
            // Within interval
            matchesInInterval += 1
            patternMatchIndex += 1
          } else {
            checkMatchesTimesBeforeMatch()
            resetCounters()
          }
        }
      }
      checkMatchesTimesBeforeMatch()

      result
    } else {
      patternMatches
    }
  }

  def isValidPatternMatchIndex(patternMatches: List[PatternMatch], index: Int): Boolean = index < patternMatches.length

  def convert(patternMatches: List[PatternMatch]): List[Document] = patternMatches.flatMap(p => p.relatedDocuments).distinct

}
