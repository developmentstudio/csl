package elasticsearch

import csl.ast._
import elasticsearch.ast.Document
import elasticsearch.detector.PatternDetector
import org.specs2.mutable.Specification

class PatternDetectorSpec extends Specification {

  val detector = Detector("Detector Name", List(
    RequestDefinition("A",
      ObjectValue(List.empty),
      ObjectValue(List.empty)
    ),
    RequestDefinition("B",
      ObjectValue(List.empty),
      ObjectValue(List.empty)
    ),
    RequestDefinition("C",
      ObjectValue(List.empty),
      ObjectValue(List.empty)
    ),
    Find(
      Pattern(
        List(
          In(List(Identifier("A"), Identifier("B"))),
          Not(List(Identifier("B"))),
          SingleWildcard(),
          Identifier("C")
        )
      ),
      Relation(List.empty),
      None,
      None,
      None,
      None
    )
  ))

  val patternDetector = new PatternDetector(detector)

  "Pattern detector method matchesIdentifier" should {
    "match" in {
      val document = Document("", "", "", "", List("A", "B"), "")
      val element = Identifier("A")

      patternDetector.matchesIdentifier(document, element) mustEqual true
    }

    "no match" in {
      val document = Document("", "", "", "", List("A", "B"), "")
      val element = Identifier("C")

      patternDetector.matchesIdentifier(document, element) mustEqual false
    }
  }

  "Pattern detector method matchesIn" should {
    "match" in {
      val document = Document("", "", "", "", List("A", "B"), "")
      val element = In(List(Identifier("A"), Identifier("C")))

      patternDetector.matchesIn(document, element) mustEqual true
    }

    "no match" in {
      val document = Document("", "", "", "", List("A", "B"), "")
      val element = In(List(Identifier("C")))

      patternDetector.matchesIn(document, element) mustEqual false
    }
  }

  "Pattern detector method matchesNot" should {
    "match" in {
      val document = Document("", "", "", "", List("A", "B"), "")
      val element = Not(List(Identifier("C"), Identifier("D")))

      patternDetector.matchesNot(document, element) mustEqual true
    }

    "no match" in {
      val document = Document("", "", "", "", List("A", "B"), "")
      val element = Not(List(Identifier("B"), Identifier("C")))

      patternDetector.matchesNot(document, element) mustEqual false
    }
  }

  "Pattern detector method matchesRepeat" should {
    "match" in {
      val documents = List(
        Document("", "", "", "", List("A", "B"), ""),
        Document("", "", "", "", List("A", "B"), ""),
        Document("", "", "", "", List("A", "B"), ""),
        Document("", "", "", "", List("A", "B"), ""),
        Document("", "", "", "", List("A", "B"), ""),
        Document("", "", "", "", List("A", "B"), ""),
        Document("", "", "", "", List("A", "B"), ""),
        Document("", "", "", "", List("A", "B"), ""),
        Document("", "", "", "", List("A", "B"), ""),
        Document("", "", "", "", List("A", "B"), "")
      )
      val index = 0
      val element = Repeat(Identifier("B"), 5)

      patternDetector.matchesRepeat(documents, index, element) mustEqual true
    }

    "no match" in {
      val documents = List(
        Document("", "", "", "", List("A", "B"), ""),
        Document("", "", "", "", List("A", "B"), ""),
        Document("", "", "", "", List("A", "B"), ""),
        Document("", "", "", "", List("A", "B"), ""),
        Document("", "", "", "", List("A", "C"), ""),
        Document("", "", "", "", List("A", "B"), ""),
        Document("", "", "", "", List("A", "B"), ""),
        Document("", "", "", "", List("A", "B"), ""),
        Document("", "", "", "", List("A", "B"), ""),
        Document("", "", "", "", List("A", "B"), "")
      )
      val index = 0
      val element = Repeat(Identifier("B"), 10)

      patternDetector.matchesRepeat(documents, index, element) mustEqual false
    }
  }

  "Method detect" should {
    "detect pattern" in {
      val documents = List(
        Document("", "", "1", "", List("A"), ""),
        Document("", "", "2", "", List("C"), ""),
        Document("", "", "3", "", List("B"), ""),
        Document("", "", "4", "", List("C"), ""),
        Document("", "", "5", "", List("A"), ""),
        Document("", "", "6", "", List("C"), "")
      )
      val result = List(
        Document("", "", "1", "", List("A"), ""),
        Document("", "", "2", "", List("C"), ""),
        Document("", "", "3", "", List("B"), ""),
        Document("", "", "4", "", List("C"), ""),
        Document("", "", "5", "", List("A"), ""),
        Document("", "", "6", "", List("C"), "")
      )

      patternDetector.detect(documents) mustEqual result
    }
  }

  "Helper Methods" should {
    "detect an invalid document index" in {
      val documents = List.empty
      val index = 0

      patternDetector.isValidDocumentIndex(documents, index) mustEqual false
    }

    "detect a valid document index" in {
      val documents = List(
        Document("", "", "", "", List.empty, "")
      )
      val index = 0

      patternDetector.isValidDocumentIndex(documents, index) mustEqual true
    }

    "get pattern match related documents" in {
      val documents = List(
        Document("", "", "", "", List("A"), ""),
        Document("", "", "", "", List("B"), ""),
        Document("", "", "", "", List("C"), ""),
        Document("", "", "", "", List("D"), ""),
        Document("", "", "", "", List("E"), ""),
        Document("", "", "", "", List("F"), "")
      )
      val firstIndex = 2
      val lastIndex = 4
      val result = List(
        Document("", "", "", "", List("C"), ""),
        Document("", "", "", "", List("D"), ""),
        Document("", "", "", "", List("E"), "")
      )

      patternDetector.getPatternMatchRelatedDocuments(documents, firstIndex, lastIndex) mustEqual result
    }

    "see that their is a remaining non wildcard in pattern" in {
      val elements = List(
        Identifier("A"),
        MultiWildcard(),
        Identifier("C")
      )
      val elementIndex = 1

      patternDetector.isThereARemainingNonWildcardInPattern(elements, elementIndex) mustEqual true
    }

    "see that their no remaining non wildcard in pattern" in {
      val elements = List(
        Identifier("A"),
        MultiWildcard(),
        SingleWildcard()
      )
      val elementIndex = 1

      patternDetector.isThereARemainingNonWildcardInPattern(elements, elementIndex) mustEqual false
    }

    "get index of next non wildcard in pattern" in {
      val elements = List(
        Identifier("A"),
        MultiWildcard(),
        Identifier("C")
      )
      val elementIndex = 1

      patternDetector.getNextNonWildcardIndex(elements, elementIndex) mustEqual 2
    }

  }




  // TODO: Write missing tests.

}
