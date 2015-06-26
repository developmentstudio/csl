package elasticsearch

import csl.ast._
import csl.elasticsearch.PatternDetector
import csl.storage.Document
import org.specs2.mutable.Specification

class PatterDetectorSpec extends Specification {

  val detector = Detector("Detector Name", List(
    RequestDefinition("A",
      ObjectValue(List.empty),
      ObjectValue(List.empty)
    ),
    RequestDefinition("B",
      ObjectValue(List.empty),
      ObjectValue(List.empty)
    ),
    Find(
      Pattern(
        List(
          In(List(Identifier("A"))),
          Not(List(Identifier("B"))),
          Repeat(Identifier("A"), 1)
        )
      ),
      Relation(List.empty)
    )
  ))

  val patternDetector = new PatternDetector(detector)
  patternDetector.documents = List(
    Document("20141016", "accesslog", "AU2utQeqH90A4sW5rfW0", "2014-10-15 09:25:38.0", List("A")),
    Document("20141016", "accesslog", "AU2utQjyH90A4sW5rfaG", "2014-10-15 09:25:58.0", List("A")),
    Document("20141016", "accesslog", "AU2utQuLH90A4sW5rfgH", "2014-10-15 09:26:35.0", List("A")),
    Document("20141016", "accesslog", "AU2utQvYH90A4sW5rfg8", "2014-10-15 09:26:41.0", List("A")),
    Document("20141016", "accesslog", "AU2utQvYH90A4sW5rfg9", "2014-10-15 09:26:41.0", List("A"))
  )

  "Pattern detector method matchesIdentifier" should {
    "match" in {
      val document = Document("", "", "", "", List("A", "B"))
      val element = Identifier("A")

      patternDetector.matchesIdentifier(document, element) mustEqual true
    }

    "no match" in {
      val document = Document("", "", "", "", List("A", "B"))
      val element = Identifier("C")

      patternDetector.matchesIdentifier(document, element) mustEqual false
    }
  }

  "Pattern detector method matchesIn" should {
    "match" in {
      val document = Document("", "", "", "", List("A", "B"))
      val element = In(List(Identifier("A"), Identifier("C")))

      patternDetector.matchesIn(document, element) mustEqual true
    }

    "no match" in {
      val document = Document("", "", "", "", List("A", "B"))
      val element = In(List(Identifier("C")))

      patternDetector.matchesIn(document, element) mustEqual false
    }
  }

  "Pattern detector method matchesNot" should {
    "match" in {
      val document = Document("", "", "", "", List("A", "B"))
      val element = Not(List(Identifier("C"), Identifier("D")))

      patternDetector.matchesNot(document, element) mustEqual true
    }

    "no match" in {
      val document = Document("", "", "", "", List("A", "B"))
      val element = Not(List(Identifier("B"), Identifier("C")))

      patternDetector.matchesNot(document, element) mustEqual false
    }
  }

  "Pattern detector method matchesRepeat" should {
    "match" in {
      val documents = List(
        Document("", "", "", "", List("A", "B")),
        Document("", "", "", "", List("A", "B")),
        Document("", "", "", "", List("A", "B")),
        Document("", "", "", "", List("A", "B")),
        Document("", "", "", "", List("A", "B")),
        Document("", "", "", "", List("A", "B")),
        Document("", "", "", "", List("A", "B")),
        Document("", "", "", "", List("A", "B")),
        Document("", "", "", "", List("A", "B")),
        Document("", "", "", "", List("A", "B"))
      )
      val index = 0
      val element = Repeat(Identifier("B"), 5)

      patternDetector.matchesRepeat(documents, index, element) mustEqual true
    }

    "no match" in {
      val documents = List(
        Document("", "", "", "", List("A", "B")),
        Document("", "", "", "", List("A", "B")),
        Document("", "", "", "", List("A", "B")),
        Document("", "", "", "", List("A", "B")),
        Document("", "", "", "", List("A", "C")),
        Document("", "", "", "", List("A", "B")),
        Document("", "", "", "", List("A", "B")),
        Document("", "", "", "", List("A", "B")),
        Document("", "", "", "", List("A", "B")),
        Document("", "", "", "", List("A", "B"))
      )
      val index = 0
      val element = Repeat(Identifier("B"), 10)

      patternDetector.matchesRepeat(documents, index, element) mustEqual false
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
        Document("", "", "", "", List.empty)
      )
      val index = 0

      patternDetector.isValidDocumentIndex(documents, index) mustEqual true
    }
  }

  // TODO: Write missing tests.

}
