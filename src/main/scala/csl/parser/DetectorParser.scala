package csl.parser

import csl.ast._

import scala.util.parsing.combinator.JavaTokenParsers

class DetectorParser extends JavaTokenParsers
{
  override val whiteSpace = """(\s|//.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r

  def detector: Parser[Detector] = positioned("detector" ~> label ~ detectorBody ^^ {
    case l ~ b => Detector(l, b)
  })

  def label: Parser[String] = stringLiteral ^^ {
    s => s.substring(1, s.length - 1).replace("\\", "")
  }

  def detectorBody: Parser[List[DetectorElement]] = "{" ~> rep(find | variable) <~ "}"

  def find: Parser[Find] = positioned("find" ~ "{" ~> (pattern ~ relation) <~ "}" ^^ {
    case p ~ r => Find(p, r)
  })

  def pattern: Parser[Pattern] = positioned("pattern" ~ "{" ~> repsep(patternElement, "->") <~ "}" ^^ {
    case elements => Pattern(elements)
  })

  def patternElement: Parser[PatternElement] = (wildcard | in | not | repeat | identifier)

  def identifier: Parser[Identifier] = positioned(ident ^^ Identifier)

  def in: Parser[In] = positioned("in" ~ "(" ~> repsep(identifier, ",") <~ ")" ^^ In)

  def not: Parser[Not] = positioned("not" ~ "(" ~> repsep(identifier, ",") <~ ")" ^^ Not)

  def repeat: Parser[Repeat] = positioned("repeat" ~ "(" ~> identifier ~ ("," ~> wholeNumber) <~ ")" ^^ {
    case (id ~ times) => Repeat(id, times.toInt)
  })

  def wildcard: Parser[Wildcard] = positioned(("?" | "*") ^^ {
    case "?" => SingleWildcard()
    case "*" => MultiWildcard()
  })

  def relation: Parser[Relation] = positioned("with" ~ "relation" ~ "on" ~ "{" ~> repsep(propertyKey, "and") <~ "}" ^^ {
    case keys => Relation(keys.filterNot(_.forall(_.equals(""))))
  })

  def variable: Parser[RequestDefinition] = positioned((ident <~ "=") ~ request ~ ("=>" ~> response) ^^ {
    case v ~ req ~ res  => RequestDefinition(v, req, res)
  })

  def request: Parser[ObjectValue] = "request" ~> objectValue

  def response: Parser[ObjectValue] = "response" ~> objectValue

  def property: Parser[Property] = propertyKey ~ value ^^ {
    case k ~ v => Property(k, v)
  }

  def propertyKey: Parser[String] = repsep(propertyKeyPart, ".") ^^  {
    case s => s.mkString(".")
  }

  def propertyKeyPart: Parser[String] = """[a-zA-Z0-9-]{1,64}""".r

  def value: Parser[Value] = objectValue | (":" ~> (regexValue | dateValue | stringValue | numberValue))

  def regexValue: Parser[RegexValue] = ("\"\"\"" + """.*""" + "\"\"\"").r ^^ {
    case s: String => RegexValue(s.substring(3, s.length - 3))
  }

  def stringValue: Parser[StringValue] = ("\"" + """.*""").r ^^ {
    case s: String => StringValue(removeQuotes(s))
  }

  def numberValue: Parser[NumberValue] = (decimalNumber | wholeNumber) ^^ {
    case n => NumberValue(n.toDouble)
  }

  def dateValue: Parser[DateValue] = ("\"" + """[1-9][0-9]{3}-(0[0-9]|1[0-2])-(3[0-1]|[1-2][0-9]|0[0-9])T(2[0-4]|1[0-9]|0[0-9]):([0-5][0-9]|60):([0-5][0-9]|60)[+-](0[1-9]|1[0-2]):(00|15|30|45)""" + "\"").r ^^ {
    case s: String => DateValue(removeQuotes(s))
  }

  def objectValue: Parser[ObjectValue] = "{" ~> rep(property) <~ "}" ^^ ObjectValue

  private def removeQuotes(s: String): String = {
    s.substring(1, s.length - 1).replace("\\", "")
  }

}
