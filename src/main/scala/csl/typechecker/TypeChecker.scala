package csl.typechecker

import csl.ast._

class TypeChecker {

  var variables: Map[String, RequestDefinition] = Map.empty
  var errors: List[Error] = List.empty
  var warnings: List[Warning] = List.empty

  def check(detector: Detector): Unit = check(detector.body)

  def check(body: List[DetectorElement]): Unit = {
    filterVariables(body).foreach(checkRequestDefinition)

    if (multipleFindDefinitions(body)) {
      addError(Error(s"Multiple find definitions found.", None))
    } else {
      body.collectFirst{ case f: Find => f } match {
        case Some(f) => checkFind(f)
        case None => addError(Error(s"No find definition found.", None))
      }
    }
  }

  def filterVariables(elements: List[DetectorElement]): List[RequestDefinition] = elements.collect{ case v: RequestDefinition => v }

  def multipleFindDefinitions(elements: List[DetectorElement]): Boolean = elements.collect{ case f: Find => f }.size > 1

  def checkRequestDefinition(v: RequestDefinition): Unit = {
    this.variables get v.name match {
      case Some(x) => addError(Error(s"Variable ${v.name} is already defined at line ${x.pos}", Some(v.pos)))
      case None => this.variables = this.variables + (v.name -> v)
    }
  }

  def checkFind(f: Find): Unit = {
    val pattern = f.pattern
    val relation = f.relation

    if (!pattern.isDefined) {
      addError(Error("No pattern described", Some(pattern.pos)))
    } else
    if (pattern.hasMoreThanOneElement && !relation.isDefined) {
      addError(Error(s"No relation between request definitions described.", Some(relation.pos)))
    } else {
      checkIfPatternElementsAreDefined(f.pattern.elements)
    }

    // TODO: Check Constraints
  }

  def checkIfPatternElementsAreDefined(elements: List[PatternElement]): Unit = elements.foreach(checkIsDefined)

  def checkIsDefined(element: PatternElement): Unit = element match {
    case id: Identifier => checkIsDefined(id)
    case Repeat(id, _) => checkIsDefined(id)
    case In(ids) => checkIsDefined(ids)
    case Not(ids) => checkIsDefined(ids)
    case _: SingleWildcard =>
    case _: MultiWildcard =>
  }

  def checkIsDefined(ids: List[Identifier]): Unit = ids.foreach(checkIsDefined)

  def checkIsDefined(id: Identifier): Unit = this.variables getOrElse(id.name, addError(Error(s"Unknown request description variable ${id.name}.", Some(id.pos))))

  def addError(error: Error) = this.errors = this.errors :+ error

}
