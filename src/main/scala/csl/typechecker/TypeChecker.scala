package csl.typechecker

import csl.ast.{Detector, DetectorElement, Find, Variable}

class TypeChecker {

  var variables: Map[String, Variable] = Map.empty
  var errors: List[Error] = List.empty
  var warnings: List[Warning] = List.empty

  def check(detector: Detector): Unit = check(detector.body)

  def check(body: List[DetectorElement]): Unit = {
    filterVariables(body).foreach(checkVariable)

    if (multipleFindDefinitions(body)) {
      addError(Error(s"Multiple find definitions found.", None))
    } else {
      body.collectFirst{ case f: Find => f } match {
        case Some(f) => checkFind(f)
        case None => addError(Error(s"No find definition found.", None))
      }
    }
  }

  def filterVariables(elements: List[DetectorElement]): List[Variable] = elements.collect{ case v: Variable => v }

  def multipleFindDefinitions(elements: List[DetectorElement]): Boolean = elements.collect{ case f: Find => f }.size > 1

  def checkVariable(v: Variable): Unit = {
    this.variables get v.name match {
      case Some(x) => addError(Error(s"Variable ${v.name} is already defined at line ${x.pos}", Some(v.pos)))
      case None => this.variables = this.variables + (v.name -> v)
    }
  }

  def checkFind(f: Find): Unit = {
    // TODO: Check Pattern
    val vars = f.pattern.variables
    val relation = f.pattern.relationKeys
    val position = Some(f.pattern.pos)

    // - Error: if no pattern.
    if (vars.isEmpty) {
      addError(Error("No pattern described", position))
    } else
    // - Error: Pattern exist out of more than 1 variable and no relation is given.
    if (vars.nonEmpty && relation.isEmpty) {
      addError(Error(s"No relation between request pattern described.", position))
    } else {
      // - All variables are defined?
      vars.map(x => this.variables getOrElse(x, addError(Error(s"Unknown request description variable $x.", position))))
    }

    //println(f.pattern)
    // TODO: Check Constraints
  }

  def addWarning(warning: Warning) = this.warnings = this.warnings :+ warning

  def hasWarnings: Boolean = this.warnings.nonEmpty

  def addError(error: Error) = this.errors = this.errors :+ error

  def hasErrors: Boolean = this.errors.nonEmpty

}
