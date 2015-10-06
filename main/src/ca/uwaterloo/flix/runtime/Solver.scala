package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.language.ast.TypedAst.Constraint.Rule
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.Head
import ca.uwaterloo.flix.language.ast.{Name, TypedAst}

import scala.collection.mutable
import scala.collection.immutable

class Solver(root: TypedAst.Root) {

  val env = mutable.Map.empty[Name.Resolved, List[List[Value]]]

  val worklist = mutable.ListBuffer.empty[(Name.Resolved, List[Value])]

  def solve(): Unit = {

    for (fact <- root.facts) {
      val name = fact.head.name
      val values = fact.head.terms map Interpreter.evalHeadTerm
      newFact(name, values)
    }

    while (worklist.nonEmpty) {
      val item = worklist.head
      worklist -= item
      val (name, row) = item

      val rules = dependencies(name)

      for (rule <- rules) {
        eval(rule, row)
      }
    }
  }

  def newFact(name: Name.Resolved, row: List[Value]): Unit = {
    val table = env.getOrElse(name, List.empty)

    val rowExists = table.contains(row)

    if (!rowExists) {
      env += (name -> (row :: table))
      worklist += ((name, row))
    }
  }

  def eval(rule: Rule, row: List[Value]) = {
    // TODO: For now we ignore `row` and just re-evaluate the entire rule.
    evalBody(rule, Map.empty[String, Value])
  }

  

  def evalBody(rule: Rule, env0: Map[String, Value]): Unit = {
    
    def visit(p: TypedAst.Predicate.Body, env: List[Map[String, Value]]): List[Map[String, Value]] = ???
    
    val env2 = rule.body.foldLeft(List(env0)) {
      case (env1, p) => visit(p, env1)
    }
    
    for (env <- env2) {
      evalHead(rule.head, env)  
    }
  }

  def evalHead(head: Head, env: Map[String, Value]) = {
    val row = head.terms map Interpreter.evalHeadTerm
    newFact(head.name, row)
  }

  /**
   * Returns all rules where the given `name` occurs in a body predicate of the rule.
   */
  def dependencies(name: Name.Resolved): List[TypedAst.Constraint.Rule] = root.rules.filter {
    case rule => rule.body.exists(p => p.name == name)
  }

}
