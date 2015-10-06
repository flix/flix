package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.language.ast.TypedAst.Constraint.Rule
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.Head
import ca.uwaterloo.flix.language.ast.TypedAst.Term
import ca.uwaterloo.flix.language.ast.TypedAst.Term.Body
import ca.uwaterloo.flix.language.ast.{Name, TypedAst}
import ca.uwaterloo.flix.util.AsciiTable

import scala.collection.mutable

class Solver(root: TypedAst.Root) {

  val database = mutable.Map.empty[Name.Resolved, List[List[Value]]]

  val worklist = mutable.Queue.empty[(Name.Resolved, List[Value])]

  /**
   * Solves the Flix program.
   */
  def solve(): Unit = {
    // adds all facts to the database.
    for (fact <- root.facts) {
      val name = fact.head.name
      val values = fact.head.terms map (term => Interpreter.evalHeadTerm(term, Map.empty))
      newFact(name, values)
    }

    // iterate until fixpoint.
    while (worklist.nonEmpty) {
      // extract fact from the worklist.
      val (name, row) = worklist.dequeue()

      // re-evaluate all dependencies.
      val rules = dependencies(name)
      for (rule <- rules) {
        // TODO: Use `row` as the initial environment to speedup computation.
        evalBody(rule, Map.empty)
      }
    }

    printDatabase()
  }

  /**
   * Adds the given `row` as a fact in the database for the relation with the given `name`.
   *
   * Adds the new fact to the worklist if the database was changed.
   */
  def newFact(name: Name.Resolved, row: List[Value]): Unit = {
    val table = database.getOrElse(name, List.empty)

    // TODO: Must take lattice semantics into account.
    val rowExists = table.contains(row)
    if (!rowExists) {
      database += (name -> (row :: table))
      worklist += ((name, row))
    }
  }

  /**
   * Evaluates the head of the given `rule` under the given environment `env0`.
   */
  def evalHead(rule: Rule, env0: Map[String, Value]) = {
    val row = rule.head.terms map (term => Interpreter.evalHeadTerm(term, env0))
    newFact(rule.head.name, row)
  }

  /**
   * Evaluates the body of the given `rule` under the given initial environment `env0`.
   */
  def evalBody(rule: Rule, env0: Map[String, Value]): Unit = {
    /**
     * Extend the given environment `env` according to the given predicate `p`.
     */
    def visit(p: TypedAst.Predicate.Body, env: List[Map[String, Value]]): List[Map[String, Value]] = {
      // TODO: Replace by faster join algorithm.
      val table = database(p.name)

      table flatMap {
        case row => unifyRow(row, p.terms) match {
          case None => List.empty
          case Some(m) => extend(env, m)
        }
      }
    }

    // fold the environment over every rule in the body.
    val envs = rule.body.foldLeft(List(env0)) {
      case (env1, p) => visit(p, env1)
    }

    // evaluate the head predicate for every satisfying environment. 
    for (env <- envs) {
      evalHead(rule, env)
    }
  }

  /**
   * Unifies the given `row` with the given terms.
   */
  def unifyRow(row: List[Value], terms: List[Body]): Option[Map[String, Value]] = {
    assert(row.length == terms.length)

    (row zip terms).foldLeft(Option(Map.empty[String, Value])) {
      case (None, (value, term)) => None
      case (Some(macc), (value, term)) => term match {
        case Term.Body.Wildcard(tpe, loc) => Some(macc)
        case Term.Body.Var(ident, tpe, loc) => macc.get(ident.name) match {
          case None => Some(macc + (ident.name -> value))
          case Some(otherValue) =>
            if (value != otherValue)
              None
            else
              Some(macc)
        }
        case Term.Body.Lit(lit, tpe, loc) =>
          val otherValue = Interpreter.evalLit(lit)
          if (value != otherValue)
            None
          else
            Some(macc)
      }
    }
  }

  /**
   * Extends the given environments `envs` with the environment `m`.
   */
  def extend(envs: List[Map[String, Value]], m: Map[String, Value]): List[Map[String, Value]] = {
    envs flatMap {
      case env =>
        m.foldLeft(Option(env)) {
          case (None, _) => None
          case (Some(macc), (key, value)) => macc.get(key) match {
            case None => Some(macc + (key -> value))
            case Some(otherValue) =>
              if (value != otherValue)
                None
              else
                Some(macc)
          }
        }
    }
  }

  /**
   * Returns all rules where the given `name` occurs in a body predicate of the rule.
   */
  def dependencies(name: Name.Resolved): List[TypedAst.Constraint.Rule] = root.rules.filter {
    case rule => rule.body.exists(p => p.name == name)
  }


  def printDatabase(): Unit = {
    for ((name, relation) <- root.relations) {
      val table = database(name)
      val cols = relation.attributes.map(_.ident.name)
      val ascii = new AsciiTable().withCols(cols: _*)
      for (row <- table) {
        ascii.mkRow(row map pretty)
      }

      Console.println(relation.name)
      ascii.write(System.out)
      Console.println()
      Console.println()
    }
  }

  // TODO: Move somewhere. Decide where
  def pretty(v: Value): String = v match {
    case Value.Unit => "()"
    case Value.Bool(b) => b.toString
    case Value.Int(i) => i.toString
    case Value.Str(s) => s.toString
    case Value.Tag(enum, tag, value) => enum + "." + tag + pretty(value)
    case Value.Tuple(elms) => "(" + (elms map pretty) + ")"
    case Value.Closure(_, _, _) => ??? // TODO: WHAT?
  }

}
