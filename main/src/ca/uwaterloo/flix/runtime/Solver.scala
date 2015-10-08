package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.language.ast.TypedAst.Constraint.Rule
import ca.uwaterloo.flix.language.ast.TypedAst.Directive.{AssertRule, AssertFact}
import ca.uwaterloo.flix.language.ast.TypedAst.Term
import ca.uwaterloo.flix.language.ast.TypedAst.Term.Body
import ca.uwaterloo.flix.language.ast.TypedAst._
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
      val name = fact.head.asInstanceOf[TypedAst.Predicate.Head.Relation].name // TODO: Cast
      val values = fact.head.asInstanceOf[TypedAst.Predicate.Head.Relation].terms map (term => Interpreter.evalHeadTerm(term, Map.empty)) // TODO: Cast
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

    directives()
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
  def evalHead(rule: Rule, env0: Map[String, Value]): Unit = rule.head match {
    case p: Predicate.Head.Relation =>
      val row = p.terms map (t => Interpreter.evalHeadTerm(t, env0))
      newFact(p.name, row)
    case p: Predicate.Head.Print =>
      val values = p.terms.collect {
        case Term.Head.Var(ident, _, _) => ident.name + " => " + pretty(env0(ident.name))
      }
      println(values.mkString(", "))
    case p: Predicate.Head.Write =>
      println("Write Not supported yet.")
  }

  /**
   * Evaluates the body of the given `rule` under the given initial environment `env0`.
   */
  def evalBody(rule: Rule, env0: Map[String, Value]): Unit = {
    /**
     * Extend the given environment `env` according to the given predicate `p`.
     */
    def visit(p: TypedAst.Predicate.Body, env: List[Map[String, Value]]): List[Map[String, Value]] = p match {
      case r: Predicate.Body.Relation => {
        // TODO: Replace by faster join algorithm.
        val table = database(p.asInstanceOf[TypedAst.Predicate.Body.Relation].name) // TODO: Cast

        table flatMap {
          case row => unifyRow(row, p.asInstanceOf[TypedAst.Predicate.Body.Relation].terms) match {
            // TODO: Cast
            case None => List.empty
            case Some(m) => extend(env, m)
          }
        }
      }

      case Predicate.Body.NotEqual(ident1, ident2, _, _) =>
        env flatMap {
          case m =>
            val v1 = m(ident1.name)
            val v2 = m(ident2.name)
            if (v1 == v2)
              None
            else
              Some(m)
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
    case rule => rule.body.exists {
      case r: Predicate.Body.Relation => name == r.name
      case _ => false
    }
  }


  /**
   * Processes all directives in the program.
   */
  def directives(): Unit = {
    for (directive <- root.directives.prints) {
      print(directive)
    }

    for (directive <- root.directives.assertedFacts) {
      checkAssertedFact(directive)
    }

    for (directive <- root.directives.assertedRules) {
      checkAssertedRule(directive)
    }
  }

  /**
   * Evaluates the given print `directive`.
   */
  def print(directive: Directive.Print): Unit = {
    val relation = root.relations(directive.name)
    val table = database(directive.name)
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

  /**
   * Verifies that the given asserted fact `d` holds in the minimal model.
   */
  def checkAssertedFact(d: Directive.AssertFact) = d.fact.head match {
    case Predicate.Head.Relation(name, terms, _, _) =>
      val row = terms map (t => Interpreter.evalHeadTerm(t, Map.empty))
      val table = database(name)
      if (!(table contains row)) {
        throw new RuntimeException("Assertion violation!")
      }
    case _ => // nop
  }

  /**
   * Verifies that the given asserted rule `d` holds in the minimal model.
   */
  def checkAssertedRule(d: Directive.AssertRule) = {
    // TODO:
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
