package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.Flix
import ca.uwaterloo.flix.language.Compiler
import ca.uwaterloo.flix.language.ast.TypedAst.Term
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast.{SourceLocation, Name, TypedAst}
import ca.uwaterloo.flix.runtime.datastore.DataStore
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Solver {

  /**
   * A case class representing a solver context.
   */
  case class SolverContext(root: TypedAst.Root)

}

/**
 * A solver based on semi-naive evaluation.
 */
class Solver(implicit sCtx: Solver.SolverContext) {

  /**
   * A common super-type for solver errors.
   */
  sealed trait SolverError extends Flix.FlixError {
    /**
     * Returns a human readable string representation of the error.
     */
    def format: String
  }

  object SolverError {

    implicit val consoleCtx = Compiler.ConsoleCtx

    /**
     * An error raised to indicate that the asserted fact does not hold in the minimal model.
     *
     * @param loc the location of the asserted fact.
     */
    case class AssertedFactViolated(loc: SourceLocation) extends SolverError {
      val format =
        s"""${consoleCtx.blue(s"-- SOLVER ERROR -------------------------------------------------- ${loc.formatSource}")}
           |
            |${consoleCtx.red(s">> Assertion violated: The asserted fact does not hold in the minimal model!")}
           |
           |${loc.underline}
         """.stripMargin
    }

  }

  /**
   * The primary data store that holds all relations and lattices.
   */
  val dataStore = new DataStore()

  /**
   * The work list of pending predicate names and their associated values.
   */
  val worklist = mutable.Queue.empty[(Constraint.Rule, mutable.Map[String, Value])]

  /**
   * Solves the current Flix program.
   */
  def solve(): Model = {
    // measure the time elapsed.
    val t = System.nanoTime()

    // evaluate all facts.
    for (fact <- sCtx.root.facts) {
      evalHead(fact.head, Map.empty, enqueue = false)
    }

    // add all rules to the worklist (under empty environments).
    for (rule <- sCtx.root.rules) {
      worklist.enqueue((rule, mutable.Map.empty))
    }

    // iterate until fixpoint.
    while (worklist.nonEmpty) {
      // extract fact from the worklist.
      val (rule, env) = worklist.dequeue()
      evalBody(rule, env)
    }

    // computed elapsed time.
    val elapsed = System.nanoTime() - t
    println(s"Successfully solved in ${elapsed / 1000000} msec.")

    // verify assertions.
    checkAssertions()

    // print some statistics.
    dataStore.stats()

    // construct the model.
    val relations = dataStore.relations.foldLeft(Map.empty[Name.Resolved, List[List[Value]]]) {
      case (macc, (name, relation)) =>
        val inner = relation.scan.toList.map(_.toList)
        macc + (name -> inner)
    }
    val lattices = dataStore.lattices.foldLeft(Map.empty[Name.Resolved, Map[List[Value], List[Value]]]) {
      case (macc, (name, lattice)) =>
        val inner = lattice.scan.map {
          case (keys, values) => (keys.toList, values.toList)
        }
        macc + (name -> inner.toMap)
    }

    Model(relations, lattices)
  }

  /**
   * Processes an inferred `fact` for the relation or lattice with the `name`.
   */
  def inferredFact(name: Name.Resolved, fact: Array[Value], enqueue: Boolean): Unit = sCtx.root.collections(name) match {
    case r: TypedAst.Collection.Relation =>
      val changed = dataStore.relations(name).inferredFact(fact)
      if (changed && enqueue) {
        worklist ++= dependencies(r.name, fact)
      }

    case l: TypedAst.Collection.Lattice =>
      val changed = dataStore.lattices(name).inferredFact(fact)
      if (changed && enqueue) {
        worklist ++= dependencies(l.name, fact)
      }
  }

  /**
   * Evaluates the given head predicate `p` under the given environment `env0`.
   */
  def evalHead(p: Predicate.Head, env0: Map[String, Value], enqueue: Boolean): Unit = p match {
    case p: Predicate.Head.Relation =>
      val terms = p.terms.toArray
      // TODO: Use new Array instead ofDim.
      val fact = Array.ofDim[Value](p.terms.length)
      for (i <- fact.indices) {
        fact(i) = Interpreter.evalHeadTerm(terms(i), sCtx.root, env0)
      }
      inferredFact(p.name, fact, enqueue)
    case p: Predicate.Head.Trace =>
      val row = p.terms map (t => Interpreter.evalHeadTerm(t, sCtx.root, env0).pretty)
      val out = "Trace(" + row.mkString(", ") + ")"
      Console.println(out)
    case p: Predicate.Head.Write => // NOP - used when the fixpoint has been found.
    case p: Predicate.Head.Error => // NOP - used when the fixpoint has been found.
  }


  /**
   * Evaluates the body of the given `rule` under the given initial environment `env0`.
   */
  def evalBody(rule: Constraint.Rule, env0: mutable.Map[String, Value]): Unit = {

    /**
     * Computes the cross product of all collections in the body.
     */
    def cross(ps: List[Predicate.Body.Relation], row: mutable.Map[String, Value]): Unit = ps match {
      case Nil =>
        // cross product complete, now filter
        filter(rule.filters, row)
      case Predicate.Body.Relation(name, terms, _, _) :: xs =>
        val collection = sCtx.root.collections(name) match {
          case r: Collection.Relation => dataStore.relations(name)
          case l: Collection.Lattice => dataStore.lattices(name)
        }

        val values = terms.map(t => eval(t, row.toMap))
        val offset2var = terms.zipWithIndex.foldLeft(Map.empty[Int, String]) {
          case (macc, (Term.Body.Var(ident, _, _), i)) => macc + (i -> ident.name)
          case (macc, _) => macc
        }

        for (row2 <- collection.lookup(values.toArray)) {
          val newRow = row.clone()
          for (i <- row2.indices) {
            offset2var.get(i) match {
              case None => // nop
              case Some(x) =>
                newRow += (x -> row2(i))
            }
          }
          cross(xs, newRow)
        }
    }

    /**
     * Filters the given `row` through all filter functions in the body.
     */
    @tailrec
    def filter(ps: List[Predicate.Body.Function], row: mutable.Map[String, Value]): Unit = ps match {
      case Nil =>
        // filter complete, now check disjointness
        disjoint(rule.disjoint, row)
      case Predicate.Body.Function(name, terms, _, _) :: xs =>
        val lambda = sCtx.root.constants(name)
        val args = terms.map(t => Interpreter.evalBodyTerm(t, row.toMap))
        val result = Interpreter.evalCall(lambda.exp, args, sCtx.root, row.toMap).toBool
        if (result)
          filter(xs, row)
    }

    /**
     * Filters the given `row` through all disjointness filters in the body.
     */
    @tailrec
    def disjoint(ps: List[Predicate.Body.NotEqual], row: mutable.Map[String, Value]): Unit = ps match {
      case Nil =>
        // rule body complete, evaluate the head.
        evalHead(rule.head, row.toMap, enqueue = true)
      case Predicate.Body.NotEqual(ident1, ident2, _, _) :: xs =>
        val value1 = row(ident1.name)
        val value2 = row(ident2.name)
        if (value1 != value2) {
          disjoint(xs, row)
        }
    }

    cross(rule.collections, env0)
  }

  /**
   * Evaluates the given body term `t` to a value.
   *
   * Returns `null` if the term is a free variable.
   */
  def eval(t: TypedAst.Term.Body, env: Map[String, Value]): Value = t match {
    case t: TypedAst.Term.Body.Wildcard => null
    case t: TypedAst.Term.Body.Var =>
      if (env contains t.ident.name)
        env(t.ident.name)
      else
        null
    case t: TypedAst.Term.Body.Lit => Interpreter.evalLit(t.lit)
  }

  /**
   * Returns all dependencies of the given `name` along with an environment.
   */
  def dependencies(name: Name.Resolved, fact: Array[Value]): Traversable[(Constraint.Rule, mutable.Map[String, Value])] = {

    def unify(terms: List[Term.Body], fact: Array[Value]): mutable.Map[String, Value] = {
      val env = mutable.Map.empty[String, Value]
      for ((term, value) <- terms zip fact) {
        term match {
          case t: Term.Body.Wildcard => // nop
          case t: Term.Body.Var =>
            env += (t.ident.name -> value)
          case t: Term.Body.Lit =>
            val literal = Interpreter.evalLit(t.lit)
            // NB: This assumes that the values are not lattice elements.
            if (literal != value) {
              return null
            }
        }
      }
      env
    }

    val result = ListBuffer.empty[(Constraint.Rule, mutable.Map[String, Value])]

    for (rule <- sCtx.root.rules) {
      for (body <- rule.collections) {
        if (name == body.name) {
          sCtx.root.collections(name) match {
            case r: TypedAst.Collection.Relation =>
              // unify all terms with their values.
              val env = unify(body.terms, fact)
              if (env != null) {
                result += ((rule, env))
              }
            case l: TypedAst.Collection.Lattice =>
              // unify only key terms with their values.
              val numberOfKeys = l.keys.length
              val env = unify(body.terms.take(numberOfKeys), fact.take(numberOfKeys))
              if (env != null) {
                result += ((rule, mutable.Map.empty))
              }
          }
        }
      }
    }

    result
  }


  /**
   * Checks all assertions.
   */
  def checkAssertions(): Unit = {
    // asserted rules
    val assertedFacts = @@(sCtx.root.directives.assertedFacts map checkAssertedFact)
    if (assertedFacts.hasErrors) {
      assertedFacts.errors.foreach(e => println(e.format))
    }

    // asserted facts
    val assertedRules = @@(sCtx.root.directives.assertedRules map checkAssertedRule)
    if (assertedRules.hasErrors) {
      assertedRules.errors.foreach(e => println(e.format))
    }
  }

  /**
   * Verifies that the given asserted fact `d` holds in the minimal model.
   */
  def checkAssertedFact(d: Directive.AssertFact): Validation[Boolean, SolverError] = {
    // TODO: Implement checkAssertedFact.
    false.toSuccess
  }

  /**
   * Verifies that the given asserted rule `d` holds in the minimal model.
   */
  def checkAssertedRule(d: Directive.AssertRule): Validation[Boolean, SolverError] = {
    // TODO: Implement checkAssertedRule.
    false.toSuccess
  }


}
