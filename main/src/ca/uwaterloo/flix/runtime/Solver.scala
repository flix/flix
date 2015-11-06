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
        s"""${consoleCtx.blue(s"-- SOLVER ERROR -------------------------------------------------- ${loc.source.format}")}
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

    Model(sCtx.root, relations, lattices)
  }

  /**
    * Processes an inferred `fact` for the relation or lattice with the `name`.
    */
  def inferredFact(name: Name.Resolved, fact: Array[Value], enqueue: Boolean): Unit = sCtx.root.collections(name) match {
    case r: TypedAst.Collection.Relation =>
      val changed = dataStore.relations(name).inferredFact(fact)
      if (changed && enqueue) {
        dependencies(r.name, fact)
      }

    case l: TypedAst.Collection.Lattice =>
      val changed = dataStore.lattices(name).inferredFact(fact)
      if (changed && enqueue) {
        dependencies(l.name, fact)
      }
  }

  /**
    * Evaluates the given head predicate `p` under the given environment `env0`.
    */
  def evalHead(p: Predicate.Head, env0: Map[String, Value], enqueue: Boolean): Unit = p match {
    case p: Predicate.Head.Relation =>
      val terms = p.termsArray
      val fact = new Array[Value](p.arity)
      var i = 0
      while (i < fact.length) {
        fact(i) = Interpreter.evalHeadTerm(terms(i), sCtx.root, env0)
        i = i + 1
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

    // TODO: Need a static layout of variables and then implement `row` as an array.

    /**
      * Computes the cross product of all collections in the body.
      */
    def cross(ps: List[Predicate.Body.Collection], row: mutable.Map[String, Value]): Unit = ps match {
      case Nil =>
        // cross product complete, now filter
        filter(rule.filters, row)
      case (p: Predicate.Body.Collection) :: xs =>
        // lookup the relation or lattice.
        val collection = sCtx.root.collections(p.name) match {
          case r: Collection.Relation => dataStore.relations(p.name)
          case l: Collection.Lattice => dataStore.lattices(p.name)
        }

        // evaluate all terms in the predicate.
        val pat = new Array[Value](p.arity)
        var i = 0
        while (i < pat.length) {
          pat(i) = eval(p.termsArray(i), row)
          i = i + 1
        }

        // lookup all matching rows.
        for (matchedRow <- collection.lookup(pat)) {
          // copy the environment for every row.
          val newRow = row.clone()

          var i = 0
          while (i < matchedRow.length) {
            val varName = p.index2var(i)
            if (varName != null)
              newRow.put(varName, matchedRow(i)) // TODO: Don't use put. It returns an option.
            i = i + 1
          }

          // compute the cross product of the remaining
          // collections under the new environment.
          cross(xs, newRow)
        }
    }

    def loop(ps: List[Predicate.Body.Loop], row: mutable.Map[String, Value]): Unit = ps match {
      case Nil => filter(rule.filters, row)
      case Predicate.Body.Loop(name, term, _, _) :: rest =>
        val result = Interpreter.evalHeadTerm(term, sCtx.root, row.toMap)
      // TODO: Cast to Set and iterate.
      // TODO: Call loop from cross.
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
  def eval(t: TypedAst.Term.Body, env: mutable.Map[String, Value]): Value = t match {
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
  def dependencies(name: Name.Resolved, fact: Array[Value]): Unit = {

    def unify(pat: Array[String], fact: Array[Value]): mutable.Map[String, Value] = {
      val env = mutable.Map.empty[String, Value]
      var i = 0
      while (i < pat.length) {
        val varName = pat(i)
        if (varName != null)
          env.update(varName, fact(i))
        i = i + 1
      }
      env
    }

    for ((rule, p) <- sCtx.root.dependenciesOf(name)) {
      sCtx.root.collections(name) match {
        case r: TypedAst.Collection.Relation =>
          // unify all terms with their values.
          val env = unify(p.index2var, fact)
          if (env != null) {
            worklist += ((rule, env))
          }
        case l: TypedAst.Collection.Lattice =>
          // unify only key terms with their values.
          val numberOfKeys = l.keys.length
          val env = unify(p.index2var.take(numberOfKeys), fact.take(numberOfKeys))
          if (env != null) {
            worklist += ((rule, env))
          }
      }
    }
  }


  /**
    * Checks all assertions.
    */
  def checkAssertions(): Unit = {
    // asserted rules
    val assertedFacts = @@(sCtx.root.directives.assertedFacts map checkAssertedFact)
    assertedFacts.errors.foreach(e => println(e.format))

    // asserted facts
    val assertedRules = @@(sCtx.root.directives.assertedRules map checkAssertedRule)
    assertedRules.errors.foreach(e => println(e.format))
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
