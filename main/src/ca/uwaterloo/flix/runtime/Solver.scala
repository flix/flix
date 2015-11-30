package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast.{Name, TypedAst}
import ca.uwaterloo.flix.runtime.datastore.DataStore
import ca.uwaterloo.flix.runtime.debugger.RestServer
import ca.uwaterloo.flix.util.{Verbosity, Debugger, Options, AsciiTable}

import scala.annotation.tailrec
import scala.collection.mutable

object Solver {

  /**
   * A case class representing a solver context.
   */
  case class SolverContext(root: TypedAst.Root, options: Options)

}

/**
 * A solver based on semi-naive evaluation.
 */
class Solver(implicit val sCtx: Solver.SolverContext) {

  /**
   * The primary data store that holds all relations and lattices.
   */
  val dataStore = new DataStore()

  /**
   * The work list of pending predicate names and their associated values.
   */
  val worklist = new mutable.ArrayStack[(Constraint.Rule, mutable.Map[String, Value])]

  /**
   * The runtime performance monitor.
   */
  val monitor = new Monitor(this)

  /**
   * The current state of the solver.
   */
  @volatile
  var paused: Boolean = false

  /**
   * Returns the number of elements in the worklist.
   */
  def getQueueSize = worklist.length

  /**
   * Returns the number of facts in the database.
   */
  def getNumberOfFacts: Int = dataStore.numberOfFacts

  /**
   * Pauses the solver.
   */
  def pause(): Unit = synchronized {
    paused = true
  }

  /**
   * Resumes the fixpoint computation.
   */
  def resume(): Unit = synchronized {
    paused = false
    notify()
  }

  /**
   * Solves the current Flix program.
   */
  def solve(): Model = {

    if (sCtx.options.debugger == Debugger.Enabled) {
      monitor.start()

      val restServer = new RestServer(this)
      restServer.start()

      val shell = new Shell(this)
      shell.start()
    }

    // measure the time elapsed.
    val t = System.nanoTime()

    // evaluate all facts.
    for (fact <- sCtx.root.facts) {
      evalHead(fact.head, mutable.Map.empty, enqueue = false)
    }

    // add all rules to the worklist (under empty environments).
    for (rule <- sCtx.root.rules) {
      worklist.push((rule, mutable.Map.empty))
    }

    // iterate until fixpoint.
    while (worklist.nonEmpty) {
      if (paused) {
        synchronized {
          wait()
        }
      }
      // extract fact from the worklist.
      val (rule, env) = worklist.pop()
      evalBody(rule, env)
    }

    // computed elapsed time.
    val elapsed = System.nanoTime() - t

    if (sCtx.options.verbosity != Verbosity.Silent) {
      val solverTime = elapsed / 1000000
      val initialFacts = sCtx.root.facts.size
      val totalFacts = dataStore.numberOfFacts
      println(s"Solved in $solverTime msec. Initial Facts: $initialFacts. Total Facts: $totalFacts.")
    }

    if (sCtx.options.debugger == Debugger.Enabled) {
      monitor.stop()
    }

    // construct the model.
    val relations = dataStore.relations.foldLeft(Map.empty[Name.Resolved, Iterator[List[Value]]]) {
      case (macc, (name, relation)) =>
        val table = relation.scan.map(_.toList)
        macc + ((name, table))
    }
    val lattices = dataStore.lattices.foldLeft(Map.empty[Name.Resolved, Iterator[(List[Value], List[Value])]]) {
      case (macc, (name, lattice)) =>
        val table = lattice.scan.map {
          case (keys, values) => (keys.toArray.toList, values.toList)
        }
        macc + ((name, table))
    }
    Model(sCtx.root, relations, lattices)
  }

  // TODO: Move
  def getRuleStats: List[(TypedAst.Constraint.Rule, Int, Long)] =
    sCtx.root.rules.toSeq.sortBy(_.elapsedTime).reverse.map {
      case r => (r, r.hitcount, r.elapsedTime)
    }.toList

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
  def evalHead(p: Predicate.Head, env0: mutable.Map[String, Value], enqueue: Boolean): Unit = p match {
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
    val t = System.nanoTime()

    cross(rule, rule.collections, env0)

    rule.elapsedTime += System.nanoTime() - t
    rule.hitcount += 1
  }

  /**
   * Computes the cross product of all collections in the body.
   */
  def cross(rule: Constraint.Rule, ps: List[Predicate.Body.Collection], row: mutable.Map[String, Value]): Unit = ps match {
    case Nil =>
      // cross product complete, now filter
      loop(rule, rule.loops, row)
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
            newRow.update(varName, matchedRow(i))
          i = i + 1
        }

        // compute the cross product of the remaining
        // collections under the new environment.
        cross(rule, xs, newRow)
      }
  }

  /**
   * Unfolds the given loop predicates `ps` over the initial `row`.
   */
  def loop(rule: Constraint.Rule, ps: List[Predicate.Body.Loop], row: mutable.Map[String, Value]): Unit = ps match {
    case Nil => filter(rule, rule.filters, row)
    case Predicate.Body.Loop(name, term, _, _) :: rest =>
      val result = Interpreter.evalHeadTerm(term, sCtx.root, row).toSet
      for (x <- result) {
        val newRow = row.clone()
        newRow.update(name.name, x)
        loop(rule, rest, newRow)
      }
  }

  /**
   * Filters the given `row` through all filter functions in the body.
   */
  @tailrec
  private def filter(rule: Constraint.Rule, ps: List[Predicate.Body.Function], row: mutable.Map[String, Value]): Unit = ps match {
    case Nil =>
      // filter complete, now check disjointness
      disjoint(rule, rule.disjoint, row)
    case (pred: Predicate.Body.Function) :: xs =>
      val lambda = sCtx.root.constants(pred.name)
      val args = new Array[Value](pred.termsAsArray.length)
      var i = 0
      while (i < args.length) {
        args(i) = Interpreter.evalBodyTerm(pred.termsAsArray(i), row)
        i = i + 1
      }
      val result = Interpreter.evalCall(lambda.exp, args, sCtx.root, row).toBool
      if (result)
        filter(rule, xs, row)
  }

  /**
   * Filters the given `row` through all disjointness filters in the body.
   */
  @tailrec
  private def disjoint(rule: Constraint.Rule, ps: List[Predicate.Body.NotEqual], row: mutable.Map[String, Value]): Unit = ps match {
    case Nil =>
      // rule body complete, evaluate the head.
      evalHead(rule.head, row, enqueue = true)
    case Predicate.Body.NotEqual(ident1, ident2, _, _) :: xs =>
      val value1 = row(ident1.name)
      val value2 = row(ident2.name)
      if (value1 != value2) {
        disjoint(rule, xs, row)
      }
  }

  /**
   * Evaluates the given body term `t` to a value.
   *
   * Returns `null` if the term is a free variable.
   */
  def eval(t: TypedAst.Term.Body, env: mutable.Map[String, Value]): Value = t match {
    case t: TypedAst.Term.Body.Wildcard => null
    case t: TypedAst.Term.Body.Var => env.getOrElse(t.ident.name, null)
    case t: TypedAst.Term.Body.Lit => Interpreter.evalLit(t.lit)
  }

  /**
   * Returns all dependencies of the given `name` along with an environment.
   */
  def dependencies(name: Name.Resolved, fact: Array[Value]): Unit = {

    def unify(pat: Array[String], fact: Array[Value], limit: Int): mutable.Map[String, Value] = {
      val env = mutable.Map.empty[String, Value]
      var i = 0
      while (i < limit) {
        val varName = pat(i)
        if (varName != null)
          env.update(varName, fact(i))
        i = i + 1
      }
      env
    }

    val collection = sCtx.root.collections(name)
    for ((rule, p) <- sCtx.root.dependenciesOf(name)) {
      collection match {
        case r: TypedAst.Collection.Relation =>
          // unify all terms with their values.
          val env = unify(p.index2var, fact, fact.length)
          if (env != null) {
            worklist += ((rule, env))
          }
        case l: TypedAst.Collection.Lattice =>
          // unify only key terms with their values.
          val numberOfKeys = l.keys.length
          val env = unify(p.index2var, fact, numberOfKeys)
          if (env != null) {
            worklist += ((rule, env))
          }
      }
    }
  }

}
