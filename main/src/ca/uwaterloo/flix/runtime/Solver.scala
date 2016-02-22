package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.api.{IValue, WrappedValue}
import ca.uwaterloo.flix.language.ast.ExecutableAst._
import ca.uwaterloo.flix.language.ast.{Ast, ExecutableAst, Name}
import ca.uwaterloo.flix.runtime.datastore.DataStore
import ca.uwaterloo.flix.runtime.debugger.RestServer
import ca.uwaterloo.flix.util.{Debugger, Options, Verbosity}

import scala.annotation.tailrec
import scala.collection.mutable

object Solver {

  /**
    * A case class representing a solver context.
    */
  case class SolverContext(root: ExecutableAst.Root, options: Options)

}

/**
  * A solver based on semi-naive evaluation.
  */
class Solver(implicit val sCtx: Solver.SolverContext) {

  /**
    * The primary data store that holds all relations and lattices.
    */
  val dataStore = new DataStore[AnyRef]()

  /**
    * The work list of pending predicate names and their associated values.
    */
  val worklist = new mutable.ArrayStack[(Constraint.Rule, mutable.Map[String, AnyRef])]

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
    * The model (if it exists).
    */
  @volatile
  var model: Model = null

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

    val constants = sCtx.root.constants.foldLeft(Map.empty[Name.Resolved, AnyRef]) {
      case (macc, (name, constant)) => constant.exp match {
        case e: ExecutableAst.Expression.Lambda if e.args.length == 0 =>
          val v = Interpreter.eval(e.body, sCtx.root)
          macc + (name -> v)
        case _ => macc
      }
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
      val initialFacts = sCtx.root.facts.length
      val totalFacts = dataStore.numberOfFacts
      val throughput = (1000 * totalFacts) / (solverTime + 1)
      Console.println(f"Successfully solved in $solverTime%,d msec.")
      Console.println(f"Initial Facts: $initialFacts%,d. Total Facts: $totalFacts%,d.")
      Console.println(f"Throughput: $throughput%,d facts per second.")
    }

    if (sCtx.options.debugger == Debugger.Enabled) {
      monitor.stop()
    }

    // construct the model.
    val relations = dataStore.relations.foldLeft(Map.empty[Name.Resolved, Iterable[List[AnyRef]]]) {
      case (macc, (name, relation)) =>
        val table = relation.scan.toIterable.map(_.toList)
        macc + ((name, table))
    }
    val lattices = dataStore.lattices.foldLeft(Map.empty[Name.Resolved, Iterable[(List[AnyRef], List[AnyRef])]]) {
      case (macc, (name, lattice)) =>
        val table = lattice.scan.toIterable.map {
          case (keys, values) => (keys.toArray.toList, values.toList)
        }
        macc + ((name, table))
    }
    model = Model(sCtx.root, constants, relations, lattices)
    model
  }

  def getModel: Model = model

  // TODO: Move
  def getRuleStats: List[(ExecutableAst.Constraint.Rule, Int, Long)] =
    sCtx.root.rules.toSeq.sortBy(_.elapsedTime).reverse.map {
      case r => (r, r.hitcount, r.elapsedTime)
    }.toList

  /**
    * Processes an inferred `fact` for the relation or lattice with the `name`.
    */
  def inferredFact(name: Name.Resolved, fact: Array[AnyRef], enqueue: Boolean): Unit = sCtx.root.collections(name) match {
    case r: ExecutableAst.Collection.Relation =>
      val changed = dataStore.relations(name).inferredFact(fact)
      if (changed && enqueue) {
        dependencies(r.name, fact)
      }

    case l: ExecutableAst.Collection.Lattice =>
      val changed = dataStore.lattices(name).inferredFact(fact)
      if (changed && enqueue) {
        dependencies(l.name, fact)
      }
  }

  /**
    * Evaluates the given head predicate `p` under the given environment `env0`.
    */
  def evalHead(p: Predicate.Head, env0: mutable.Map[String, AnyRef], enqueue: Boolean): Unit = p match {
    case p: Predicate.Head.Relation =>
      val terms = p.terms
      val fact = new Array[AnyRef](p.arity)
      var i = 0
      while (i < fact.length) {
        fact(i) = Interpreter.evalHeadTerm(terms(i), sCtx.root, env0)
        i = i + 1
      }
      inferredFact(p.name, fact, enqueue)
  }


  /**
    * Evaluates the body of the given `rule` under the given initial environment `env0`.
    */
  def evalBody(rule: Constraint.Rule, env0: mutable.Map[String, AnyRef]): Unit = {
    val t = System.nanoTime()

    cross(rule, rule.collections, env0)

    rule.elapsedTime += System.nanoTime() - t
    rule.hitcount += 1
  }

  /**
    * Computes the cross product of all collections in the body.
    */
  def cross(rule: Constraint.Rule, ps: List[Predicate.Body.Collection], row: mutable.Map[String, AnyRef]): Unit = ps match {
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
      val pat = new Array[AnyRef](p.arity)
      var i = 0
      while (i < pat.length) {
        pat(i) = eval(p.terms(i), row)
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
  def loop(rule: Constraint.Rule, ps: List[Predicate.Body.Loop], row: mutable.Map[String, AnyRef]): Unit = ps match {
    case Nil => filter(rule, rule.filters, row)
    case Predicate.Body.Loop(name, term, _, _, _) :: rest =>
      val result = Value.cast2set(Interpreter.evalHeadTerm(term, sCtx.root, row))
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
  private def filter(rule: Constraint.Rule, ps: List[Predicate.Body.ApplyFilter], row: mutable.Map[String, AnyRef]): Unit = ps match {
    case Nil =>
      // filter with hook functions
      filterHook(rule, rule.filterHooks, row)
    case (pred: Predicate.Body.ApplyFilter) :: xs =>
      val lambda = sCtx.root.constants(pred.name)
      val args = new Array[AnyRef](pred.terms.length)
      var i = 0
      while (i < args.length) {
        args(i) = Interpreter.evalBodyTerm(pred.terms(i), sCtx.root, row)
        i = i + 1
      }
      val result = Interpreter.evalCall(lambda.exp, args, sCtx.root, row)
      if (Value.cast2bool(result))
        filter(rule, xs, row)
  }

  /**
    * Filters the given `row` through all filter hook functions in the body.
    */
  @tailrec
  private def filterHook(rule: Constraint.Rule, ps: List[Predicate.Body.ApplyHookFilter], row: mutable.Map[String, AnyRef]): Unit = ps match {
    case Nil =>
      // filter complete, now check disjointness
      disjoint(rule, rule.disjoint, row)
    case (pred: Predicate.Body.ApplyHookFilter) :: xs =>

      val args = new Array[AnyRef](pred.terms.length)
      var i = 0
      while (i < args.length) {
        args(i) = Interpreter.evalBodyTerm(pred.terms(i), sCtx.root, row)
        i = i + 1
      }

      pred.hook match {
        case Ast.Hook.Safe(name, inv, tpe) =>
          val wargs = args map {
            case arg => new WrappedValue(arg): IValue
          }
          val result = inv.apply(wargs).getUnsafeRef
          if (Value.cast2bool(result)) {
            filterHook(rule, xs, row)
          }
        case Ast.Hook.Unsafe(name, inv, tpe) =>
          val result = inv.apply(args)
          if (Value.cast2bool(result)) {
            filterHook(rule, xs, row)
          }
      }
  }

  /**
    * Filters the given `row` through all disjointness filters in the body.
    */
  @tailrec
  private def disjoint(rule: Constraint.Rule, ps: List[Predicate.Body.NotEqual], row: mutable.Map[String, AnyRef]): Unit = ps match {
    case Nil =>
      // rule body complete, evaluate the head.
      evalHead(rule.head, row, enqueue = true)
    case Predicate.Body.NotEqual(ident1, ident2, _, _, _) :: xs =>
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
  def eval(t: ExecutableAst.Term.Body, env: mutable.Map[String, AnyRef]): AnyRef = t match {
    case t: ExecutableAst.Term.Body.Wildcard => null
    case t: ExecutableAst.Term.Body.Var => env.getOrElse(t.ident.name, null)
    case t: ExecutableAst.Term.Body.Exp => Interpreter.eval(t.e, sCtx.root, env)
  }

  /**
    * Returns all dependencies of the given `name` along with an environment.
    */
  def dependencies(name: Name.Resolved, fact: Array[AnyRef]): Unit = {

    def unify(pat: Array[String], fact: Array[AnyRef], limit: Int): mutable.Map[String, AnyRef] = {
      val env = mutable.Map.empty[String, AnyRef]
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
        case r: ExecutableAst.Collection.Relation =>
          // unify all terms with their values.
          val env = unify(p.index2var, fact, fact.length)
          if (env != null) {
            worklist += ((rule, env))
          }
        case l: ExecutableAst.Collection.Lattice =>
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
