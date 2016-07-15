/*
 * Copyright 2015-2016 Magnus Madsen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package ca.uwaterloo.flix.runtime

import java.util.ArrayList
import java.util.concurrent._

import ca.uwaterloo.flix.api.{IValue, RuleException, TimeoutException, WrappedValue}
import ca.uwaterloo.flix.language.ast.ExecutableAst.Constraint.Rule
import ca.uwaterloo.flix.language.ast.ExecutableAst._
import ca.uwaterloo.flix.language.ast.{Ast, ExecutableAst, Symbol}
import ca.uwaterloo.flix.runtime.datastore.DataStore
import ca.uwaterloo.flix.runtime.debugger.RestServer
import ca.uwaterloo.flix.util._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.{Duration, _}

/**
  * Flix Fixed Point Solver.
  *
  * Based on a variant of semi-naive evaluation.
  *
  * The solver computes the least fixed point of the rules in the given program.
  */
class Solver(val root: ExecutableAst.Root, options: Options) {

  //
  // Types of the solver:
  //

  /**
    * The type of environments.
    *
    * An environment is map from identifiers to values.
    */
  type Env = mutable.Map[String, AnyRef]

  /**
    * The type of work lists.
    *
    * A (possibly local) work list is a collection of (rule, env) pairs
    * specifying that the rule must be re-evaluated under the associated environment.
    */
  type WorkList = mutable.ArrayStack[(Rule, Env)]

  /**
    * The type of a (partial) interpretation.
    *
    * An interpretation is collection of facts (a table symbol associated with an array of facts).
    */
  type Interpretation = mutable.ArrayBuffer[(Symbol.TableSym, Array[AnyRef])]

  //
  // State of the solver:
  //

  /**
    * The datastore holds the facts in all relations and lattices in the program.
    *
    * Reading from the datastore is guaranteed to be thread-safe and can be performed by multiple threads concurrently.
    *
    * Writing to the datastore is, in general, not thread-safe: Each relation/lattice may be concurrently updated, but
    * no concurrent writes may occur for the *same* relation/lattice.
    */
  val dataStore = new DataStore[AnyRef](root)

  /**
    * The thread pool where rule evaluation takes place.
    *
    * Note: Evaluation of a rule only *reads* from the datastore.
    * Thus it is safe to evaluate multiple rules concurrently.
    */
  val readersPool = mkThreadPool()

  /**
    * The thread pool where writes to the datastore takes places.
    *
    * Note: A writer *must not* concurrently write to the same relation/lattice.
    * However, different relations/lattices can be written to concurrently.
    */
  val writersPool = mkThreadPool()

  /**
    * The global work list.
    */
  val worklist = mkWorkList()

  /**
    * The performance monitor.
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

  //
  // Statistics:
  //
  /**
    * Current read tasks.
    */
  @volatile
  var currentReadTasks: Int = 0

  /**
    * Current write tasks.
    */
  @volatile
  var currentWriteTasks: Int = 0

  /**
    * Total wall-clock time.
    */
  var totalTime: Long = 0

  /**
    * Time spent during initialization of the datastore.
    */
  var initTime: Long = 0

  /**
    * Time spent during the readers phase.
    */
  var readersTime: Long = 0

  /**
    * Time spent during the writers phase.
    */
  var writersTime: Long = 0

  /**
    * Returns the number of current read tasks.
    */
  def getCurrentReadTasks = currentReadTasks

  /**
    * Returns the number of current write tasks.
    */
  def getCurrentWriteTasks = currentWriteTasks

  /**
    * Returns the number of facts in the datastore.
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
    * Solves the Flix program.
    */
  def solve(): Model = try {
    // initialize the solver.
    initSolver()

    // initialize the datastore.
    initDataStore()

    // initialize the worklist.
    initWorkList()

    // iterate until fixpoint.
    while (worklist.nonEmpty) {
      // check if the solver has been paused.
      checkPaused()

      // check soft timeout.
      checkTimeout()

      // evaluate the rules in parallel.
      val interps = parallelEval()

      // update the datastore in parallel.
      parallelUpdate(interps)
    }

    // stop the solver.
    stopSolver()

    // print debugging information.
    printDebug()

    // build and return the model.
    mkModel(totalTime)
  } catch {
    // Re-throw exceptions caught inside the individual reader/writer tasks.
    case ex: ExecutionException =>
      stopSolver()
      throw ex.getCause
  }

  /**
    * Returns the model (if available).
    */
  def getModel: Model = model

  def getRuleStats: List[(Rule, Int, Long)] =
    root.rules.toSeq.sortBy(_.time.get()).reverse.map {
      case r => (r, r.hits.get(), r.time.get())
    }.toList

  /**
    * Initialize the solver by starting the monitor, debugger, shell etc.
    */
  private def initSolver(): Unit = {
    if (options.monitor) {
      monitor.start()

      val restServer = new RestServer(this)
      restServer.start()

      val shell = new Shell(this)
      shell.start()
    }

    totalTime = System.nanoTime()
  }

  /**
    * Initialize the datastore with all the facts in the program.
    */
  private def initDataStore(): Unit = {
    val t = System.nanoTime()
    // iterate through all facts.
    for (fact <- root.facts) {
      // evaluate the head of each fact.
      val interp = mkInterpretation()
      evalHead(fact.head, mutable.Map.empty, interp)

      // iterate through the interpretation.
      for ((sym, fact) <- interp) {
        // update the datastore, but don't compute any dependencies.
        root.tables(sym) match {
          case r: ExecutableAst.Table.Relation =>
            dataStore.relations(sym).inferredFact(fact)
          case l: ExecutableAst.Table.Lattice =>
            dataStore.lattices(sym).inferredFact(fact)
        }
      }
    }
    initTime = System.nanoTime() - t
  }

  /**
    * Initialize the worklist with every rule (under the empty environment).
    */
  private def initWorkList(): Unit = {
    // add all rules to the worklist (under empty environments).
    for (rule <- root.rules) {
      worklist.push((rule, mutable.Map.empty))
    }
  }

  /**
    * Stops the solver.
    */
  private def stopSolver(): Unit = {
    // empty worklist.
    worklist.clear()

    // spin down thread pools.
    readersPool.shutdownNow()
    writersPool.shutdownNow()

    // stop the debugger (if enabled).
    if (options.monitor) {
      monitor.stop()
    }

    totalTime = System.nanoTime() - totalTime
  }

  /**
    * Prints debugging information.
    */
  private def printDebug(): Unit = {
    if (options.verbosity != Verbosity.Silent) {
      val solverTime = totalTime / 1000000
      val initMiliSeconds = initTime / 1000000
      val readersMiliSeconds = readersTime / 1000000
      val writersMiliSeconds = writersTime / 1000000
      val initialFacts = root.facts.length
      val totalFacts = dataStore.numberOfFacts
      val throughput = (1000 * totalFacts) / (solverTime + 1)
      Console.println(f"Solved in $solverTime%,d msec. (init: $initMiliSeconds%,d msec, readers: $readersMiliSeconds%,d msec, writers: $writersMiliSeconds%,d msec)")
      Console.println(f"Initial Facts: $initialFacts%,d. Total Facts: $totalFacts%,d.")
      Console.println(f"Throughput: $throughput%,d facts per second.")
    }
  }

  /**
    * Returns a callable which evaluates the body of the given `rule` under the given initial environment `env`.
    *
    * Updates the given interpretation `interp`.
    */
  private def evalBody(rule: Rule, env: Env): Callable[Interpretation] = new Callable[Interpretation] {
    def call(): Interpretation = {
      val t = System.nanoTime()

      val interp = mkInterpretation()
      evalCross(rule, rule.tables, env, interp)

      rule.hits.incrementAndGet()
      rule.time.addAndGet(System.nanoTime() - t)

      interp
    }
  }

  /**
    * Computes the cross product of all collections in the body.
    */
  private def evalCross(rule: Rule, ps: List[Predicate.Body.Table], env: Env, interp: Interpretation): Unit = ps match {
    case Nil =>
      // cross product complete, now filter
      evalLoop(rule, rule.loops, env, interp)
    case (p: Predicate.Body.Table) :: xs =>
      // lookup the relation or lattice.
      val table = root.tables(p.sym) match {
        case r: Table.Relation => dataStore.relations(p.sym)
        case l: Table.Lattice => dataStore.lattices(p.sym)
      }

      // evaluate all terms in the predicate.
      val pat = new Array[AnyRef](p.arity)
      var i = 0
      while (i < pat.length) {
        pat(i) = evalTerm(p.terms(i), env)
        i = i + 1
      }

      // lookup all matching rows.
      for (matchedRow <- table.lookup(pat)) {
        // copy the environment for every row.
        val newRow = env.clone()

        var i = 0
        while (i < matchedRow.length) {
          val varName = p.index2var(i)
          if (varName != null)
            newRow.update(varName, matchedRow(i))
          i = i + 1
        }

        // compute the cross product of the remaining
        // collections under the new environment.
        evalCross(rule, xs, newRow, interp)
      }
  }

  /**
    * Unfolds the given loop predicates `ps` over the initial `env`.
    */
  private def evalLoop(rule: Rule, ps: List[Predicate.Body.Loop], env: Env, interp: Interpretation): Unit = ps match {
    case Nil => evalFilter(rule, rule.filters, env, interp)
    case Predicate.Body.Loop(name, term, _, _, _) :: rest =>
      val value = Value.cast2set(Interpreter.evalHeadTerm(term, root, env.toMap))
      for (x <- value) {
        val newRow = env.clone()
        newRow.update(name.name, Value.cast2flix(x))
        evalLoop(rule, rest, newRow, interp)
      }
  }

  /**
    * Filters the given `env` through all filter functions in the body.
    */
  @tailrec
  private def evalFilter(rule: Rule, ps: List[Predicate.Body.ApplyFilter], env: Env, interp: Interpretation): Unit = ps match {
    case Nil =>
      // filter with hook functions
      evalFilterHook(rule, rule.filterHooks, env, interp)
    case (pred: Predicate.Body.ApplyFilter) :: xs =>
      val defn = root.constants(pred.name)
      val args = new Array[AnyRef](pred.terms.length)
      var i = 0
      while (i < args.length) {
        args(i) = Interpreter.evalBodyTerm(pred.terms(i), root, env.toMap)
        i = i + 1
      }
      val result = Interpreter.evalCall(defn, args, root, env.toMap)
      if (Value.cast2bool(result))
        evalFilter(rule, xs, env, interp)
  }

  /**
    * Filters the given `env` through all filter hook functions in the body.
    */
  @tailrec
  private def evalFilterHook(rule: Rule, ps: List[Predicate.Body.ApplyHookFilter], env: Env, interp: Interpretation): Unit = ps match {
    case Nil =>
      // filter complete, now check disjointness
      evalDisjoint(rule, rule.disjoint, env, interp)
    case (pred: Predicate.Body.ApplyHookFilter) :: xs =>

      val args = new Array[AnyRef](pred.terms.length)
      var i = 0
      while (i < args.length) {
        args(i) = Interpreter.evalBodyTerm(pred.terms(i), root, env.toMap)
        i = i + 1
      }

      pred.hook match {
        case Ast.Hook.Safe(name, inv, tpe) =>
          val wargs = args map {
            case arg => new WrappedValue(arg): IValue
          }
          val result = inv.apply(wargs).getUnsafeRef
          if (Value.cast2bool(result)) {
            evalFilterHook(rule, xs, env, interp)
          }
        case Ast.Hook.Unsafe(name, inv, tpe) =>
          val result = inv.apply(args)
          if (Value.cast2bool(result)) {
            evalFilterHook(rule, xs, env, interp)
          }
      }
  }

  /**
    * Filters the given `env` through all disjointness filters in the body.
    */
  @tailrec
  private def evalDisjoint(rule: Rule, ps: List[Predicate.Body.NotEqual], env: Env, interp: Interpretation): Unit = ps match {
    case Nil =>
      // rule body complete, evaluate the head.
      evalHead(rule.head, env, interp)
    case Predicate.Body.NotEqual(ident1, ident2, _, _, _) :: xs =>
      val value1 = env(ident1.name)
      val value2 = env(ident2.name)
      if (value1 != value2) {
        evalDisjoint(rule, xs, env, interp)
      }
  }

  /**
    * Evaluates the given head predicate `p` under the given environment `env0`.
    */
  private def evalHead(p: Predicate.Head, env: Env, interp: Interpretation): Unit = p match {
    case p: Predicate.Head.Table =>
      val terms = p.terms
      val fact = new Array[AnyRef](p.arity)
      var i = 0
      while (i < fact.length) {
        fact(i) = Interpreter.evalHeadTerm(terms(i), root, env.toMap)
        i = i + 1
      }

      interp += ((p.sym, fact))
    case Predicate.Head.True(loc) => // nop
    case Predicate.Head.False(loc) => throw RuleException(s"The integrity rule defined at ${loc.format} is violated.", loc)
  }

  /**
    * Returns a callable to process a collection of inferred `facts` for the relation or lattice with the symbol `sym`.
    */
  private def inferredFacts(sym: Symbol.TableSym, facts: ArrayBuffer[Array[AnyRef]]): Callable[WorkList] = new Callable[WorkList] {
    def call(): WorkList = {
      val localWorkList = mkWorkList()
      for (fact <- facts) {
        inferredFact(sym, fact, localWorkList)
      }
      localWorkList
    }
  }

  /**
    * Processes an inferred `fact` for the relation or lattice with the symbol `sym`.
    */
  private def inferredFact(sym: Symbol.TableSym, fact: Array[AnyRef], localWorkList: WorkList): Unit = root.tables(sym) match {
    case r: ExecutableAst.Table.Relation =>
      val changed = dataStore.relations(sym).inferredFact(fact)
      if (changed) {
        dependencies(r.sym, fact, localWorkList)
      }

    case l: ExecutableAst.Table.Lattice =>
      val changed = dataStore.lattices(sym).inferredFact(fact)
      if (changed) {
        dependencies(l.sym, fact, localWorkList)
      }
  }

  /**
    * Evaluates the given body term `t` to a value.
    *
    * Returns `null` if the term is a free variable.
    */
  private def evalTerm(t: ExecutableAst.Term.Body, env: Env): AnyRef = t match {
    case t: ExecutableAst.Term.Body.Wildcard => null
    case t: ExecutableAst.Term.Body.Var => env.getOrElse(t.ident.name, null)
    case t: ExecutableAst.Term.Body.Exp => Interpreter.eval(t.e, root, env.toMap)
  }

  /**
    * Returns all dependencies of the given symbol `sym` along with an environment.
    */
  private def dependencies(sym: Symbol.TableSym, fact: Array[AnyRef], localWorkList: WorkList): Unit = {

    def unify(pat: Array[String], fact: Array[AnyRef], limit: Int): Env = {
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

    val table = root.tables(sym)
    for ((rule, p) <- root.dependenciesOf(sym)) {
      table match {
        case r: ExecutableAst.Table.Relation =>
          // unify all terms with their values.
          val env = unify(p.index2var, fact, fact.length)
          if (env != null) {
            localWorkList.push((rule, env))
          }
        case l: ExecutableAst.Table.Lattice =>
          // unify only key terms with their values.
          val numberOfKeys = l.keys.length
          val env = unify(p.index2var, fact, numberOfKeys)
          if (env != null) {
            localWorkList.push((rule, env))
          }
      }
    }
  }

  /**
    * Evaluates each of the rules in parallel.
    */
  private def parallelEval(): Iterator[Interpretation] = {
    val t = System.nanoTime()

    // --- begin parallel execution ---
    val readerTasks = new ArrayList[Callable[Interpretation]]()
    for ((rule, env) <- worklist) {
      val task = evalBody(rule, env)
      readerTasks.add(task)
      currentReadTasks += 1
    }
    val result = flatten(readersPool.invokeAll(readerTasks))
    // -- end parallel execution ---

    currentReadTasks = 0
    readersTime += System.nanoTime() - t

    worklist.clear()
    result
  }

  /**
    * Updates the datastore in parallel.
    */
  private def parallelUpdate(iter: Iterator[Interpretation]): Unit = {
    val t = System.nanoTime()

    // --- begin parallel execution ---
    val tasks = new ArrayList[Callable[WorkList]]()
    for ((sym, facts) <- groupFactsBySymbol(iter)) {
      val task = inferredFacts(sym, facts)
      tasks.add(task)
      currentWriteTasks += facts.length
    }
    val localWorkLists = writersPool.invokeAll(tasks)
    // --- end parallel execution ---
    currentWriteTasks = 0

    // update the global work list with each of the local work lists.
    for (localWorkList <- flatten(localWorkLists)) {
      worklist ++= localWorkList
    }

    writersTime += System.nanoTime() - t
  }


  /**
    * Sorts the given facts by their table symbol.
    */
  private def groupFactsBySymbol(iter: Iterator[Interpretation]): mutable.Map[Symbol.TableSym, ArrayBuffer[Array[AnyRef]]] = {
    val result = mutable.Map.empty[Symbol.TableSym, ArrayBuffer[Array[AnyRef]]]
    while (iter.hasNext) {
      val interp = iter.next()
      for ((symbol, fact) <- interp) {
        val buffer = result.getOrElseUpdate(symbol, ArrayBuffer.empty)
        buffer += fact
      }
    }
    result
  }

  /**
    * Constructs the minimal model from the datastore.
    */
  private def mkModel(elapsed: Long): Model = {
    // construct the model.
    val definitions = root.constants.foldLeft(Map.empty[Symbol.Resolved, () => AnyRef]) {
      case (macc, (sym, defn)) =>
        if (defn.formals.isEmpty)
          macc + (sym -> (() => Interpreter.evalCall(defn, Array.empty, root)))
        else
          macc + (sym -> (() => throw new InternalRuntimeException("Unable to evalaute non-constant top-level definition.")))
    }

    val relations = dataStore.relations.foldLeft(Map.empty[Symbol.TableSym, Iterable[List[AnyRef]]]) {
      case (macc, (sym, relation)) =>
        val table = relation.scan.toIterable.map(_.toList)
        macc + ((sym, table))
    }
    val lattices = dataStore.lattices.foldLeft(Map.empty[Symbol.TableSym, Iterable[(List[AnyRef], AnyRef)]]) {
      case (macc, (sym, lattice)) =>
        val table = lattice.scan.toIterable.map {
          case (keys, values) => (keys.toArray.toList, values)
        }
        macc + ((sym, table))
    }
    model = new Model(root, root.time.copy(solver = elapsed), definitions, relations, lattices)
    model
  }

  /**
    * Returns a new thread pool configured to use the appropriate number of threads.
    */
  private def mkThreadPool(): ExecutorService = options.threads match {
    // Case 1: Parallel execution disabled. Use a single thread.
    case 1 => Executors.newSingleThreadExecutor()
    // Case 2: Parallel execution enabled. Use the specified number of processors.
    case n => Executors.newFixedThreadPool(n)
  }

  /**
    * Returns an iterator over the result of the given list of Java futures.
    */
  private def flatten[A](fs: java.util.List[Future[A]]): Iterator[A] = {
    val iter = fs.iterator()
    new Iterator[A] {
      def hasNext: Boolean = iter.hasNext

      def next(): A = iter.next().get()
    }
  }

  /**
    * Returns a fresh work list.
    */
  private def mkWorkList(): WorkList = new mutable.ArrayStack[(Rule, Env)]

  /**
    * Returns a fresh (empty) interpretation.
    */
  private def mkInterpretation(): Interpretation = new mutable.ArrayBuffer[(Symbol.TableSym, Array[AnyRef])]()

  /**
    * Checks if the solver is paused, and if so, waits for an interrupt.
    */
  private def checkPaused(): Unit =
    if (paused) {
      synchronized {
        wait()
      }
    }

  /**
    * Checks whether the solver has exceed the timeout. If so, throws a timeout exception.
    */
  private def checkTimeout(): Unit = {
    if (options.timeout.isFinite()) {
      val elapsed = System.nanoTime() - totalTime
      if (elapsed > options.timeout.toNanos) {
        stopSolver()
        throw TimeoutException(options.timeout, Duration(elapsed, NANOSECONDS))
      }
    }
  }

}
