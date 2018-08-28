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

package ca.uwaterloo.flix.runtime.solver

import java.time.Duration
import java.util.concurrent._
import java.util.concurrent.atomic.AtomicInteger

import ca.uwaterloo.flix.runtime.solver.datastore.DataStore
import ca.uwaterloo.flix.runtime.debugger.RestServer
import ca.uwaterloo.flix.runtime.solver.api._
import ca.uwaterloo.flix.runtime.Monitor
import ca.uwaterloo.flix.runtime.solver.api.predicate._
import ca.uwaterloo.flix.runtime.solver.api.symbol.VarSym
import ca.uwaterloo.flix.runtime.solver.api.term._
import ca.uwaterloo.flix.util._
import flix.runtime.{ReifiedSourceLocation, RuleError, TimeoutError}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Flix Fixed Point Solver.
  *
  * Based on a variant of semi-naive evaluation.
  *
  * The solver computes the least fixed point of the rules in the given program.
  */
class Solver(val constraintSet: ConstraintSet, options: FixpointOptions) {

  /**
    * Controls the number of batches per thread. A value of one means one batch per thread.
    *
    * If there is one batch per thread that means when one thread is finished it will have nothing else to do.
    *
    * A low number means lower overhead in terms of futures created, but possible more wasted time.
    * A higher number means less time wasted, but larger overhead in the number of futures created.
    */
  val BatchesPerThread: Int = 8

  //
  // Types of the solver:
  //

  /**
    * The type of environments.
    *
    * An environment is map from variables to values.
    */
  type Env = Array[ProxyObject]

  /**
    * The type of work lists.
    *
    * A (possibly local) work list is a collection of (rule, env) pairs
    * specifying that the rule must be re-evaluated under the associated environment.
    */
  type WorkList = mutable.ArrayStack[(Constraint, Env)]

  /**
    * The type of a (partial) interpretation.
    *
    * An interpretation is collection of facts (a table symbol associated with an array of facts).
    */
  type Interpretation = mutable.ArrayBuffer[(Table, Array[ProxyObject])]

  /**
    * The type of the dependency graph, a map from symbols to (constraint, atom) pairs.
    */
  type DependencyGraph = mutable.Map[Table, Set[(Constraint, AtomPredicate)]]

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
  val dataStore = new DataStore[AnyRef](constraintSet)

  /**
    * The dependencies of the program. Populated by [[initDependencies]].
    */
  val dependenciesOf: DependencyGraph = mutable.Map.empty

  /**
    * The thread pool where rule evaluation takes place.
    *
    * Note: Evaluation of a rule only *reads* from the datastore.
    * Thus it is safe to evaluate multiple rules concurrently.
    */
  val readersPool: ExecutorService = mkThreadPool("readers")

  /**
    * The thread pool where writes to the datastore takes places.
    *
    * Note: A writer *must not* concurrently write to the same relation/lattice.
    * However, different relations/lattices can be written to concurrently.
    */
  val writersPool: ExecutorService = mkThreadPool("writers")

  /**
    * The global work list.
    */
  val worklist: WorkList = mkWorkList()

  /**
    * The performance monitor.
    */
  val monitor = new Monitor(this)

  /**
    * The current state of the solver.
    */
  @volatile
  var paused: Boolean = false

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
  def getCurrentReadTasks: Int = currentReadTasks

  /**
    * Returns the number of current write tasks.
    */
  def getCurrentWriteTasks: Int = currentWriteTasks

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
  def solve(): Fixpoint = try {
    // initialize the solver.
    initSolver()

    // initialize the dependencies.
    initDependencies()

    // initialize the datastore.
    initDataStore()

    // compute the fixedpoint for each stratum, in sequence.
    for (stratum <- constraintSet.getConstraintsByStrata) {
      // initialize the worklist.
      initWorkList(stratum)

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
    }

    // stop the solver.
    stopSolver()

    // print debugging information.
    printDebug()

    // build and return the model.
    mkFixpoint(totalTime)
  } catch {
    // Re-throw exceptions caught inside the individual reader/writer tasks.
    case ex: ExecutionException =>
      stopSolver()
      throw ex.getCause
  }

  /**
    * Initialize the solver by starting the monitor, debugger, etc.
    */
  private def initSolver(): Unit = {
    if (options.isMonitored) {
      monitor.start()

      val restServer = new RestServer(this)
      restServer.start()
    }

    totalTime = System.nanoTime()
  }

  /**
    * Initialize the datastore with all the facts in the program.
    *
    * A fact is always in the lowest stratum.
    */
  private def initDataStore(): Unit = {
    val t = System.nanoTime()
    // retrieve the lowest stratum.
    val stratum0 = constraintSet.getConstraintsByStrata(0)

    // iterate through all facts.
    for (constraint <- stratum0) {
      if (constraint.isFact) {
        // evaluate the head of each fact.
        val interp = mkInterpretation()
        evalHead(constraint.getHeadPredicate(), Array.empty, interp)

        // iterate through the interpretation.
        for ((sym, fact) <- interp) {
          // update the datastore, but don't compute any dependencies.
          sym match {
            case r: Relation =>
              r.getIndexedRelation().inferredFact(fact)
            case l: Lattice =>
              l.getIndexedLattice().inferredFact(fact)
          }
        }
      }
    }
    initTime = System.nanoTime() - t
  }

  /**
    * Initialize the worklist with every rule (under the empty environment) in the given `stratum`.
    */
  private def initWorkList(stratum: Array[Constraint]): Unit = {
    // add all rules to the worklist (under empty environments).
    for (rule <- stratum) {
      worklist.push((rule, new Array[ProxyObject](rule.getNumberOfParameters)))
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
    if (options.isMonitored) {
      monitor.stop()
    }

    totalTime = System.nanoTime() - totalTime
  }

  /**
    * Prints debugging information.
    */
  private def printDebug(): Unit = {
    if (options.isVerbose) {
      val solverTime = totalTime / 1000000
      val initMiliSeconds = initTime / 1000000
      val readersMiliSeconds = readersTime / 1000000
      val writersMiliSeconds = writersTime / 1000000
      val initialFacts = constraintSet.getConstraintsByStrata.head.count(_.isFact)
      val totalFacts = dataStore.numberOfFacts
      val throughput = ((1000.0 * totalFacts.toDouble) / (solverTime.toDouble + 1.0)).toInt
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
  private def evalBody(rule: Constraint, env: Env): Interpretation = {
    val t = System.nanoTime()

    val interp = mkInterpretation()
    evalCross(rule, rule.getBodyAtoms().toList, env, interp)
    val e = System.nanoTime() - t

    rule.incrementNumberOfHits()
    rule.incrementElapsedTime(e)

    interp
  }

  /**
    * Computes the cross product of all collections in the body.
    */
  private def evalCross(rule: Constraint, ps: List[AtomPredicate], env: Env, interp: Interpretation): Unit = ps match {
    case Nil =>
      // complete, now functionals.
      evalAllFunctionals(rule, env, interp)
    case p :: xs =>
      // Compute the rows that match the atom.
      val rows = evalAtom(p, env)

      // Case split on whether the atom is positive or negative.
      if (p.isPositive) {
        // Case 1: The atom is positive. Recurse on all matched rows.
        for (newRow <- rows) {
          evalCross(rule, xs, newRow, interp)
        }
      } else {
        // Case 2: The atom is negative.
        // Assertion: Check that every variable has been assigned a value.
        for (t <- p.getTerms) {
          t match {
            case p: VarTerm =>
              assert(env(p.getSym.getStackOffset) != null, s"Unbound variable in negated atom.")
            case _ => // Nop
          }
        }

        // Case 2: The atom is negative. Recurse on the original environment if *no* rows were matched.
        if (rows.isEmpty) {
          evalCross(rule, xs, env, interp)
        }
      }

    case p => throw InternalRuntimeException(s"Unmatched predicate: '$p'.")
  }

  /**
    * Returns a sequence of rows matched by the given atom `p`.
    */
  private def evalAtom(p: AtomPredicate, env: Env): Traversable[Env] = {
    // lookup the relation or lattice.
    val table = p.getSym() match {
      case r: Relation => r.getIndexedRelation()
      case l: Lattice => l.getIndexedLattice()
    }

    // evaluate all terms in the predicate.
    val pat = new Array[ProxyObject](p.getTerms.length)
    var i = 0
    while (i < pat.length) {
      val value: ProxyObject = p.getTerms()(i) match {
        case p: VarTerm =>
          // A variable is replaced by its value from the environment (or null if unbound).
          env(p.getSym().getStackOffset)
        case p: LitTerm =>
          p.getFunction()()
        case p: WildTerm =>
          // A wildcard places no restrictions on the value.
          null
      }
      pat(i) = value
      i = i + 1
    }

    // a mutable collection to hold the matched results.
    val result = mutable.ArrayBuffer.empty[Env]

    // lookup all matching rows.
    for (matchedRow <- table.lookup(pat)) {
      // copy the environment for every row.
      val newRow = copy(env)

      // A matched row may still fail to unify with a pattern term.
      // We use this boolean variable to track whether that is the case.
      var skip = false

      var i = 0
      while (i < matchedRow.length) {
        val sym = p.getIndex2SymTEMPORARY(i)
        if (sym != null)
          newRow.update(sym.getStackOffset, matchedRow(i))
        i = i + 1
      }

      // Check whether the row was successfully matched.
      if (!skip) {
        result += newRow
      }
    }

    result
  }

  /**
    * Computes the cross product of all functionals in the body.
    */
  private def evalAllFunctionals(rule: Constraint, env: Env, interp: Interpretation): Unit =
    evalFunctionals(rule, rule.getFunctionals().toList, env, interp)

  /**
    * Evaluates a single functional.
    */
  private def evalFunctionals(rule: Constraint, ps: List[FunctionalPredicate], env: Env, interp: Interpretation): Unit = ps match {
    case Nil =>
      // complete, now filter.
      evalAllFilters(rule, env, interp)
    case r :: rs =>
      // compute the values of the arguments
      val args = new Array[AnyRef](r.getArguments().length)
      var i = 0
      for (a <- r.getArguments()) {
        args(i) = env(a.getStackOffset)
        i = i + 1
      }

      // apply the function to obtain the array of values.
      val values: Array[ProxyObject] = r.getFunction()(args)

      // iterate through each value.
      for (value <- values) {
        val newEnv = copy(env)
        newEnv(r.getVarSym().getStackOffset) = value
        evalFunctionals(rule, rs, newEnv, interp)
      }
  }

  /**
    * Evaluates all filters in the given `rule`.
    */
  private def evalAllFilters(rule: Constraint, env: Env, interp: Interpretation): Unit = {
    // Extract the filters function predicates from the rule.
    val filters = rule.getFilters()

    // Evaluate each filter function predicate one-by-one.
    var i = 0
    while (i < filters.length) {
      // Evaluate the current filter.
      val result = evalFilter(filters(i), env)

      // Return immediately if the filter function returned false.
      if (!result)
        return

      // Otherwise evaluate the next filter function.
      i = i + 1
    }

    // All filter functions returned `true`.
    // Continue evaluation of the head of the rule.
    evalHead(rule.getHeadPredicate(), env, interp)
  }

  /**
    * Evaluates the given `filter` and returns its result.
    */
  private def evalFilter(filter: FilterPredicate, env: Env): Boolean = filter match {
    case p: FilterPredicate =>
      // Evaluate the arguments of the filter function predicate.
      val args = new Array[AnyRef](p.getArguments().length)
      var j = 0
      // Iterate through each term of the filter function predicate.
      while (j < args.length) {
        // Compute the value of the term.
        val value: ProxyObject = p.getArguments()(j) match {
          case p: VarTerm =>
            // A variable is replaced by its value from the environment.
            env(p.getSym.getStackOffset)
          case p: LitTerm =>
            p.getFunction()()
          case p: WildTerm =>
            // A wildcard should not appear as an argument to a filter function.
            throw InternalRuntimeException("Wildcard not allowed here!")
        }

        // Store the value of the term into the argument array.
        args(j) = value.getValue
        j = j + 1
      }

      // Evaluate the filter function passing the arguments.
      val result = p.getFunction()(args)

      // Return the result.
      result.asInstanceOf[java.lang.Boolean].booleanValue()
  }

  /**
    * Evaluates the given head predicate `p` under the given environment `env0`.
    */
  private def evalHead(p: Predicate, env: Env, interp: Interpretation): Unit = p match {
    case p: AtomPredicate =>
      val terms = p.getTerms
      val fact = new Array[ProxyObject](p.getTerms.length)
      var i = 0
      while (i < fact.length) {
        val term: ProxyObject = evalHeadTerm(terms(i), constraintSet, env)
        fact(i) = term
        i = i + 1
      }

      interp += ((p.getSym, fact))
    case _: TruePredicate => // nop
    case _: FalsePredicate => throw new RuleError(new ReifiedSourceLocation("", 0, 0, 0, 0))
  }

  /**
    * Evaluates the given head term `t` under the given environment `env0`
    */
  def evalHeadTerm(t: Term, root: ConstraintSet, env: Env): ProxyObject = t match {
    case t: VarTerm => env(t.getSym.getStackOffset)
    case t: LitTerm => t.getFunction()()
    case t: AppTerm =>
      val args = new Array[AnyRef](t.getArguments().length)
      var i = 0
      while (i < args.length) {
        args(i) = env(t.getArguments()(i).getStackOffset).getValue
        i = i + 1
      }
      t.getFunction()(args)
  }

  /**
    * Returns a callable to process a collection of inferred `facts` for the relation or lattice with the symbol `sym`.
    */
  private def inferredFacts(sym: Table, facts: ArrayBuffer[Array[ProxyObject]]): Callable[WorkList] = () => {
    val localWorkList = mkWorkList()
    for (fact <- facts) {
      inferredFact(sym, fact, localWorkList)
    }
    localWorkList
  }

  /**
    * Processes an inferred `fact` for the relation or lattice with the symbol `sym`.
    */
  private def inferredFact(sym: Table, fact: Array[ProxyObject], localWorkList: WorkList): Unit = sym match {
    case r: Relation =>
      val changed = r.getIndexedRelation().inferredFact(fact)
      if (changed) {
        dependencies(sym, fact, localWorkList)
      }

    case l: Lattice =>
      val changed = l.getIndexedLattice().inferredFact(fact)
      if (changed) {
        dependencies(sym, fact, localWorkList)
      }
  }

  /**
    * Returns all dependencies of the given symbol `sym` along with an environment.
    */
  private def dependencies(sym: Table, fact: Array[ProxyObject], localWorkList: WorkList): Unit = {

    def unify(pat: Array[VarSym], fact: Array[ProxyObject], limit: Int, len: Int): Env = {
      val env: Env = new Array[ProxyObject](len)
      var i = 0
      while (i < limit) {
        val varName = pat(i)
        if (varName != null)
          env(varName.getStackOffset) = fact(i)
        i = i + 1
      }
      env
    }


    sym match {
      case r: Relation =>
        for ((rule, p) <- dependenciesOf(sym)) {
          // unify all terms with their values.
          val env = unify(p.getIndex2SymTEMPORARY, fact, fact.length, rule.getNumberOfParameters)
          if (env != null) {
            localWorkList.push((rule, env))
          }
        }

      case l: Lattice =>
        for ((rule, p) <- dependenciesOf(sym)) {
          // unify only key terms with their values.
          val numberOfKeys = l.getKeys().length
          val env = unify(p.getIndex2SymTEMPORARY, fact, numberOfKeys, rule.getNumberOfParameters)
          if (env != null) {
            localWorkList.push((rule, env))
          }
        }
    }

  }

  /**
    * A batch of (rule, environment)-pairs pending evaluation.
    *
    * This batch should evaluate the pairs from the begin offset `b` to the end offset `e`.
    */
  class Batch(array: Array[(Constraint, Env)], b: Int, e: Int) extends Callable[Iterator[Interpretation]] {
    def call(): Iterator[Interpretation] = {
      val result = new Array[Interpretation](e - b)
      var index = 0
      while (index < result.length) {
        val (rule, env) = array(b + index)
        val interp = evalBody(rule, env)
        result(index) = interp
        index = index + 1
      }
      result.toIterator
    }
  }

  /**
    * Evaluates each of the rules in parallel.
    */
  private def parallelEval(): Iterator[Interpretation] = {
    val t = System.nanoTime()

    // --- begin parallel execution ---
    val readerTasks = new java.util.ArrayList[Batch]()

    val worklistAsArray = worklist.toArray
    val chunkSize = (worklist.length / (BatchesPerThread * options.getThreads)) + 1
    var b = 0
    while (b < worklistAsArray.length) {
      val e = Math.min(b + chunkSize, worklistAsArray.length)
      val batch = new Batch(worklistAsArray, b, e)
      b = b + chunkSize

      readerTasks.add(batch)
      currentReadTasks += 1
    }

    val result = flatten(readersPool.invokeAll(readerTasks))
    // -- end parallel execution ---

    currentReadTasks = 0
    readersTime += System.nanoTime() - t

    worklist.clear()
    result.flatten
  }

  /**
    * Updates the datastore in parallel.
    */
  private def parallelUpdate(iter: Iterator[Interpretation]): Unit = {
    val t = System.nanoTime()

    // --- begin parallel execution ---
    val tasks = new java.util.ArrayList[Callable[WorkList]]()
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
  private def groupFactsBySymbol(iter: Iterator[Interpretation]): mutable.Map[Table, ArrayBuffer[Array[ProxyObject]]] = {
    val result = mutable.Map.empty[Table, ArrayBuffer[Array[ProxyObject]]]
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
  private def mkFixpoint(elapsed: Long): Fixpoint = new Fixpoint(constraintSet.getRelations(), constraintSet.getLattices())

  /**
    * Returns a new thread pool configured to use the appropriate number of threads.
    */
  private def mkThreadPool(name: String): ExecutorService = options.getThreads match {
    // Case 1: Parallel execution disabled. Use a single thread.
    case 1 => Executors.newSingleThreadExecutor(mkThreadFactory(name))
    // Case 2: Parallel execution enabled. Use the specified number of processors.
    case n => Executors.newFixedThreadPool(n, mkThreadFactory(name))
  }

  /**
    * Returns a new thread factory.
    */
  private def mkThreadFactory(name: String): ThreadFactory = new ThreadFactory {
    val count = new AtomicInteger()

    def newThread(runnable: Runnable): Thread = {
      val t = new Thread(runnable, s"pool-$name-${count.incrementAndGet()}")
      t.setDaemon(false)
      t.setPriority(Thread.NORM_PRIORITY)
      t
    }
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
  private def mkWorkList(): WorkList = new mutable.ArrayStack[(Constraint, Env)]

  /**
    * Returns a fresh (empty) interpretation.
    */
  private def mkInterpretation(): Interpretation = new mutable.ArrayBuffer[(Table, Array[ProxyObject])]()

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
  private def checkTimeout(): Unit = options.getTimeout match {
    case None => // nop
    case Some(timeout) =>
      val elapsed = System.nanoTime() - totalTime
      if (elapsed > timeout.toNanos) {
        stopSolver()
        throw new TimeoutError(timeout, Duration.ofNanos(elapsed))
      }
  }

  /**
    * Returns a shallow copy of the given array `src`.
    */
  @inline
  def copy(src: Array[ProxyObject]): Array[ProxyObject] = {
    val dst = new Array[ProxyObject](src.length)
    System.arraycopy(src, 0, dst, 0, src.length)
    dst
  }

  /////////////////////////////////////////////////////////////////////////////
  // Dependencies                                                            //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * Computes the dependencies between rules in the program.
    *
    * A dependency can only exist between two rules in the same stratum.
    */
  private def initDependencies(): Unit = {
    // Iterate through each stratum.
    for (stratum <- constraintSet.getConstraintsByStrata) {
      // Initialize the dependencies of every symbol to the empty set.
      for (rule <- stratum) {
        rule.getHeadPredicate() match {
          case p: AtomPredicate => dependenciesOf.update(p.getSym, Set.empty)
          case _ => // nop
        }
      }

      // Compute the dependencies via cross product.
      for (outerRule <- stratum if outerRule.isRule) {
        for (innerRule <- stratum if innerRule.isRule) {
          for (body <- innerRule.getBodyPredicates()) {
            // Loop through the atoms of the inner rule.
            (outerRule.getHeadPredicate(), body) match {
              case (outer: AtomPredicate, inner: AtomPredicate) =>
                // We have found a head and body atom. Check if they share the same symbol.
                if (outer.getSym == inner.getSym) {
                  // The symbol is the same. Update the dependencies.
                  val deps = dependenciesOf(outer.getSym)
                  dependenciesOf(outer.getSym) = deps + ((innerRule, inner))
                }
              case _ => // nop
            }
          }
        }
      }
    }
  }

}
