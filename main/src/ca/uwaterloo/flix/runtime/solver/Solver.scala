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
import ca.uwaterloo.flix.util._
import flix.runtime.fixpoint.predicate._
import flix.runtime.fixpoint.symbol.{LatSym, PredSym, RelSym, VarSym}
import flix.runtime.fixpoint.term._
import flix.runtime.fixpoint.{ConstantFunction, Constraint, ConstraintSystem, Stratification}
import flix.runtime.{ProxyObject, TimeoutError}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Flix Fixed Point Solver.
  *
  * Based on a variant of semi-naive evaluation.
  *
  * The solver computes the least fixed point of the rules in the given program.
  */
class Solver(@volatile var constraintSystem: ConstraintSystem, stratification: Stratification, options: flix.runtime.fixpoint.Options) {

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
  type WorkList = mutable.Stack[(Constraint, Env)]

  /**
    * The type of a (partial) interpretation.
    *
    * An interpretation is collection of facts (a table symbol associated with an array of facts).
    */
  type Interpretation = mutable.ArrayBuffer[(PredSym, Array[ProxyObject])]

  /**
    * The type of the dependency graph, a map from symbols to (constraint, atom) pairs.
    */
  type DependencyGraph = mutable.Map[PredSym, Set[(Constraint, AtomPredicate)]]

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
  val dataStore = new DataStore[AnyRef]

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
  def solve(): ConstraintSystem = try {
    // initialize the solver.
    initSolver()

    // initialize the dependencies.
    initDependencies()

    // initialize the datastore.
    initDataStore()

    // compute the fixedpoint for each stratum, in sequence.
    for (stratum <- getConstraintsByStrata) {
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
    getModel()
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
    val stratum0 = getConstraintsByStrata(0)

    // iterate through all facts.
    for (constraint <- stratum0) {
      if (constraint.isFact) {
        // evaluate the head of each fact.
        val interp = mkInterpretation()
        evalHead(constraint.getHeadPredicate, Array.empty, interp)

        // iterate through the interpretation.
        for ((sym, fact) <- interp) {
          // update the datastore, but don't compute any dependencies.
          sym match {
            case r: RelSym =>
              dataStore.getRelation(r, constraintSystem).inferredFact(fact)
            case l: LatSym =>
              dataStore.getLattice(l, constraintSystem).inferredFact(fact)
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
      worklist.push((rule, new Array[ProxyObject](rule.getConstraintParameters.length)))
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
      val initialFacts = getConstraintsByStrata.head.count(_.isFact)
      val totalFacts = dataStore.numberOfFacts
      val throughput = ((1000.0 * totalFacts.toDouble) / (solverTime.toDouble + 1.0)).toInt
      Console.println(f"Solved in $solverTime%,d msec. (init: $initMiliSeconds%,d msec, readers: $readersMiliSeconds%,d msec, writers: $writersMiliSeconds%,d msec)")
      Console.println(f"Initial Facts: $initialFacts%,d. Total Facts: $totalFacts%,d.")
      Console.println(f"Throughput: $throughput%,d facts per second.")
    }
  }

  /**
    * Returns a new constraint system with all the facts inferred by the solver.
    */
  private def getModel(): ConstraintSystem = {
    val relationFacts = dataStore.relations.flatMap {
      case (sym, indexedRelation) =>
        indexedRelation.scan.map {
          case row =>
            val cparams = Array.emptyObjectArray
            val terms = row map {
              case proxyObject => LitTerm.of(ConstantFunction.of(proxyObject)): Term
            }
            val head = AtomPredicate.of(sym, true, terms)
            val body = new Array[Predicate](0)

            Constraint.of(new Array[VarSym](0), head, body, null)
        }
    }

    val latticeFacts = dataStore.lattices.flatMap {
      case (sym, indexedLattice) =>
        indexedLattice.scan.map {
          case (key, value) =>
            val cparams = Array.emptyObjectArray
            val terms = (key.toArray.toList ::: value :: Nil) map {
              case proxyObject => LitTerm.of(ConstantFunction.of(proxyObject)): Term
            }
            val head = AtomPredicate.of(sym, true, terms.toArray)
            val body = new Array[Predicate](0)

            Constraint.of(new Array[VarSym](0), head, body, null)
        }
    }

    val facts = (relationFacts ++ latticeFacts).toArray

    ConstraintSystem.of(facts)
  }

  /**
    * Returns a callable which evaluates the body of the given `rule` under the given initial environment `env`.
    *
    * Updates the given interpretation `interp`.
    */
  private def evalBody(rule: Constraint, env: Env): Interpretation = {
    val t = System.nanoTime()

    val interp = mkInterpretation()
    evalCross(rule, rule.getBodyAtoms.toList, env, interp)
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
      evalAllGuards(rule, env, interp)
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
              assert(env(p.getSym.getIndex) != null, s"Unbound variable in negated atom.")
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
  private def evalAtom(p: AtomPredicate, env: Env): Iterable[Env] = {
    // retrieve the terms.
    val terms = p.getTerms

    // lookup the relation or lattice.
    val table = p.getSym match {
      case r: RelSym => dataStore.getRelation(r, constraintSystem)
      case l: LatSym => dataStore.getLattice(l, constraintSystem)
    }

    // evaluate all terms in the predicate.
    val pat = new Array[ProxyObject](p.getTerms.length)
    var i = 0
    while (i < pat.length) {
      val value: ProxyObject = p.getTerms()(i) match {
        case p: VarTerm =>
          // A variable is replaced by its value from the environment (or null if unbound).
          env(p.getSym.getIndex)
        case p: LitTerm =>
          invoke(p.getFunction)
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

      // bind all variables in the terms to their values.
      var i = 0
      while (i < terms.length) {
        val term = terms(i)
        term match {
          case t: VarTerm =>
            val sym = t.getSym
            newRow.update(sym.getIndex, matchedRow(i))
          case _ => // nop
        }

        i = i + 1
      }

      result += newRow

    }

    result
  }

  /**
    * Evaluates all guards in the given `rule`.
    */
  private def evalAllGuards(rule: Constraint, env: Env, interp: Interpretation): Unit = {
    // Extract the filters function predicates from the rule.
    val filters = rule.getGuards

    // Evaluate each filter function predicate one-by-one.
    var i = 0
    while (i < filters.length) {
      // Evaluate the current filter.
      val result = evalGuard(filters(i), env)

      // Return immediately if the filter function returned false.
      if (!result)
        return

      // Otherwise evaluate the next filter function.
      i = i + 1
    }

    // All filter functions returned `true`.
    // Continue evaluation of the head of the rule.
    evalHead(rule.getHeadPredicate, env, interp)
  }

  /**
    * Evaluates the given `guard` and returns its result.
    */
  private def evalGuard(guard: GuardPredicate, env: Env): Boolean = guard match {
    case p: GuardPredicate =>
      // Evaluate the terms to actual arguments.
      val args = evalArgs(p.getArguments, env)

      // Evaluate the guard passing the arguments.
      val result = p.getFunction()(args)

      // Return the result.
      result.getValue.asInstanceOf[Boolean]
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
        val term: ProxyObject = evalHeadTerm(terms(i), constraintSystem, env)
        fact(i) = term
        i = i + 1
      }

      interp += ((p.getSym, fact))

    case p: UnionPredicate => synchronized {
      // Evaluate the terms to actual arguments.
      val args = evalArgs(p.getArguments, env)

      // Evaluate the union function passing the arguments.
      val result = p.getFunction()(args)

      // Cast to a constraint system
      val cs = result.getValue.asInstanceOf[ConstraintSystem]

      // Merge the two constraint sets.
      // TODO: If this also return rules then we should rebuild our data structures...
      // Below we throw an exception if that is the case.
      constraintSystem = flix.runtime.fixpoint.Solver.compose(constraintSystem, cs)

      // Iterate through the facts.
      for (constraint <- cs.getConstraints) {
        if (constraint.isFact) {
          evalHead(constraint.getHeadPredicate, Array.empty, interp)
        }
        if (constraint.isRule) {
          throw InternalRuntimeException(s"Unexpected rule in union predicate: '$constraint'.")
        }
      }
    }
  }

  /**
    * Evaluates the given head term `t` under the given environment `env0`
    */
  def evalHeadTerm(t: Term, root: ConstraintSystem, env: Env): ProxyObject = t match {
    case t: VarTerm => env(t.getSym.getIndex)
    case t: LitTerm => t.getFunction.apply(new Array[AnyRef](1))
    case t: AppTerm =>
      if (t.getArguments.length == 0) {
        // TODO: A small hack. If the function takes zero arguments that actually means it must be invoked with the unit value.
        // This means that it can be invoked with an array of size 1 with null in it.
        t.getFunction()(new Array[AnyRef](1))
      } else {
        val args = new Array[AnyRef](t.getArguments.length)
        var i = 0
        while (i < args.length) {
          args(i) = env(t.getArguments()(i).getIndex).getValue
          i = i + 1
        }
        t.getFunction()(args)
      }
  }

  /**
    * Evaluates the given terms `ts`.
    */
  private def evalArgs(ts: Array[Term], env: Env) = {
    // Evaluate the arguments of the filter function predicate.
    val args = new Array[AnyRef](ts.length)
    var j = 0
    // Iterate through each term of the filter function predicate.
    while (j < args.length) {
      // Compute the value of the term.
      val value: ProxyObject = ts(j) match {
        case p: VarTerm =>
          // A variable is replaced by its value from the environment.
          env(p.getSym.getIndex)
        case p: LitTerm =>
          invoke(p.getFunction)
        case p: WildTerm =>
          // A wildcard should not appear as an argument to a filter function.
          throw InternalRuntimeException("Wildcard not allowed here!")
      }

      // Store the value of the term into the argument array.
      args(j) = value.getValue
      j = j + 1
    }
    args
  }

  /**
    * Returns a callable to process a collection of inferred `facts` for the relation or lattice with the symbol `sym`.
    */
  private def inferredFacts(sym: PredSym, facts: ArrayBuffer[Array[ProxyObject]]): Callable[WorkList] = () => {
    val localWorkList = mkWorkList()
    for (fact <- facts) {
      inferredFact(sym, fact, localWorkList)
    }
    localWorkList
  }

  /**
    * Processes an inferred `fact` for the relation or lattice with the symbol `sym`.
    */
  private def inferredFact(sym: PredSym, fact: Array[ProxyObject], localWorkList: WorkList): Unit = sym match {
    case r: RelSym =>
      val changed = dataStore.getRelation(r, constraintSystem).inferredFact(fact)
      if (changed) {
        dependencies(sym, fact, localWorkList)
      }

    case l: LatSym =>
      val changed = dataStore.getLattice(l, constraintSystem).inferredFact(fact)
      if (changed) {
        dependencies(sym, fact, localWorkList)
      }
  }

  /**
    * Returns all dependencies of the given symbol `sym` along with an environment.
    */
  private def dependencies(sym: PredSym, fact: Array[ProxyObject], localWorkList: WorkList): Unit = {

    def unify(pat: Array[VarSym], fact: Array[ProxyObject], limit: Int, len: Int): Env = {
      val env: Env = new Array[ProxyObject](len)
      var i = 0
      while (i < limit) {
        val varName = pat(i)
        if (varName != null)
          env(varName.getIndex) = fact(i)
        i = i + 1
      }
      env
    }

    def getVarArray(p: AtomPredicate): Array[VarSym] = {
      p.getTerms.map {
        case t: VarTerm => t.getSym
        case _ => null
      }
    }

    sym match {
      case r: RelSym =>
        // TODO: It can happen that a constraint is extended with a new predicate symbol via union,
        //  if so, the dependencies might be empty, but that should be okay.
        for ((rule, p) <- dependenciesOf.getOrElse(sym, Set.empty)) {
          // unify all terms with their values.
          val env = unify(getVarArray(p), fact, fact.length, rule.getConstraintParameters.length)
          if (env != null) {
            localWorkList.push((rule, env))
          }
        }

      case l: LatSym =>
        // TODO: It can happen that a constraint is extended with a new predicate symbol via union,
        //  if so, the dependencies might be empty, but that should be okay.
        for ((rule, p) <- dependenciesOf.getOrElse(sym, Set.empty)) {
          // unify only key terms with their values.
          val numberOfKeys = constraintSystem.getArity(l) - 1
          val env = unify(getVarArray(p), fact, numberOfKeys, rule.getConstraintParameters.length)
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
      result.iterator
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
  private def groupFactsBySymbol(iter: Iterator[Interpretation]): mutable.Map[PredSym, ArrayBuffer[Array[ProxyObject]]] = {
    val result = mutable.Map.empty[PredSym, ArrayBuffer[Array[ProxyObject]]]
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
  private def mkInterpretation(): Interpretation = new mutable.ArrayBuffer[(PredSym, Array[ProxyObject])]()

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
    case null => // nop
    case timeout =>
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
    for (stratum <- getConstraintsByStrata) {
      // Initialize the dependencies of every symbol to the empty set.
      for (rule <- stratum) {
        rule.getHeadPredicate match {
          case p: AtomPredicate => dependenciesOf.update(p.getSym, Set.empty)
          case _ => // nop
        }
      }

      // Compute the dependencies via cross product.
      for (outerRule <- stratum if outerRule.isRule) {
        for (innerRule <- stratum if innerRule.isRule) {
          for (body <- innerRule.getBodyPredicates) {
            // Loop through the atoms of the inner rule.
            (outerRule.getHeadPredicate, body) match {
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

  /**
    * Returns all the constraints in the constraint set by stratum.
    */
  // TODO: Precompute once.
  private val getConstraintsByStrata: Array[Array[Constraint]] = {

    val constraintsWithStratum = constraintSystem.getConstraints.map {
      case c => c.getHeadPredicate match {
        case h: AtomPredicate => (c, stratification.getStratum(h.getSym))
        case h: UnionPredicate => (c, stratification.getMaxStratum)
      }

    }

    // TODO: Not very efficient...
    val result = for ((_, arr) <- constraintsWithStratum.groupBy(_._2).toList.sortBy(_._1))
      yield arr.map(_._1)

    if (result.nonEmpty)
      result.toArray
    else {
      // TODO: Hack, in case the system is empty.
      val a = Array.ofDim[Constraint](1, 1)
      a(0) = Array.empty[Constraint]
      a
    }

  }

  /**
    * Invokes the given function `f` with zero arguments.
    *
    * That is, invokes the function with an object array of length with the null value.
    */
  private def invoke(f: java.util.function.Function[AnyRef, ProxyObject]): ProxyObject = {
    val args = new Array[AnyRef](1)
    f.apply(args)
  }

}
