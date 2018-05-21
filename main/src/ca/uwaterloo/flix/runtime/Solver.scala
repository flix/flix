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

import java.util.concurrent._
import java.util.concurrent.atomic.AtomicInteger

import ca.uwaterloo.flix.api.{Flix, RuleException, Tag, TimeoutException}
import ca.uwaterloo.flix.language.ast.Ast.Polarity
import ca.uwaterloo.flix.language.ast.ExecutableAst.Term.Body.Pat
import ca.uwaterloo.flix.language.ast.ExecutableAst._
import ca.uwaterloo.flix.language.ast.{ExecutableAst, Symbol}
import ca.uwaterloo.flix.runtime.datastore.{DataStore, KeyCache, ProxyObject}
import ca.uwaterloo.flix.runtime.debugger.RestServer
import ca.uwaterloo.flix.runtime.interpreter.Value
import ca.uwaterloo.flix.util._

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
class Solver(val root: ExecutableAst.Root, options: Options)(implicit flix: Flix) {

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
  type Interpretation = mutable.ArrayBuffer[(Symbol.TableSym, Array[ProxyObject])]

  /**
    * The type of the dependency graph, a map from symbols to (constraint, atom) pairs.
    */
  type DependencyGraph = mutable.Map[Symbol.TableSym, Set[(Constraint, Predicate.Body.Atom)]]

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
    * The key cache holds a bi-map from key values to integers.
    *
    * Reading and writing to the cache is guaranteed to be thread-safe.
    */
  val keyCache = new KeyCache()

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

  /**
    * The model (if it exists).
    */
  @volatile
  var model: Model = _

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
  def solve(): Model = try {
    // initialize the solver.
    initSolver()

    // initialize the dependencies.
    initDependencies()

    // initialize the datastore.
    initDataStore()

    // compute the fixedpoint for each stratum, in sequence.
    for (stratum <- root.strata) {
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

  def getRuleStats: List[(Constraint, Int, Long)] = {
    val constraints = root.strata.flatMap(_.constraints)
    constraints.filter(_.isRule).sortBy(_.time.get()).reverse.map {
      case r => (r, r.hits.get(), r.time.get())
    }
  }

  /**
    * Initialize the solver by starting the monitor, debugger, etc.
    */
  private def initSolver(): Unit = {
    if (options.monitor) {
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
    val stratum0 = root.strata.head

    // iterate through all facts.
    for (constraint <- stratum0.constraints) {
      if (constraint.isFact) {
        // evaluate the head of each fact.
        val interp = mkInterpretation()
        evalHead(constraint.head, Array.empty, interp)

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
    }
    initTime = System.nanoTime() - t
  }

  /**
    * Initialize the worklist with every rule (under the empty environment) in the given `stratum`.
    */
  private def initWorkList(stratum: Stratum): Unit = {
    // add all rules to the worklist (under empty environments).
    for (rule <- stratum.constraints) {
      worklist.push((rule, new Array[ProxyObject](rule.arity)))
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
    if (options.verbosity == Verbosity.Verbose) {
      val solverTime = totalTime / 1000000
      val initMiliSeconds = initTime / 1000000
      val readersMiliSeconds = readersTime / 1000000
      val writersMiliSeconds = writersTime / 1000000
      val initialFacts = root.strata.head.constraints.count(_.isFact)
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
    evalCross(rule, rule.atoms, env, interp)

    rule.hits.incrementAndGet()
    rule.time.addAndGet(System.nanoTime() - t)

    interp
  }

  /**
    * Computes the cross product of all collections in the body.
    */
  private def evalCross(rule: Constraint, ps: List[Predicate.Body.Atom], env: Env, interp: Interpretation): Unit = ps match {
    case Nil =>
      // cross product complete, now filter
      evalLoop(rule, rule.loops, env, interp)
    case p :: xs =>
      // Compute the rows that match the atom.
      val rows = evalAtom(p, env)

      // Case split on whether the atom is positive or negative.
      p.polarity match {
        case Polarity.Positive =>
          // Case 1: The atom is positive. Recurse on all matched rows.
          for (newRow <- rows) {
            evalCross(rule, xs, newRow, interp)
          }

        case Polarity.Negative =>
          // Assertion: Check that every variable has been assigned a value.
          for (t <- p.terms) {
            t match {
              case Term.Body.Var(sym, tpe, loc) =>
                assert(env(sym.getStackOffset) != null, s"Unbound variable in negated atom near: ${p.loc.format}")
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
  private def evalAtom(p: ExecutableAst.Predicate.Body.Atom, env: Env): Traversable[Env] = {
    // lookup the relation or lattice.
    val table = root.tables(p.sym) match {
      case r: Table.Relation => dataStore.relations(p.sym)
      case l: Table.Lattice => dataStore.lattices(p.sym)
    }

    // evaluate all terms in the predicate.
    val pat = new Array[ProxyObject](p.arity)
    var i = 0
    while (i < pat.length) {
      val value: ProxyObject = p.terms(i) match {
        case ExecutableAst.Term.Body.Var(sym, _, _) =>
          // A variable is replaced by its value from the environment (or null if unbound).
          env(sym.getStackOffset)
        case ExecutableAst.Term.Body.Lit(lit, _, _) =>
          lit
        case ExecutableAst.Term.Body.Cst(litSym, _, _) =>
          // Every literal is lifted to a function definition and its value is obtained by invoking it.
          Linker.link(litSym, root).invoke(Array.emptyObjectArray)
        case ExecutableAst.Term.Body.Wild(_, _) =>
          // A wildcard places no restrictions on the value.
          null
        case ExecutableAst.Term.Body.Pat(_, _, _) =>
          // A pattern places no restrictions on the value, but is filtered later.
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

        // Check if the term is pattern term.
        // If so, we must checked whether the pattern can be unified with the value.
        p.terms(i) match {
          case term: Pat =>
            val value = matchedRow(i)
            if (!unify(term.pat, value, env)) {
              // Value does not unify with the pattern term. We should skip this row.
              skip = true
            }
          case _ => // nop
        }

        val sym = p.index2sym(i)
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
    * Unfolds the given loop predicates `ps` over the initial `env`.
    */
  private def evalLoop(rule: Constraint, ps: List[Predicate.Body.Loop], env: Env, interp: Interpretation): Unit = ps match {
    case Nil => evalAllFilters(rule, env, interp)
    case Predicate.Body.Loop(sym, term, _) :: rest =>
      val value = evalHeadTerm(term, root, env)
      for (x <- iteratorOf(value)) {
        val newRow = copy(env)
        newRow(sym.getStackOffset) = x
        evalLoop(rule, rest, newRow, interp)
      }
  }

  /**
    * Evaluates all filters in the given `rule`.
    */
  private def evalAllFilters(rule: Constraint, env: Env, interp: Interpretation): Unit = {
    // Extract the filters function predicates from the rule.
    val filters = rule.filters

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
    evalHead(rule.head, env, interp)
  }

  /**
    * Evaluates the given `filter` and returns its result.
    */
  private def evalFilter(filter: Predicate.Body.Filter, env: Env): Boolean = filter match {
    case Predicate.Body.Filter(sym, terms, _) =>
      // Evaluate the arguments of the filter function predicate.
      val args = new Array[AnyRef](terms.length)
      var j = 0
      // Iterate through each term of the filter function predicate.
      while (j < args.length) {
        // Compute the value of the term.
        val value = terms(j) match {
          case Term.Body.Var(x, _, _) =>
            // A variable is replaced by its value from the environment.
            env(x.getStackOffset)
          case Term.Body.Lit(lit, _, _) =>
            lit
          case Term.Body.Cst(litSym, _, _) =>
            // Every literal is lifted to a function definition and its value is obtained by invoking it.
            Linker.link(litSym, root).invoke(Array.emptyObjectArray)
          case Term.Body.Wild(_, _) =>
            // A wildcard should not appear as an argument to a filter function.
            throw InternalRuntimeException("Wildcard not allowed here!")
          case Term.Body.Pat(_, _, _) =>
            // A pattern should not appear as an argument to a filter function.
            throw InternalRuntimeException("Pattern not allowed here!")
        }

        // Store the value of the term into the argument array.
        args(j) = value.getValue
        j = j + 1
      }

      // Link the filter function invocation target (if not already done).
      if (filter.target == null) {
        filter.target = Linker.link(sym, root)
      }

      // Evaluate the filter function passing the arguments.
      val result = filter.target.invoke(args).getValue

      // Return the result.
      result.asInstanceOf[java.lang.Boolean].booleanValue()
  }

  /**
    * Evaluates the given head predicate `p` under the given environment `env0`.
    */
  private def evalHead(p: Predicate.Head, env: Env, interp: Interpretation): Unit = p match {
    case p: Predicate.Head.Atom =>
      val terms = p.termsAsArray
      val fact = new Array[ProxyObject](p.arity)
      var i = 0
      while (i < fact.length) {
        val term: ProxyObject = evalHeadTerm(terms(i), root, env)
        fact(i) = term
        i = i + 1

        // TODO: Experiment with keyCache.
        //if(i != fact.length)
        //  keyCache.put(term)
      }

      interp += ((p.sym, fact))
    case Predicate.Head.True(loc) => // nop
    case Predicate.Head.False(loc) => throw RuleException(s"The integrity rule defined at ${loc.format} is violated.", loc)
  }

  /**
    * Evaluates the given head term `t` under the given environment `env0`
    */
  def evalHeadTerm(t: Term.Head, root: Root, env: Env): ProxyObject = t match {
    case Term.Head.Var(sym, _, _) => env(sym.getStackOffset)
    case Term.Head.Lit(lit, _, _) => lit
    case Term.Head.Cst(litSym, _, _) =>
      // Every literal is lifted to a function definition and its value is obtained by invoking it.
      Linker.link(litSym, root).invoke(Array.emptyObjectArray)
    case Term.Head.App(sym, syms, _, _) =>
      val args = new Array[AnyRef](syms.length)
      var i = 0
      while (i < args.length) {
        args(i) = env(syms(i).getStackOffset).getValue
        i = i + 1
      }
      Linker.link(sym, root).invoke(args)
  }

  /**
    * Returns a callable to process a collection of inferred `facts` for the relation or lattice with the symbol `sym`.
    */
  private def inferredFacts(sym: Symbol.TableSym, facts: ArrayBuffer[Array[ProxyObject]]): Callable[WorkList] = () => {
    val localWorkList = mkWorkList()
    for (fact <- facts) {
      inferredFact(sym, fact, localWorkList)
    }
    localWorkList
  }

  /**
    * Processes an inferred `fact` for the relation or lattice with the symbol `sym`.
    */
  private def inferredFact(sym: Symbol.TableSym, fact: Array[ProxyObject], localWorkList: WorkList): Unit = root.tables(sym) match {
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
    * Returns all dependencies of the given symbol `sym` along with an environment.
    */
  private def dependencies(sym: Symbol.TableSym, fact: Array[ProxyObject], localWorkList: WorkList): Unit = {

    def unify(pat: Array[Symbol.VarSym], fact: Array[ProxyObject], limit: Int, len: Int): Env = {
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


    root.tables(sym) match {
      case r: ExecutableAst.Table.Relation =>
        for ((rule, p) <- dependenciesOf(sym)) {
          // unify all terms with their values.
          val env = unify(p.index2sym, fact, fact.length, rule.arity)
          if (env != null) {
            localWorkList.push((rule, env))
          }
        }

      case l: ExecutableAst.Table.Lattice =>
        for ((rule, p) <- dependenciesOf(sym)) {
          // unify only key terms with their values.
          val numberOfKeys = l.keys.length
          val env = unify(p.index2sym, fact, numberOfKeys, rule.arity)
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
    val chunkSize = (worklist.length / (BatchesPerThread * options.threads)) + 1
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
  private def groupFactsBySymbol(iter: Iterator[Interpretation]): mutable.Map[Symbol.TableSym, ArrayBuffer[Array[ProxyObject]]] = {
    val result = mutable.Map.empty[Symbol.TableSym, ArrayBuffer[Array[ProxyObject]]]
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
    val definitions = root.defs.foldLeft(Map.empty[Symbol.DefnSym, () => ProxyObject]) {
      case (macc, (sym, defn)) =>
        // Invokes the function with a single argument (which is supposed to be the Unit value, but we pass null instead).
        val args: Array[AnyRef] = Array(null)
        macc + (sym -> (() => Linker.link(sym, root).invoke(args)))
    }

    val relations = dataStore.relations.foldLeft(Map.empty[Symbol.TableSym, Iterable[List[ProxyObject]]]) {
      case (macc, (sym, relation)) =>
        val table = relation.scan.toIterable.map(_.toList)
        macc + ((sym, table))
    }

    val lattices = dataStore.lattices.foldLeft(Map.empty[Symbol.TableSym, Iterable[(List[ProxyObject], ProxyObject)]]) {
      case (macc, (sym, lattice)) =>
        val table = lattice.scan.toIterable.map {
          case (keys, values) => (keys.toArray.toList, values)
        }
        macc + ((sym, table))
    }

    model = new Model(root, definitions, relations, lattices)
    model
  }

  /**
    * Returns a new thread pool configured to use the appropriate number of threads.
    */
  private def mkThreadPool(name: String): ExecutorService = options.threads match {
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
  private def mkInterpretation(): Interpretation = new mutable.ArrayBuffer[(Symbol.TableSym, Array[ProxyObject])]()

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
    for (stratum <- root.strata) {
      // Retrieve the constraints in the current stratum.
      val constraints = stratum.constraints

      // Initialize the dependencies of every symbol to the empty set.
      for (rule <- constraints) {
        rule.head match {
          case Predicate.Head.Atom(sym, _, _) => dependenciesOf.update(sym, Set.empty)
          case _ => // nop
        }
      }

      // Compute the dependencies via cross product.
      for (outerRule <- constraints if outerRule.isRule) {
        for (innerRule <- constraints if innerRule.isRule) {
          for (body <- innerRule.body) {
            // Loop through the atoms of the inner rule.
            (outerRule.head, body) match {
              case (outer: Predicate.Head.Atom, inner: Predicate.Body.Atom) =>
                // We have found a head and body atom. Check if they share the same symbol.
                if (outer.sym == inner.sym) {
                  // The symbol is the same. Update the dependencies.
                  val deps = dependenciesOf(outer.sym)
                  dependenciesOf(outer.sym) = deps + ((innerRule, inner))
                }
              case _ => // nop
            }
          }
        }
      }
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // Unification                                                             //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * Tries to unify the given pattern `p0` with the given value `v0` under the environment `env0`.
    *
    * Mutates the given map. Returns `true` if unification was successful.
    */
  private def unify(p0: Pattern, v0: AnyRef, env0: Array[ProxyObject]): Boolean = (p0, v0) match {
    case (Pattern.Wild(_, _), _) => true
    case (Pattern.Var(sym, _, _), _) =>
      val v2 = env0(sym.getStackOffset)
      // TODO: Value unification.
      throw InternalRuntimeException(s"Currently broken.")
    case (Pattern.Unit(_), Value.Unit) => true
    case (Pattern.True(_), java.lang.Boolean.TRUE) => true
    case (Pattern.False(_), java.lang.Boolean.FALSE) => true
    case (Pattern.Char(lit, _), o: java.lang.Character) => lit == o.charValue()
    case (Pattern.Float32(lit, _), o: java.lang.Float) => lit == o.floatValue()
    case (Pattern.Float64(lit, _), o: java.lang.Double) => lit == o.doubleValue()
    case (Pattern.Int8(lit, _), o: java.lang.Byte) => lit == o.byteValue()
    case (Pattern.Int16(lit, _), o: java.lang.Short) => lit == o.shortValue()
    case (Pattern.Int32(lit, _), o: java.lang.Integer) => lit == o.intValue()
    case (Pattern.Int64(lit, _), o: java.lang.Long) => lit == o.longValue()
    case (Pattern.BigInt(lit, _), o: java.math.BigInteger) => lit.equals(o)
    case (Pattern.Str(lit, _), o: java.lang.String) => lit.equals(o)
    case (Pattern.Tag(enum, tag, p, _, _), o: Value.Tag) => if (tag.equals(o.tag)) unify(p, o.value, env0) else false
    case (Pattern.Tag(enum, tag, p, _, _), o: Tag) => if (tag == o.getTag) unify(p, o.getBoxedTagValue, env0) else false
    case (Pattern.Tuple(elms, _, _), o: Array[AnyRef]) =>
      if (elms.length != o.length)
        return false
      var i: Int = 0
      while (i < o.length) {
        val pi = elms(i)
        val vi = o(i)
        val success = unify(pi, vi, env0)
        if (!success)
          return false
        i = i + 1
      }
      true
    case _ =>
      // Unification failed. Return `null`.
      false
  }

  /////////////////////////////////////////////////////////////////////////////
  // Iterators                                                               //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * Return an iterator over the given Set.
    */
  private def iteratorOf(value: AnyRef): Iterator[ProxyObject] = {
    def visit(o: AnyRef): List[AnyRef] = {
      val taggedValue = o.asInstanceOf[Value.Tag]
      if (taggedValue.tag == "Nil") {
        Nil
      } else {
        val hd = taggedValue.value.asInstanceOf[Array[AnyRef]](0)
        val tl = taggedValue.value.asInstanceOf[Array[AnyRef]](1)
        hd :: visit(tl)
      }
    }

    // val list = value.asInstanceOf[Value.Tag].value
    // visit(list).iterator
    // TODO: Loop predicate.
    throw InternalRuntimeException(s"Currently broken.")
  }

}
