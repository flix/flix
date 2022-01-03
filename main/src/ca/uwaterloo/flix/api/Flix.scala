/*
 * Copyright 2015-2016 Magnus Madsen, Ming-Ho Yee
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

package ca.uwaterloo.flix.api

import ca.uwaterloo.flix.language.ast.Ast.Input
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.phase._
import ca.uwaterloo.flix.language.phase.jvm.JvmBackend
import ca.uwaterloo.flix.language.{CompilationMessage, GenSym}
import ca.uwaterloo.flix.runtime.CompilationResult
import ca.uwaterloo.flix.util.Formatter.NoFormatter
import ca.uwaterloo.flix.util._

import java.net.URI
import java.nio.charset.Charset
import java.nio.file.{Files, Path, Paths}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Flix {
  /**
    * The reserved Flix delimiter.
    */
  val Delimiter: String = "%"
}

/**
  * Main programmatic interface for Flix.
  */
class Flix {

  /**
    * A sequence of inputs to be parsed into Flix ASTs.
    */
  private val inputs = mutable.Map.empty[String, Input]

  /**
    * A set of reachable root definitions.
    */
  private val reachableRoots = mutable.Set.empty[Symbol.DefnSym]

  /**
    * The set of sources changed since last compilation.
    */
  private var changeSet: ChangeSet = ChangeSet.Everything

  /**
    * A cache of compiled ASTs (for incremental compilation).
    */
  private var cachedParsedAst: ParsedAst.Root = ParsedAst.Root(Map.empty)
  private var cachedWeededAst: WeededAst.Root = WeededAst.Root(Map.empty, Set.empty)
  private var cachedTypedAst: TypedAst.Root = TypedAst.Root(Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Set.empty, Map.empty, Map.empty)

  /**
    * A sequence of internal inputs to be parsed into Flix ASTs.
    *
    * The core library *must* be present for any program to compile.
    */
  private val coreLibrary = List(
    // Prelude
    "Prelude.flix" -> LocalResource.get("/src/library/Prelude.flix"),

    // Comparison
    "Comparison.flix" -> LocalResource.get("/src/library/Comparison.flix"),

    // Operators
    "Neg.flix" -> LocalResource.get("/src/library/Neg.flix"),
    "Add.flix" -> LocalResource.get("/src/library/Add.flix"),
    "Sub.flix" -> LocalResource.get("/src/library/Sub.flix"),
    "Mul.flix" -> LocalResource.get("/src/library/Mul.flix"),
    "Div.flix" -> LocalResource.get("/src/library/Div.flix"),
    "Rem.flix" -> LocalResource.get("/src/library/Rem.flix"),
    "Mod.flix" -> LocalResource.get("/src/library/Mod.flix"),
    "Exp.flix" -> LocalResource.get("/src/library/Exp.flix"),
    "BitwiseNot.flix" -> LocalResource.get("/src/library/BitwiseNot.flix"),
    "BitwiseAnd.flix" -> LocalResource.get("/src/library/BitwiseAnd.flix"),
    "BitwiseOr.flix" -> LocalResource.get("/src/library/BitwiseOr.flix"),
    "BitwiseXor.flix" -> LocalResource.get("/src/library/BitwiseXor.flix"),
    "BitwiseShl.flix" -> LocalResource.get("/src/library/BitwiseShl.flix"),
    "BitwiseShr.flix" -> LocalResource.get("/src/library/BitwiseShr.flix"),

    // Built-in
    "Eq.flix" -> LocalResource.get("/src/library/Eq.flix"),
    "Hash.flix" -> LocalResource.get("/src/library/Hash.flix"),
    "Order.flix" -> LocalResource.get("/src/library/Order.flix"),

    // Lattices
    "PartialOrder.flix" -> LocalResource.get("/src/library/PartialOrder.flix"),
    "LowerBound.flix" -> LocalResource.get("/src/library/LowerBound.flix"),
    "UpperBound.flix" -> LocalResource.get("/src/library/UpperBound.flix"),
    "JoinLattice.flix" -> LocalResource.get("/src/library/JoinLattice.flix"),
    "MeetLattice.flix" -> LocalResource.get("/src/library/MeetLattice.flix"),

    // String
    "ToString.flix" -> LocalResource.get("/src/library/ToString.flix"),

    // Boxable
    "Boxable.flix" -> LocalResource.get("/src/library/Boxable.flix"),
    "Boxed.flix" -> LocalResource.get("/src/library/Boxed.flix"),
  )

  /**
    * A sequence of internal inputs to be parsed into Flix ASTs.
    *
    * The standard library is not required to be present for at least some programs to compile.
    */
  private val standardLibrary = List(
    "Array.flix" -> LocalResource.get("/src/library/Array.flix"),
    "Benchmark.flix" -> LocalResource.get("/src/library/Benchmark.flix"),
    "Bool.flix" -> LocalResource.get("/src/library/Bool.flix"),
    "BigInt.flix" -> LocalResource.get("/src/library/BigInt.flix"),
    "Chain.flix" -> LocalResource.get("/src/library/Chain.flix"),
    "Char.flix" -> LocalResource.get("/src/library/Char.flix"),
    "Choice.flix" -> LocalResource.get("/src/library/Choice.flix"),
    "Condition.flix" -> LocalResource.get("/src/library/Condition.flix"),
    "Console.flix" -> LocalResource.get("/src/library/Console.flix"),
    "DelayMap.flix" -> LocalResource.get("/src/library/DelayMap.flix"),
    "DemandList.flix" -> LocalResource.get("/src/library/DemandList.flix"),
    "Float32.flix" -> LocalResource.get("/src/library/Float32.flix"),
    "Float64.flix" -> LocalResource.get("/src/library/Float64.flix"),
    "Int8.flix" -> LocalResource.get("/src/library/Int8.flix"),
    "Int16.flix" -> LocalResource.get("/src/library/Int16.flix"),
    "Int32.flix" -> LocalResource.get("/src/library/Int32.flix"),
    "Int64.flix" -> LocalResource.get("/src/library/Int64.flix"),
    "Iterator.flix" -> LocalResource.get("/src/library/Iterator.flix"),
    "LazyList.flix" -> LocalResource.get("/src/library/LazyList.flix"),
    "List.flix" -> LocalResource.get("/src/library/List.flix"),
    "Map.flix" -> LocalResource.get("/src/library/Map.flix"),
    "Nel.flix" -> LocalResource.get("/src/library/Nel.flix"),
    "Object.flix" -> LocalResource.get("/src/library/Object.flix"),
    "Option.flix" -> LocalResource.get("/src/library/Option.flix"),
    "Random.flix" -> LocalResource.get("/src/library/Random.flix"),
    "ReentrantLock.flix" -> LocalResource.get("/src/library/ReentrantLock.flix"),
    "Result.flix" -> LocalResource.get("/src/library/Result.flix"),
    "Set.flix" -> LocalResource.get("/src/library/Set.flix"),
    "String.flix" -> LocalResource.get("/src/library/String.flix"),
    "System.flix" -> LocalResource.get("/src/library/System.flix"),

    "MutDeque.flix" -> LocalResource.get("/src/library/MutDeque.flix"),
    "MutList.flix" -> LocalResource.get("/src/library/MutList.flix"),
    "MutSet.flix" -> LocalResource.get("/src/library/MutSet.flix"),
    "MutMap.flix" -> LocalResource.get("/src/library/MutMap.flix"),

    "File.flix" -> LocalResource.get("/src/library/File.flix"),

    "Environment.flix" -> LocalResource.get("/src/library/Environment.flix"),
    "Epoch.flix" -> LocalResource.get("/src/library/Epoch.flix"),

    "FromString.flix" -> LocalResource.get("/src/library/FromString.flix"),
    "Functor.flix" -> LocalResource.get("/src/library/Functor.flix"),
    "Applicative.flix" -> LocalResource.get("/src/library/Applicative.flix"),
    "Monad.flix" -> LocalResource.get("/src/library/Monad.flix"),
    "SemiGroup.flix" -> LocalResource.get("/src/library/SemiGroup.flix"),
    "Monoid.flix" -> LocalResource.get("/src/library/Monoid.flix"),
    "Foldable.flix" -> LocalResource.get("/src/library/Foldable.flix"),
    "Traversable.flix" -> LocalResource.get("/src/library/Traversable.flix"),

    "Validation.flix" -> LocalResource.get("/src/library/Validation.flix"),

    "Channel.flix" -> LocalResource.get("/src/library/Channel.flix"),
    "Ticker.flix" -> LocalResource.get("/src/library/Ticker.flix"),
    "Timer.flix" -> LocalResource.get("/src/library/Timer.flix"),
    "Duration.flix" -> LocalResource.get("/src/library/Duration.flix"),
    "Instant.flix" -> LocalResource.get("/src/library/Instant.flix"),

    "StringBuilder.flix" -> LocalResource.get("/src/library/StringBuilder.flix"),
    "RedBlackTree.flix" -> LocalResource.get("/src/library/RedBlackTree.flix"),
    "GetOpt.flix" -> LocalResource.get("/src/library/GetOpt.flix"),

    "Fixpoint/Compiler.flix" -> LocalResource.get("/src/library/Fixpoint/Compiler.flix"),
    "Fixpoint/Debugging.flix" -> LocalResource.get("/src/library/Fixpoint/Debugging.flix"),
    "Fixpoint/IndexSelection.flix" -> LocalResource.get("/src/library/Fixpoint/IndexSelection.flix"),
    "Fixpoint/Interpreter.flix" -> LocalResource.get("/src/library/Fixpoint/Interpreter.flix"),
    "Fixpoint/Options.flix" -> LocalResource.get("/src/library/Fixpoint/Options.flix"),
    "Fixpoint/Simplifier.flix" -> LocalResource.get("/src/library/Fixpoint/Simplifier.flix"),
    "Fixpoint/Solver.flix" -> LocalResource.get("/src/library/Fixpoint/Solver.flix"),
    "Fixpoint/Stratifier.flix" -> LocalResource.get("/src/library/Fixpoint/Stratifier.flix"),
    "Fixpoint/ToString.flix" -> LocalResource.get("/src/library/Fixpoint/ToString.flix"),
    "Fixpoint/VarsToIndices.flix" -> LocalResource.get("/src/library/Fixpoint/VarsToIndices.flix"),

    "Fixpoint/Ast/BodyPredicate.flix" -> LocalResource.get("/src/library/Fixpoint/Ast/BodyPredicate.flix"),
    "Fixpoint/Ast/BodyTerm.flix" -> LocalResource.get("/src/library/Fixpoint/Ast/BodyTerm.flix"),
    "Fixpoint/Ast/Constraint.flix" -> LocalResource.get("/src/library/Fixpoint/Ast/Constraint.flix"),
    "Fixpoint/Ast/Datalog.flix" -> LocalResource.get("/src/library/Fixpoint/Ast/Datalog.flix"),
    "Fixpoint/Ast/Denotation.flix" -> LocalResource.get("/src/library/Fixpoint/Ast/Denotation.flix"),
    "Fixpoint/Ast/HeadPredicate.flix" -> LocalResource.get("/src/library/Fixpoint/Ast/HeadPredicate.flix"),
    "Fixpoint/Ast/HeadTerm.flix" -> LocalResource.get("/src/library/Fixpoint/Ast/HeadTerm.flix"),
    "Fixpoint/Ast/Polarity.flix" -> LocalResource.get("/src/library/Fixpoint/Ast/Polarity.flix"),
    "Fixpoint/Ast/PrecedenceGraph.flix" -> LocalResource.get("/src/library/Fixpoint/Ast/PrecedenceGraph.flix"),
    "Fixpoint/Ast/PredSym.flix" -> LocalResource.get("/src/library/Fixpoint/Ast/PredSym.flix"),
    "Fixpoint/Ast/VarSym.flix" -> LocalResource.get("/src/library/Fixpoint/Ast/VarSym.flix"),

    "Fixpoint/Ram/BoolExp.flix" -> LocalResource.get("/src/library/Fixpoint/Ram/BoolExp.flix"),
    "Fixpoint/Ram/RamStmt.flix" -> LocalResource.get("/src/library/Fixpoint/Ram/RamStmt.flix"),
    "Fixpoint/Ram/RamSym.flix" -> LocalResource.get("/src/library/Fixpoint/Ram/RamSym.flix"),
    "Fixpoint/Ram/RamTerm.flix" -> LocalResource.get("/src/library/Fixpoint/Ram/RamTerm.flix"),
    "Fixpoint/Ram/RelOp.flix" -> LocalResource.get("/src/library/Fixpoint/Ram/RelOp.flix"),
    "Fixpoint/Ram/RowVar.flix" -> LocalResource.get("/src/library/Fixpoint/Ram/RowVar.flix"),
  )

  /**
    * A case class to track the compile time spent in a phase and its sub-phases.
    */
  case class PhaseTime(phase: String, time: Long, subphases: List[(String, Long)])

  /**
    * A map to track the time spent in each phase and sub-phase.
    */
  var phaseTimers: ListBuffer[PhaseTime] = ListBuffer.empty

  /**
    * The current phase we are in. Initially null.
    */
  var currentPhase: PhaseTime = _

  /**
    * The progress bar.
    */
  val progressBar: ProgressBar = new ProgressBar

  /**
    * The default assumed charset.
    */
  val defaultCharset: Charset = Charset.forName("UTF-8")

  /**
    * The current Flix options.
    */
  var options: Options = Options.Default

  /**
    * The fork join pool for `this` Flix instance.
    */
  var forkJoinPool: java.util.concurrent.ForkJoinPool = _

  /**
    * The fork join task support for `this` Flix instance.
    */
  var forkJoinTaskSupport: scala.collection.parallel.ForkJoinTaskSupport = _

  /**
    * The symbol generator associated with this Flix instance.
    */
  val genSym = new GenSym()

  /**
    * The default output formatter.
    */
  private var formatter: Formatter = NoFormatter

  /**
    * A class loader for loading external JARs.
    */
  val jarLoader = new ExternalJarLoader

  /**
    * Adds the given string `s` to the list of strings to be parsed.
    */
  def addSourceCode(s: String): Flix = {
    addSourceCode("<unnamed>", s)
  }

  /**
    * Adds the given string `text` with the given `name`.
    */
  def addSourceCode(name: String, text: String): Flix = {
    if (name == null)
      throw new IllegalArgumentException("'name' must be non-null.")
    if (text == null)
      throw new IllegalArgumentException("'text' must be non-null.")
    addInput(name, Input.Text(name, text, stable = false))
    this
  }

  /**
    * Adds the given path `p` to the list of paths to be parsed.
    */
  def addSourcePath(p: String): Flix = {
    if (p == null)
      throw new IllegalArgumentException("'p' must be non-null.")
    addSourcePath(Paths.get(p))
    this
  }

  /**
    * Adds the given path `p` to the list of paths to be parsed.
    */
  def addSourcePath(p: Path): Flix = {
    if (p == null)
      throw new IllegalArgumentException(s"'p' must be non-null.")
    if (!Files.exists(p))
      throw new IllegalArgumentException(s"'$p' must be a file.")
    if (!Files.isRegularFile(p))
      throw new IllegalArgumentException(s"'$p' must be a regular file.")
    if (!Files.isReadable(p))
      throw new IllegalArgumentException(s"'$p' must be a readable file.")

    if (p.getFileName.toString.endsWith(".flix")) {
      addInput(p.toString, Input.TxtFile(p))
    } else if (p.getFileName.toString.endsWith(".fpkg")) {
      addInput(p.toString, Input.PkgFile(p))
    } else {
      throw new IllegalStateException(s"Unknown file type '${p.getFileName}'.")
    }

    this
  }

  /**
    * Adds the given `input` under the given `name`.
    */
  private def addInput(name: String, input: Input): Unit = inputs.get(name) match {
    case None =>
      inputs += name -> input
    case Some(_) =>
      changeSet = changeSet.markChanged(input)
      inputs += name -> input
  }

  /**
    * Adds the JAR file at path `p` to the class loader.
    */
  def addJar(p: String): Flix = {
    val uri = new URI(p)
    val path = Path.of(uri)
    addJar(path)
  }

  /**
    * Adds the JAR file at path `p` to the class loader.
    */
  def addJar(p: Path): Flix = {
    if (p == null)
      throw new IllegalArgumentException(s"'p' must be non-null.")
    if (!Files.exists(p))
      throw new IllegalArgumentException(s"'$p' must be a file.")
    if (!Files.isRegularFile(p))
      throw new IllegalArgumentException(s"'$p' must be a regular file.")
    if (!Files.isReadable(p))
      throw new IllegalArgumentException(s"'$p' must be a readable file.")

    jarLoader.addURL(p.toUri.toURL)
    this
  }

  /**
    * Returns the reachable root definitions.
    */
  def getReachableRoots: Set[Symbol.DefnSym] = reachableRoots.toSet

  /**
    * Adds the given fully-qualified name as a reachable root.
    */
  def addReachableRoot(fqn: String): scala.Unit = {
    reachableRoots += Symbol.mkDefnSym(fqn)
  }

  /**
    * Sets the options used for this Flix instance.
    */
  def setOptions(opts: Options): Flix = {
    if (opts == null)
      throw new IllegalArgumentException("'opts' must be non-null.")
    options = opts
    this
  }

  /**
    * Returns the current formatter instance.
    */
  def getFormatter: Formatter = this.formatter

  /**
    * Sets the output formatter used for this Flix instance.
    */
  def setFormatter(formatter: Formatter): Flix = {
    if (formatter == null)
      throw new IllegalArgumentException("'formatter' must be non-null.")
    this.formatter = formatter
    this
  }

  /**
    * Converts a list of compiler error messages to a list of printable messages.
    * Decides whether or not to print the explanation.
    */
  def mkMessages(errors: Seq[CompilationMessage]): List[String] = {
    if (options.explain || errors.length == 1)
      errors.map(cm => cm.message(formatter) + cm.explain(formatter).getOrElse("")).toList
    else
      errors.map(cm => cm.message(formatter)).toList
  }

  /**
    * Compiles the Flix program and returns a typed ast.
    */
  def check(): Validation[TypedAst.Root, CompilationMessage] = {
    // Mark this object as implicit.
    implicit val flix: Flix = this

    // Initialize fork join pool.
    initForkJoin()

    // Reset the phase information.
    phaseTimers = ListBuffer.empty

    // The compiler pipeline.
    val result = for {
      afterReader <- Reader.run(getInputs)
      afterParser <- Parser.run(afterReader, cachedParsedAst, changeSet)
      afterWeeder <- Weeder.run(afterParser, cachedWeededAst, changeSet)
      afterNamer <- Namer.run(afterWeeder)
      afterResolver <- Resolver.run(afterNamer)
      afterKinder <- Kinder.run(afterResolver)
      afterDeriver <- Deriver.run(afterKinder)
      afterTyper <- Typer.run(afterDeriver, cachedTypedAst, changeSet)
      afterStatistics <- Statistics.run(afterTyper)
      afterInstances <- Instances.run(afterStatistics)
      afterStratifier <- Stratifier.run(afterInstances)
      afterPatternExhaustiveness <- PatternExhaustiveness.run(afterStratifier)
      afterRedundancy <- Redundancy.run(afterPatternExhaustiveness)
      afterTerminator <- Terminator.run(afterRedundancy)
      afterSafety <- Safety.run(afterTerminator)
    } yield {
      if (options.incremental) {
        // We update the caches, but only if incremental compilation is enabled.
        this.cachedParsedAst = afterParser
        this.cachedWeededAst = afterWeeder
        this.cachedTypedAst = afterTyper
      }
      afterSafety
    }

    // Shutdown fork join pool.
    shutdownForkJoin()

    // Reset the progress bar.
    progressBar.complete()

    // Return the result.
    result
  }

  /**
    * Compiles the given typed ast to an executable ast.
    */
  def codeGen(typedAst: TypedAst.Root): Validation[CompilationResult, CompilationMessage] = {
    // Mark this object as implicit.
    implicit val flix: Flix = this

    // Initialize fork join pool.
    initForkJoin()

    val result = for {
      afterDocumentor <- Documentor.run(typedAst)
      afterLowering <- Lowering.run(afterDocumentor)
      afterMonomorph <- Monomorph.run(afterLowering)
      afterSimplifier <- Simplifier.run(afterMonomorph)
      afterClosureConv <- ClosureConv.run(afterSimplifier)
      afterLambdaLift <- LambdaLift.run(afterClosureConv)
      afterTailrec <- Tailrec.run(afterLambdaLift)
      afterInliner <- Inliner.run(afterTailrec)
      afterOptimizer <- Optimizer.run(afterInliner)
      afterTreeShaker <- TreeShaker.run(afterOptimizer)
      afterVarNumbering <- VarNumbering.run(afterTreeShaker)
      afterFinalize <- Finalize.run(afterVarNumbering)
      afterEraser <- Eraser.run(afterFinalize)
      afterJvmBackend <- JvmBackend.run(afterEraser)
      afterFinish <- Finish.run(afterJvmBackend)
    } yield afterFinish

    // Shutdown fork join pool.
    shutdownForkJoin()

    // Reset the progress bar.
    progressBar.complete()

    // Return the result.
    result
  }

  /**
    * Compiles the given typed ast to an executable ast.
    */
  def compile(): Validation[CompilationResult, CompilationMessage] =
    check() flatMap codeGen

  /**
    * Enters the phase with the given name.
    */
  def phase[A](phase: String)(f: => A): A = {
    // Initialize the phase time object.
    currentPhase = PhaseTime(phase, 0, Nil)

    if (options.progress) {
      progressBar.observe(currentPhase.phase, "", sample = false)
    }

    // Measure the execution time.
    val t = System.nanoTime()
    val r = f
    val e = System.nanoTime() - t

    // Update the phase time.
    currentPhase = currentPhase.copy(time = e)

    // And add it to the list of executed phases.
    phaseTimers += currentPhase

    // Print performance information if in verbose mode.
    if (options.debug) {
      // Print information about the phase.
      val d = new Duration(e)
      val emojiPart = formatter.blue("✓ ")
      val phasePart = formatter.blue(f"$phase%-40s")
      val timePart = f"${d.fmtMiliSeconds}%8s"
      Console.println(emojiPart + phasePart + timePart)

      // Print information about each subphase.
      for ((subphase, e) <- currentPhase.subphases.reverse) {
        val d = new Duration(e)
        val emojiPart = "    "
        val phasePart = formatter.magenta(f"$subphase%-37s")
        val timePart = f"(${d.fmtMiliSeconds}%8s)"
        Console.println(emojiPart + phasePart + timePart)
      }
    }

    // Return the result computed by the phase.
    r
  }

  /**
    * Enters the sub-phase with the given name.
    */
  def subphase[A](subphase: String)(f: => A): A = {
    // Measure the execution time.
    val t = System.nanoTime()
    val r = f
    val e = System.nanoTime() - t

    // Update the phase with information about the subphase.
    val subphases = (subphase, e) :: currentPhase.subphases
    currentPhase = currentPhase.copy(subphases = subphases)

    // Return the result computed by the subphase.
    r
  }

  /**
    * A callback to indicate that work has started on the given subtask.
    */
  def subtask(subtask: String, sample: Boolean = false): Unit = {
    if (options.progress) {
      progressBar.observe(currentPhase.phase, subtask, sample)
    }
  }

  /**
    * Returns a list of inputs constructed from the strings and paths passed to Flix.
    */
  private def getInputs: List[Input] = {
    val lib = options.lib match {
      case LibLevel.Nix => Nil
      case LibLevel.Min => getLibraryInputs(coreLibrary)
      case LibLevel.All => getLibraryInputs(coreLibrary ++ standardLibrary)
    }
    inputs.values.toList ::: lib
  }

  /**
    * Returns the inputs for the given list of (path, text) pairs.
    */
  private def getLibraryInputs(xs: List[(String, String)]): List[Input] = xs.foldLeft(List.empty[Input]) {
    case (xs, (name, text)) => Input.Text(name, text, stable = true) :: xs
  }

  /**
    * Initializes the fork join pools.
    */
  private def initForkJoin(): Unit = {
    forkJoinPool = new java.util.concurrent.ForkJoinPool(options.threads)
    forkJoinTaskSupport = new scala.collection.parallel.ForkJoinTaskSupport(forkJoinPool)
  }

  /**
    * Shuts down the fork join pools.
    */
  private def shutdownForkJoin(): Unit = {
    forkJoinPool.shutdown()
  }

}
