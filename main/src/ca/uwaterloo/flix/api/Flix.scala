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

import java.nio.charset.Charset
import java.nio.file.{Files, Path, Paths}

import ca.uwaterloo.flix.language.ast.Ast.Input
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.phase._
import ca.uwaterloo.flix.language.phase.jvm.JvmBackend
import ca.uwaterloo.flix.language.{CompilationError, GenSym}
import ca.uwaterloo.flix.runtime.quickchecker.QuickChecker
import ca.uwaterloo.flix.runtime.verifier.Verifier
import ca.uwaterloo.flix.runtime.CompilationResult
import ca.uwaterloo.flix.util._
import ca.uwaterloo.flix.util.vt.TerminalContext

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * Main programmatic interface for Flix.
  */
class Flix {

  /**
    * A sequence of strings to parsed into Flix ASTs.
    */
  private val strings = ListBuffer.empty[String]

  /**
    * A sequence of paths to be parsed into Flix ASTs.
    */
  private val paths = ListBuffer.empty[Path]

  /**
    * A sequence of inputs to be parsed into Flix ASTs.
    */
  private val inputs = ListBuffer.empty[Input]

  /**
    * A set of reachable root definitions.
    */
  private val reachableRoots = mutable.Set.empty[Symbol.DefnSym]

  /**
    * A sequence of internal inputs to be parsed into Flix ASTs.
    *
    * The core library *must* be present for any program to compile.
    */
  private val coreLibrary = List(
    // Operators
    "Neg.flix" -> LocalResource.get("/src/library/Neg.flix"),
    "Add.flix" -> LocalResource.get("/src/library/Add.flix"),
    "Sub.flix" -> LocalResource.get("/src/library/Sub.flix"),
    "Mul.flix" -> LocalResource.get("/src/library/Mul.flix"),
    "Div.flix" -> LocalResource.get("/src/library/Div.flix"),
    "Rem.flix" -> LocalResource.get("/src/library/Rem.flix"),
    "Exp.flix" -> LocalResource.get("/src/library/Exp.flix"),
    "BitwiseNot.flix" -> LocalResource.get("/src/library/BitwiseNot.flix"),
    "BitwiseAnd.flix" -> LocalResource.get("/src/library/BitwiseAnd.flix"),
    "BitwiseOr.flix" -> LocalResource.get("/src/library/BitwiseOr.flix"),
    "BitwiseXor.flix" -> LocalResource.get("/src/library/BitwiseXor.flix"),
    "BitwiseShl.flix" -> LocalResource.get("/src/library/BitwiseShl.flix"),
    "BitwiseShr.flix" -> LocalResource.get("/src/library/BitwiseShr.flix"),
    "Eq.flix" -> LocalResource.get("/src/library/Eq.flix"),
    "Ord.flix" -> LocalResource.get("/src/library/Ord.flix"),

    // Lattices
    "PreOrder.flix" -> LocalResource.get("/src/library/PreOrder.flix"),
    "PartialOrder.flix" -> LocalResource.get("/src/library/PartialOrder.flix"),
    "LowerBound.flix" -> LocalResource.get("/src/library/LowerBound.flix"),
    "UpperBound.flix" -> LocalResource.get("/src/library/UpperBound.flix"),
    "JoinLattice.flix" -> LocalResource.get("/src/library/JoinLattice.flix"),
    "MeetLattice.flix" -> LocalResource.get("/src/library/MeetLattice.flix"),

    // String
    "ToString.flix" -> LocalResource.get("/src/library/ToString.flix")
  )

  /**
    * A sequence of internal inputs to be parsed into Flix ASTs.
    *
    * The standard library is not required to be present for at least some programs to compile.
    */
  private val standardLibrary = List(
    "Array.flix" -> LocalResource.get("/src/library/Array.flix"),
    "Bool.flix" -> LocalResource.get("/src/library/Bool.flix"),
    "BigInt.flix" -> LocalResource.get("/src/library/BigInt.flix"),
    "Char.flix" -> LocalResource.get("/src/library/Char.flix"),
    "Choice.flix" -> LocalResource.get("/src/library/Choice.flix"),
    "Console.flix" -> LocalResource.get("/src/library/Console.flix"),
    "Float32.flix" -> LocalResource.get("/src/library/Float32.flix"),
    "Float64.flix" -> LocalResource.get("/src/library/Float64.flix"),
    "Int8.flix" -> LocalResource.get("/src/library/Int8.flix"),
    "Int16.flix" -> LocalResource.get("/src/library/Int16.flix"),
    "Int32.flix" -> LocalResource.get("/src/library/Int32.flix"),
    "Int64.flix" -> LocalResource.get("/src/library/Int64.flix"),
    "List.flix" -> LocalResource.get("/src/library/List.flix"),
    "LazyList.flix" -> LocalResource.get("/src/library/LazyList.flix"),
    "LazyList2.flix" -> LocalResource.get("/src/library/LazyList2.flix"),
    "Map.flix" -> LocalResource.get("/src/library/Map.flix"),
    "Nel.flix" -> LocalResource.get("/src/library/Nel.flix"),
    "Object.flix" -> LocalResource.get("/src/library/Object.flix"),
    "Option.flix" -> LocalResource.get("/src/library/Option.flix"),
    "Prelude.flix" -> LocalResource.get("/src/library/Prelude.flix"),
    "Random.flix" -> LocalResource.get("/src/library/Random.flix"),
    "Result.flix" -> LocalResource.get("/src/library/Result.flix"),
    "Set.flix" -> LocalResource.get("/src/library/Set.flix"),
    "String.flix" -> LocalResource.get("/src/library/String.flix"),

    "MutList.flix" -> LocalResource.get("/src/library/MutList.flix"),
    "MutSet.flix" -> LocalResource.get("/src/library/MutSet.flix"),
    "MutMap.flix" -> LocalResource.get("/src/library/MutMap.flix"),

    "Core/Io/File.flix" -> LocalResource.get("/src/library/Core/Io/File.flix"),
    "Core/Io/InputStream.flix" -> LocalResource.get("/src/library/Core/Io/InputStream.flix"),
    "Core/Io/IOError.flix" -> LocalResource.get("/src/library/Core/Io/IOError.flix"),
    "Core/Io/OutputStream.flix" -> LocalResource.get("/src/library/Core/Io/OutputStream.flix"),
    "Core/Io/ZipInput.flix" -> LocalResource.get("/src/library/Core/Io/ZipInput.flix"),
    "Core/Io/ZipOutput.flix" -> LocalResource.get("/src/library/Core/Io/ZipOutput.flix"),
    "Core/Cmp/Ordering.flix" -> LocalResource.get("/src/library/Core/Cmp/Ordering.flix"),

    "FromString.flix" -> LocalResource.get("/src/library/FromString.flix"),
    "Functor.flix" -> LocalResource.get("/src/library/Functor.flix"),
    "Hash.flix" -> LocalResource.get("/src/library/Hash.flix"),
    "Monoid.flix" -> LocalResource.get("/src/library/Monoid.flix"),

    "Bounded.flix" -> LocalResource.get("/src/library/Bounded.flix"),
    "TotalOrder.flix" -> LocalResource.get("/src/library/TotalOrder.flix"),
    "Validation.flix" -> LocalResource.get("/src/library/Validation.flix"),

    "Channel.flix" -> LocalResource.get("/src/library/Channel.flix"),
    "Ticker.flix" -> LocalResource.get("/src/library/Ticker.flix"),
    "Timer.flix" -> LocalResource.get("/src/library/Timer.flix"),
    "Duration.flix" -> LocalResource.get("/src/library/Duration.flix"),
    "Instant.flix" -> LocalResource.get("/src/library/Instant.flix"),

    "StringBuilder.flix" -> LocalResource.get("/src/library/StringBuilder.flix"),
    "RedBlackTree.flix" -> LocalResource.get("/src/library/RedBlackTree.flix"),

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
    * Adds the given string `s` to the list of strings to be parsed.
    */
  def addStr(s: String): Flix = {
    if (s == null)
      throw new IllegalArgumentException("'s' must be non-null.")
    strings += s
    this
  }

  /**
    * Adds the given path `p` to the list of paths to be parsed.
    */
  def addPath(p: String): Flix = {
    if (p == null)
      throw new IllegalArgumentException("'p' must be non-null.")
    paths += Paths.get(p)
    this
  }

  /**
    * Adds the given string `text` with the given `name`.
    */
  def addInput(name: String, text: String): Flix = {
    if (name == null)
      throw new IllegalArgumentException("'name' must be non-null.")
    if (text == null)
      throw new IllegalArgumentException("'text' must be non-null.")
    inputs += Input.Internal(name, text)
    this
  }

  /**
    * Adds the given path `p` to the list of paths to be parsed.
    */
  def addPath(p: Path): Flix = {
    if (p == null)
      throw new IllegalArgumentException("'p' must be non-null.")
    if (!Files.exists(p))
      throw new IllegalArgumentException("'p' must a file.")
    if (!Files.isRegularFile(p))
      throw new IllegalArgumentException("'p' must a regular file.")
    if (!Files.isReadable(p))
      throw new IllegalArgumentException("'p' must a readable file.")

    paths += p
    this
  }

  /**
    * Adds the given fully-qualified name as a reachable root.
    */
  def addReachableRoot(fqn: String): scala.Unit = {
    reachableRoots += Symbol.mkDefnSym(fqn)
  }

  /**
    * Returns the reachable root definitions.
    */
  def getReachableRoots: Set[Symbol.DefnSym] = reachableRoots.toSet

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
    * Compiles the Flix program and returns a typed ast.
    */
  def check(): Validation[TypedAst.Root, CompilationError] = {
    // Initialize fork join pool.
    initForkJoin()

    // Reset the phase information.
    phaseTimers = ListBuffer.empty

    // Construct the compiler pipeline.
    val pipeline =
      Reader |>
        Parser |>
        Weeder |>
        Namer |>
        Resolver |>
        Typer |>
        Instances |>
        Stratifier |>
        PatternExhaustiveness |>
        Redundancy |>
        Linter |>
        Safety

    // Apply the pipeline to the parsed AST.
    val result = pipeline.run(getInputs)(this)

    // Shutdown fork join pool.
    shutdownForkJoin()

    // Return the result.
    result
  }

  /**
    * Compiles the given typed ast to an executable ast.
    */
  def codeGen(typedAst: TypedAst.Root): Validation[CompilationResult, CompilationError] = {
    // Initialize fork join pool.
    initForkJoin()

    // Construct the compiler pipeline.
    val pipeline = Documentor |>
      Monomorph |>
      Synthesize |>
      Simplifier |>
      ClosureConv |>
      LambdaLift |>
      Tailrec |>
      Inliner |>
      Optimizer |>
      TreeShaker |>
      VarNumbering |>
      Finalize |>
      QuickChecker |>
      Verifier |>
      JvmBackend |>
      Finish

    // Apply the pipeline to the parsed AST.
    val result = pipeline.run(typedAst)(this)

    // Shutdown fork join pool.
    shutdownForkJoin()

    // Return the result.
    result
  }

  /**
    * Compiles the given typed ast to an executable ast.
    */
  def compile(): Validation[CompilationResult, CompilationError] =
    check() flatMap {
      case typedAst => codeGen(typedAst)
    }

  /**
    * Enters the phase with the given name.
    */
  def phase[A](phase: String)(f: => A): A = {
    // Initialize the phase time object.
    currentPhase = PhaseTime(phase, 0, Nil)

    // Measure the execution time.
    val t = System.nanoTime()
    val r = f
    val e = System.nanoTime() - t

    // Update the phase time.
    currentPhase = currentPhase.copy(time = e)

    // And add it to the list of executed phases.
    phaseTimers += currentPhase

    // Print performance information if in verbose mode.
    if (options.verbosity == Verbosity.Verbose) {
      // Print information about the phase.
      val d = new Duration(e)
      val terminalCtx = TerminalContext.AnsiTerminal
      val emojiPart = terminalCtx.emitBlue("✓ ")
      val phasePart = terminalCtx.emitBlue(f"$phase%-40s")
      val timePart = f"${d.fmtMiliSeconds}%8s"
      Console.println(emojiPart + phasePart + timePart)

      // Print information about each subphase.
      for ((subphase, e) <- currentPhase.subphases.reverse) {
        val d = new Duration(e)
        val emojiPart = "    "
        val phasePart = terminalCtx.emitMagenta(f"$subphase%-37s")
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
    * Returns a list of inputs constructed from the strings and paths passed to Flix.
    */
  private def getInputs: List[Input] = {
    val si1 = getStringInputs
    val si2 = getPathInputs
    val si3 = inputs.toList
    val si4 = if (options.core) getInputs(coreLibrary) else getInputs(coreLibrary ++ standardLibrary)
    si1 ::: si2 ::: si3 ::: si4
  }

  /**
    * Returns the inputs corresponding to the strings passed to Flix.
    */
  private def getStringInputs: List[Input] = strings.foldLeft(List.empty[Input]) {
    case (xs, s) => Input.Str(s) :: xs
  }

  /**
    * Returns the inputs corresponding to the paths passed to Flix.
    */
  private def getPathInputs: List[Input] = paths.foldLeft(List.empty[Input]) {
    case (xs, p) if p.getFileName.toString.endsWith(".flix") => Input.TxtFile(p) :: xs
    case (xs, p) if p.getFileName.toString.endsWith(".fpkg") => Input.PkgFile(p) :: xs
    case (_, p) => throw new IllegalStateException(s"Unknown file type '${p.getFileName}'.")
  }

  /**
    * Returns the inputs for the given list of (path, text) pairs.
    */
  private def getInputs(xs: List[(String, String)]): List[Input] = xs.foldLeft(List.empty[Input]) {
    case (xs, (name, text)) => Input.Internal(name, text) :: xs
  }

  /**
    * Initializes the fork join pools.
    */
  private def initForkJoin(): Unit = {
    val threads = options.threads.getOrElse(Runtime.getRuntime.availableProcessors())
    forkJoinPool = new java.util.concurrent.ForkJoinPool(threads)
    forkJoinTaskSupport = new scala.collection.parallel.ForkJoinTaskSupport(forkJoinPool)
  }

  /**
    * Shuts down the fork join pools.
    */
  private def shutdownForkJoin(): Unit = {
    forkJoinPool.shutdown()
  }

}
