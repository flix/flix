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
import java.util.concurrent.{ExecutorService, Executors}

import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.phase._
import ca.uwaterloo.flix.language.phase.jvm.JvmBackend
import ca.uwaterloo.flix.language.{CompilationError, GenSym}
import ca.uwaterloo.flix.runtime.quickchecker.QuickChecker
import ca.uwaterloo.flix.runtime.verifier.Verifier
import ca.uwaterloo.flix.runtime.{DeltaSolver, Model, Solver}
import ca.uwaterloo.flix.util.{LocalResource, Options, Validation}
import ca.uwaterloo.flix.util.Validation._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext

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
    * A map of named expressions.
    */
  private val named = mutable.Map.empty[Symbol.DefnSym, String]

  /**
    * A set of reachable root definitions.
    */
  private val reachableRoots = mutable.Set.empty[Symbol.DefnSym]

  /**
    * A sequence of internal inputs to be parsed into Flix ASTs.
    */
  private val library = List(
    "BigInt.flix" -> LocalResource.get("/library/BigInt.flix"),
    "Bounded.flix" -> LocalResource.get("/library/Bounded.flix"),
    "Char.flix" -> LocalResource.get("/library/Char.flix"),
    "Console.flix" -> LocalResource.get("/library/Console.flix"),
    "Float32.flix" -> LocalResource.get("/library/Float32.flix"),
    "Float64.flix" -> LocalResource.get("/library/Float64.flix"),
    "Int8.flix" -> LocalResource.get("/library/Int8.flix"),
    "Int16.flix" -> LocalResource.get("/library/Int16.flix"),
    "Int32.flix" -> LocalResource.get("/library/Int32.flix"),
    "Int64.flix" -> LocalResource.get("/library/Int64.flix"),
    "JoinLattice.flix" -> LocalResource.get("/library/JoinLattice.flix"),
    "List.flix" -> LocalResource.get("/library/List.flix"),
    "Map.flix" -> LocalResource.get("/library/Map.flix"),
    "MeetLattice.flix" -> LocalResource.get("/library/MeetLattice.flix"),
    "Option.flix" -> LocalResource.get("/library/Option.flix"),
    "PartialOrder.flix" -> LocalResource.get("/library/PartialOrder.flix"),
    "Prelude.flix" -> LocalResource.get("/library/Prelude.flix"),
    "Result.flix" -> LocalResource.get("/library/Result.flix"),
    "Set.flix" -> LocalResource.get("/library/Set.flix"),
    "String.flix" -> LocalResource.get("/library/String.flix"),
    "TotalOrder.flix" -> LocalResource.get("/library/TotalOrder.flix"),
    "Tuple.flix" -> LocalResource.get("/library/Tuple.flix"),

    "flix/core/Functor.flix" -> LocalResource.get("/library/flix/core/Functor.flix"),

    "flix/core/cmp/package.flix" -> LocalResource.get("/library/flix/core/cmp/package.flix"),
    "flix/core/cmp/Eq.flix" -> LocalResource.get("/library/flix/core/cmp/Eq.flix"),
    "flix/core/cmp/Ord.flix" -> LocalResource.get("/library/flix/core/cmp/Ord.flix"),
    "flix/core/cmp/PartialEq.flix" -> LocalResource.get("/library/flix/core/cmp/PartialEq.flix"),
    "flix/core/cmp/PartialOrd.flix" -> LocalResource.get("/library/flix/core/cmp/PartialOrd.flix"),
    "flix/core/lattice/JoinSemiLattice.flix" -> LocalResource.get("/library/flix/core/lattice/JoinSemiLattice.flix"),

    "flix/io/BufferedReader.flix" -> LocalResource.get("/library/flix/io/BufferedReader.flix"),
    "flix/io/BufferedWriter.flix" -> LocalResource.get("/library/flix/io/BufferedWriter.flix"),
    "flix/io/InputStream.flix" -> LocalResource.get("/library/flix/io/InputStream.flix"),
    "flix/io/OpenOption.flix" -> LocalResource.get("/library/flix/io/OpenOption.flix"),
    "flix/io/OutputStream.flix" -> LocalResource.get("/library/flix/io/OutputStream.flix"),
    "flix/io/Path.flix" -> LocalResource.get("/library/flix/io/Path.flix"),

  )

  /**
    * The default assumed charset.
    */
  val defaultCharset: Charset = Charset.forName("UTF-8")

  /**
    * The current Flix options.
    */
  var options: Options = Options.Default

  /**
    * The execution context for `this` Flix instance.
    */
  var ec: ExecutionContext = mkExecutionContext(threads = 1)

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
    * Adds the given expression `exp` with the given name `sym`.
    */
  def addNamedExp(sym: Symbol.DefnSym, exp: String): scala.Unit = {
    named += (sym -> exp)
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
    ec = mkExecutionContext(threads = opts.threads)
    options = opts
    this
  }

  /**
    * Compiles the Flix program and returns a typed ast.
    */
  def check(): Validation[TypedAst.Root, CompilationError] = {
    // Construct the compiler pipeline.
    val pipeline =
      Reader |>
        Parser |>
        Weeder |>
        Namer |>
        Resolver |>
        Typer |>
        Uniqueness |>
        Effects |>
        PatternExhaustiveness |>
        Safety

    // Apply the pipeline to the parsed AST.
    pipeline.run((getInputs, named.toMap))(this)
  }

  /**
    * Compiles the given typed ast to an executable ast.
    */
  def codeGen(typedAst: TypedAst.Root): Validation[ExecutableAst.Root, CompilationError] = {
    // Construct the compiler pipeline.
    val pipeline = Documentor |>
      Stratifier |>
      Monomorph |>
      Synthesize |>
      Simplifier |>
      LambdaLift |>
      Tailrec |>
      Inliner |>
      Optimizer |>
      TreeShaker |>
      VarNumbering |>
      CreateExecutableAst |>
      JvmBackend |>
      QuickChecker |>
      Verifier

    // Apply the pipeline to the parsed AST.
    pipeline.run(typedAst)(this)

  }

  /**
    * Compiles the given typed ast to an executable ast.
    */
  def compile(): Validation[ExecutableAst.Root, CompilationError] = {
    check() flatMap {
      case typedAst => codeGen(typedAst)
    }
  }

  /**
    * Runs the Flix fixed point solver on the program and returns the minimal model.
    */
  def solve(): Validation[Model, CompilationError] = compile() flatMap solve

  /**
    * Runs the Flix fixed point solver on the program and returns the minimal model.
    */
  def solve(root: ExecutableAst.Root): Validation[Model, CompilationError] =
    new Solver(root, options)(this).solve().toSuccess

  /**
    * Runs the Flix fixed point solver on the program trying to minimize the
    * number of input facts which cause some unhandled exception.
    *
    * @param path the path to write the minimized facts to.
    */
  def deltaSolve(path: Path): Validation[scala.Unit, CompilationError] = compile().map {
    case root => DeltaSolver.solve(root, options, path)(this)
  }

  /**
    * Returns a list of inputs constructed from the strings and paths passed to Flix.
    */
  private def getInputs: List[Input] = {
    val si1 = getStringInputs
    val si2 = getPathInputs
    val si3 = if (options.core) Nil else getStandardLibraryInputs
    si1 ::: si2 ::: si3
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
    case (xs, p) if p.getFileName.toString.endsWith(".flix.zip") => Input.ZipFile(p) :: xs
    case (xs, p) if p.getFileName.toString.endsWith(".flix.gzip") => Input.ZipFile(p) :: xs
    case (_, p) => throw new IllegalStateException(s"Unknown file type '${p.getFileName}'.")
  }

  /**
    * Returns the inputs for the standard library.
    */
  private def getStandardLibraryInputs: List[Input] = library.foldLeft(List.empty[Input]) {
    case (xs, (name, text)) => Input.Internal(name, text) :: xs
  }

  /**
    * Returns an execution context fixed to the given number of `threads`.
    */
  private def mkExecutionContext(threads: Int): ExecutionContext = {
    val service = mkExecutorService(threads)
    ExecutionContext.fromExecutorService(service)
  }

  /**
    * Returns an executor service fixed to the given number of `threads`.
    */
  private def mkExecutorService(threads: Int): ExecutorService = {
    Executors.newFixedThreadPool(threads, (r: Runnable) => {
      val t = new Thread(r)
      t.setDaemon(true)
      t
    })
  }

}
