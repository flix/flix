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

import ca.uwaterloo.flix.language.ast.*
import ca.uwaterloo.flix.language.ast.shared.{AvailableClasses, Input, SecurityContext, Source}
import ca.uwaterloo.flix.language.dbg.AstPrinter
import ca.uwaterloo.flix.language.fmt.FormatOptions
import ca.uwaterloo.flix.language.phase.*
import ca.uwaterloo.flix.language.phase.jvm.{JvmBackend, JvmLoader, JvmWriter}
import ca.uwaterloo.flix.language.phase.monomorph.Specialization
import ca.uwaterloo.flix.language.phase.optimizer.{LambdaDrop, Optimizer}
import ca.uwaterloo.flix.language.{CompilationMessage, GenSym}
import ca.uwaterloo.flix.runtime.CompilationResult
import ca.uwaterloo.flix.tools.Summary
import ca.uwaterloo.flix.util.*
import ca.uwaterloo.flix.util.Formatter.NoFormatter
import ca.uwaterloo.flix.util.collection.{Chain, MultiMap}
import ca.uwaterloo.flix.util.tc.Debug

import java.net.URI
import java.nio.charset.Charset
import java.nio.file.{Files, Path}
import java.util.concurrent.ForkJoinPool
import java.util.zip.ZipFile
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions
import scala.util.Using

object Flix {
  /**
    * The reserved Flix delimiter.
    */
  val Delimiter: String = "$"

  /**
    * The file extension for intermediate representation files.
    */
  val IrFileExtension = "flixir"

  /**
    * The maximum width of the intermediate representation files.
    */
  val IrFileWidth = 80

  /**
    * The number of spaces per indentation in the intermediate representation files.
    */
  val IrFileIndentation = 4
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
    * The set of sources changed since last compilation.
    */
  private var changeSet: ChangeSet = ChangeSet.Everything

  /**
    * The set of known Java classes and interfaces.
    */
  private var availableClasses: AvailableClasses = AvailableClasses(getJavaPlatformClassesAndInterfaces())

  /**
    * A cache of ASTs for incremental compilation.
    */
  private var cachedLexerTokens: Map[Source, Array[Token]] = Map.empty
  private var cachedParserCst: SyntaxTree.Root = SyntaxTree.empty
  private var cachedWeederAst: WeededAst.Root = WeededAst.empty
  private var cachedDesugarAst: DesugaredAst.Root = DesugaredAst.empty
  private var cachedKinderAst: KindedAst.Root = KindedAst.empty
  private var cachedResolverAst: ResolvedAst.Root = ResolvedAst.empty
  private var cachedTyperAst: TypedAst.Root = TypedAst.empty

  /**
    * Returns the parsed ast.
    */
  def getParsedAst: SyntaxTree.Root = cachedParserCst

  /**
    * A cache of error messages for incremental compilation.
    */
  private var cachedErrors: List[CompilationMessage] = Nil

  /**
    * A map to track the time spent in each phase and sub-phase.
    */
  var phaseTimers: ListBuffer[PhaseTime] = ListBuffer.empty

  /**
    * The current phase we are in. Initially null.
    */
  private var currentPhase: PhaseTime = _

  /**
    * The progress bar.
    */
  private val progressBar: ProgressBar = new ProgressBar

  /**
    * The currently registered event listeners.
    */
  private val listeners: ListBuffer[FlixListener] = ListBuffer.empty

  /**
    * The default assumed charset.
    */
  val defaultCharset: Charset = Charset.forName("UTF-8")

  /**
    * The current Flix options.
    */
  var options: Options = Options.Default

  /**
    * The thread pool executor service for `this` Flix instance.
    */
  var threadPool: java.util.concurrent.ForkJoinPool = _

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
    * Adds Flix source code from a file on the filesystem.
    *
    * @param p    the path to the Flix source file. Must be a readable `.flix` file.
    * @param sctx the security context for the input.
    */
  def addFile(p: Path)(implicit sctx: SecurityContext): Flix = {
    isValidFlixFile(p) match {
      case Result.Err(e: Throwable) => throw e
      case Result.Ok(()) =>
        addInput(p.normalize().toString, Input.RealFile(p, sctx))
        this
    }
  }

  /**
    * Checks that `p` is a valid `.flix` filepath.
    * `p` is valid if all the following holds:
    *   1. `p` must not be `null`.
    *   1. `p` must exist in the file system.
    *   1. `p` must be a regular file.
    *   1. `p` must be readable.
    *   1. `p` must end with `.flix`.
    */
  private def isValidFlixFile(p: Path): Result[Unit, IllegalArgumentException] = {
    if (p == null) {
      return Result.Err(new IllegalArgumentException(s"'p' must be non-null."))
    }
    val pNorm = p.normalize()
    if (!Files.exists(pNorm)) {
      return Result.Err(new IllegalArgumentException(s"'$pNorm' must be a file."))
    }
    if (!Files.isRegularFile(pNorm)) {
      return Result.Err(new IllegalArgumentException(s"'$pNorm' must be a regular file."))
    }
    if (!Files.isReadable(pNorm)) {
      return Result.Err(new IllegalArgumentException(s"'$pNorm' must be a readable file."))
    }
    if (!FileOps.checkExt(pNorm, "flix")) {
      return Result.Err(new IllegalArgumentException(s"'$pNorm' must be a .flix file."))
    }
    Result.Ok(())
  }

  /**
    * Removes Flix source code associated with a file on the filesystem.
    *
    * @param p    the path to the Flix source file. Must be a `.flix` file.
    * @param sctx the security context for the input.
    */
  def remFile(p: Path)(implicit sctx: SecurityContext): Flix = {
    if (!p.getFileName.toString.endsWith(".flix"))
      throw new IllegalArgumentException(s"'$p' must be a *.flix file.")

    remInput(p.toString, Input.RealFile(p, sctx))
    this
  }

  /**
    * Adds Flix source code from a string with an associated virtual path.
    *
    * @param path the virtual path to associate with the source code.
    * @param src  the Flix source code.
    * @param sctx the security context for the input.
    */
  def addVirtualPath(path: Path, src: String)(implicit sctx: SecurityContext): Flix = {
    if (path == null)
      throw new IllegalArgumentException("'path' must be non-null.")
    if (src == null)
      throw new IllegalArgumentException("'src' must be non-null.")
    if (sctx == null)
      throw new IllegalArgumentException("'sctx' must be non-null.")
    addInput(path.toString, Input.VirtualFile(path, src, sctx))
    this
  }

  /**
    * Removes Flix source code associated with a virtual path.
    *
    * @param path the virtual path of the source code to remove.
    */
  def remVirtualPath(path: Path): Flix = {
    if (path == null)
      throw new IllegalArgumentException("'path' must be non-null.")
    remInput(path.toString, Input.VirtualFile(path, "", /* unused */ SecurityContext.Plain))
    this
  }

  /**
    * Adds Flix source code from a string with an associated virtual URI.
    *
    * @param uri  the virtual URI to associate with the source code.
    * @param src  the Flix source code.
    * @param sctx the security context for the input.
    */
  def addVirtualUri(uri: URI, src: String)(implicit sctx: SecurityContext): Flix = {
    if (uri == null)
      throw new IllegalArgumentException("'uri' must be non-null.")
    if (src == null)
      throw new IllegalArgumentException("'src' must be non-null.")
    if (sctx == null)
      throw new IllegalArgumentException("'sctx' must be non-null.")
    addInput(uri.toString, Input.VirtualUri(uri, src, sctx))
    this
  }

  /**
    * Removes Flix source code associated with a virtual URI.
    *
    * @param uri the virtual URI of the source code to remove.
    */
  def remVirtualUri(uri: URI): Flix = {
    if (uri == null)
      throw new IllegalArgumentException("'uri' must be non-null.")
    remInput(uri.toString, Input.VirtualUri(uri, "", /* unused */ SecurityContext.Plain))
    this
  }

  /**
    * Adds Flix source code from a Flix package file (.fpkg).
    *
    * @param p    the path to the Flix package file. Must be a readable `.fpkg` zip archive.
    * @param sctx the security context for the input.
    */
  def addPkg(p: Path)(implicit sctx: SecurityContext): Flix = {
    isValidFpkgFile(p) match {
      case Result.Err(e: Throwable) => throw e
      case Result.Ok(()) =>
        addInput(p.toString, Input.PkgFile(p, sctx))
        this
    }
  }

  /**
    * Checks that `p` is a valid `.fpkg` filepath.
    * `p` is valid if all the following holds:
    *   1. `p` must not be `null`.
    *   1. `p` must exist in the file system.
    *   1. `p` must be a regular file.
    *   1. `p` must be readable.
    *   1. `p` must end with `.fpkg`.
    *   1. `p` must be a zip archive.
    */
  def isValidFpkgFile(p: Path): Result[Unit, IllegalArgumentException] = {
    if (p == null) {
      return Result.Err(new IllegalArgumentException(s"'p' must be non-null."))
    }
    val pNorm = p.normalize()
    if (!Files.exists(pNorm)) {
      return Result.Err(new IllegalArgumentException(s"'$pNorm' must be a file."))
    }
    if (!Files.isRegularFile(pNorm)) {
      return Result.Err(new IllegalArgumentException(s"'$pNorm' must be a regular file."))
    }
    if (!Files.isReadable(pNorm)) {
      return Result.Err(new IllegalArgumentException(s"'$pNorm' must be a readable file."))
    }
    if (!FileOps.checkExt(pNorm, "fpkg")) {
      return Result.Err(new IllegalArgumentException(s"'$pNorm' must be a .fpkg file."))
    }
    if (!FileOps.isZipArchive(pNorm)) {
      return Result.Err(new IllegalArgumentException(s"'$pNorm' must be a zip archive."))
    }
    Result.Ok(())
  }

  /**
    * Adds a JAR file to the class loader and extends the set of known Java classes and interfaces.
    *
    * @param p the path to the JAR file. Must be a readable `.jar` file.
    */
  def addJar(p: Path): Flix = {
    isValidJarFile(p) match {
      case Result.Err(e: Throwable) => throw e
      case Result.Ok(()) =>
        val p1 = p.normalize()
        jarLoader.addURL(p1.toUri.toURL)
        extendKnownJavaClassesAndInterfaces(p1)
        this
    }
  }

  /**
    * Checks that `p` is a valid `.jar` filepath.
    * `p` is valid if all the following holds:
    *   1. `p` must not be `null`.
    *   1. `p` must exist in the file system.
    *   1. `p` must be a regular file.
    *   1. `p` must be readable.
    *   1. `p` must end with `.jar`.
    *   1. `p` must be a zip archive.
    */
  private def isValidJarFile(p: Path): Result[Unit, IllegalArgumentException] = {
    if (p == null) {
      return Result.Err(new IllegalArgumentException(s"'p' must be non-null."))
    }
    val pNorm = p.normalize()
    if (!Files.exists(pNorm)) {
      return Result.Err(new IllegalArgumentException(s"'$pNorm' must be a file."))
    }
    if (!Files.isRegularFile(pNorm)) {
      return Result.Err(new IllegalArgumentException(s"'$pNorm' must be a regular file."))
    }
    if (!Files.isReadable(pNorm)) {
      return Result.Err(new IllegalArgumentException(s"'$pNorm' must be a readable file."))
    }
    if (!FileOps.checkExt(pNorm, "jar")) {
      return Result.Err(new IllegalArgumentException(s"'$pNorm' must be a .jar file."))
    }
    if (!FileOps.isZipArchive(pNorm)) {
      return Result.Err(new IllegalArgumentException(s"'$pNorm' must be a zip archive."))
    }
    Result.Ok(())
  }

  /**
    * Adds the given `input` under the given `name`.
    */
  private def addInput(name: String, input: Input): Unit = inputs.get(name) match {
    case None =>
      inputs += name -> input
    case Some(_) =>
      changeSet = changeSet.markChanged(input, cachedTyperAst.dependencyGraph)
      inputs += name -> input
  }

  /**
    * Removes the given `input` under the given `name`.
    *
    * Note: Removing an input means to replace it by the empty string.
    */
  private def remInput(name: String, input: Input): Unit = inputs.get(name) match {
    case None => // nop
    case Some(_) =>
      changeSet = changeSet.markChanged(input, cachedTyperAst.dependencyGraph)
      inputs += name -> Input.VirtualFile(parsePath(name), "", /* unused */ SecurityContext.Plain)
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
    * Returns the format options associated with this Flix instance.
    */
  def getFormatOptions: FormatOptions = {
    FormatOptions(
      varNames = FormatOptions.VarName.NameBased // TODO add cli option
    )
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
    * Compiles the Flix program and returns a typed ast.
    * If the list of [[CompilationMessage]]s is empty, then the root is always `Some(root)`.
    */
  def check(): (Option[TypedAst.Root], List[CompilationMessage]) = try {
    // Mark this object as implicit.
    implicit val flix: Flix = this

    // Initialize fork-join thread pool.
    initForkJoinPool()

    // Reset the phase information.
    phaseTimers = ListBuffer.empty

    // Reset the phase list file if relevant
    if (this.options.xprintphases) {
      AstPrinter.resetPhaseFile()
    }

    // We mark all inputs that contains compilation errors as dirty.
    // Hence if a file contains an error it will be recompiled -- giving it a chance to disappear.
    for (e <- cachedErrors) {
      val i = e.loc.source.input
      changeSet = changeSet.markChanged(i, cachedTyperAst.dependencyGraph)
    }

    // The default entry point
    val entryPoint = flix.options.entryPoint

    // The global collection of errors
    val errors = mutable.ListBuffer.empty[CompilationMessage]

    val (afterReader, readerErrors) = Reader.run(getInputs, availableClasses)
    errors ++= readerErrors

    val (afterLexer, lexerErrors) = Lexer.run(afterReader, cachedLexerTokens, changeSet)
    errors ++= lexerErrors
    flix.emitEvent(FlixEvent.AfterLexer(afterLexer))

    val (afterParser, parserErrors) = Parser2.run(afterLexer, cachedParserCst, changeSet)
    errors ++= parserErrors

    val (weederValidation, weederErrors) = Weeder2.run(afterReader, entryPoint, afterParser, cachedWeederAst, changeSet)
    errors ++= weederErrors

    val result = weederValidation match {
      case Validation.Failure(failures) =>
        errors ++= failures.toList
        None

      case Validation.Success(afterWeeder) =>
        val afterDesugar = Desugar.run(afterWeeder, cachedDesugarAst, changeSet)

        val (afterNamer, nameErrors) = Namer.run(afterDesugar)
        errors ++= nameErrors

        val (resolverValidation, resolutionErrors) = Resolver.run(afterNamer, cachedResolverAst, changeSet)
        errors ++= resolutionErrors

        resolverValidation match {
          case Validation.Failure(failures) =>
            errors ++= failures.toList
            None

          case Validation.Success(afterResolver) =>

            val (afterKinder, kindErrors) = Kinder.run(afterResolver, cachedKinderAst, changeSet)
            errors ++= kindErrors

            val (afterDeriver, derivationErrors) = Deriver.run(afterKinder)
            errors ++= derivationErrors

            val (afterTyper, typeErrors) = Typer.run(afterDeriver, cachedTyperAst, changeSet)
            errors ++= typeErrors

            val (afterEntryPoint, entryPointErrors) = EntryPoints.run(afterTyper)
            errors ++= entryPointErrors

            val (afterInstances, instanceErrors) = Instances.run(afterEntryPoint, cachedTyperAst, changeSet)
            errors ++= instanceErrors

            val (afterPredDeps, predDepErrors) = PredDeps.run(afterInstances, cachedTyperAst, changeSet)
            errors ++= predDepErrors

            val (afterStratifier, stratificationErrors) = Stratifier.run(afterPredDeps)
            errors ++= stratificationErrors

            val (afterPatMatch, patMatchErrors) = PatMatch.run(afterStratifier, cachedTyperAst, changeSet)
            errors ++= patMatchErrors

            val (afterRedundancy, redundancyErrors) = Redundancy.run(afterPatMatch)
            errors ++= redundancyErrors

            val (_, safetyErrors) = Safety.run(afterRedundancy, cachedTyperAst, changeSet)
            errors ++= safetyErrors

            val (afterDependencies, _) = Dependencies.run(afterRedundancy, cachedTyperAst, changeSet)

            if (options.incremental) {
              this.cachedLexerTokens = afterLexer
              this.cachedParserCst = afterParser
              this.cachedWeederAst = afterWeeder
              this.cachedDesugarAst = afterDesugar
              this.cachedKinderAst = afterKinder
              this.cachedResolverAst = afterResolver
              this.cachedTyperAst = afterDependencies

              // We record that no files are dirty in the change set.
              this.changeSet = ChangeSet.Dirty(Set.empty)

              // We save all the current errors.
              this.cachedErrors = errors.toList
            }

            Some(afterDependencies)
        }
    }
    // Shutdown fork-join thread pool.
    shutdownForkJoinPool()

    // Reset the progress bar.
    progressBar.complete()

    // Print summary?
    if (options.xsummary) {
      result.foreach(root => {
        val table = Summary.fileSummaryTable(root, nsDepth = Some(1), minLines = Some(125))
        table.getMarkdownLines.foreach(println)
      })
    }

    // Return the result (which could contain soft failures).
    (result, errors.toList)
  } catch {
    case ex: InternalCompilerException =>
      CrashHandler.handleCrash(ex)(this)
      throw ex
  }

  /**
    * Compiles the given typed ast to an executable ast.
    *
    * Note: The `codeGen` method has a long execution time, and its local variables
    * are not eligible for garbage collection until the method completes. As a result,
    * large ASTs may be retained in memory longer than necessary. To mitigate this,
    * we explicitly set certain local variables to `null` once they are no longer needed.
    * This manual cleanup has been verified as effective in the profiler.
    */
  def codeGen(typedAst: TypedAst.Root): Validation[CompilationResult, CompilationMessage] = try {
    // Mark this object as implicit.
    implicit val flix: Flix = this

    // Initialize fork-join thread pool.
    initForkJoinPool()

    var treeShaker1Ast = TreeShaker1.run(typedAst)
    // Note: Do not null typedAst. It is used later.

    var monomorpherAst = Specialization.run(typedAst)
    treeShaker1Ast = null // Explicitly null-out such that the memory becomes eligible for GC.

    var lambdaDropAst = LambdaDrop.run(monomorpherAst)
    monomorpherAst = null // Explicitly null-out such that the memory becomes eligible for GC.

    var optimizerAst = Optimizer.run(lambdaDropAst)
    lambdaDropAst = null // Explicitly null-out such that the memory becomes eligible for GC.

    var simplifierAst = Simplifier.run(optimizerAst)
    optimizerAst = null // Explicitly null-out such that the memory becomes eligible for GC.

    var closureConvAst = ClosureConv.run(simplifierAst)
    simplifierAst = null // Explicitly null-out such that the memory becomes eligible for GC.

    var lambdaLiftAst = LambdaLift.run(closureConvAst)
    closureConvAst = null // Explicitly null-out such that the memory becomes eligible for GC.

    var treeShaker2Ast = TreeShaker2.run(lambdaLiftAst)
    lambdaLiftAst = null // Explicitly null-out such that the memory becomes eligible for GC.

    var effectBinderAst = EffectBinder.run(treeShaker2Ast)
    treeShaker2Ast = null // Explicitly null-out such that the memory becomes eligible for GC.

    var tailPosAst = TailPos.run(effectBinderAst)
    effectBinderAst = null // Explicitly null-out such that the memory becomes eligible for GC.

    flix.emitEvent(FlixEvent.AfterTailPos(tailPosAst))

    var eraserAst = Eraser.run(tailPosAst)
    tailPosAst = null // Explicitly null-out such that the memory becomes eligible for GC.

    var reducerAst = Reducer.run(eraserAst)
    eraserAst = null // Explicitly null-out such that the memory becomes eligible for GC.

    // Generate JVM classes.
    val bytecodeAst = JvmBackend.run(reducerAst)
    reducerAst = null // Explicitly null-out such that the memory becomes eligible for GC.

    val totalTime = flix.getTotalTime

    JvmWriter.run(bytecodeAst)
    // (Optionally) load generated JVM classes.
    val loaderResult = JvmLoader.run(bytecodeAst)

    // Construct the compilation result.
    val totalSize = bytecodeAst.classes.values.map(_.bytecode.length).sum
    val result = new CompilationResult(loaderResult.main, loaderResult.tests, loaderResult.sources, totalTime, totalSize)

    // Shutdown fork-join thread pool.
    shutdownForkJoinPool()

    // Reset the progress bar.
    progressBar.complete()

    // Return the result.
    Validation.Success(result)
  } catch {
    case ex: InternalCompilerException =>
      CrashHandler.handleCrash(ex)(this)
      throw ex
    case ex: Throwable =>
      CrashHandler.handleCrash(ex)(this)
      throw ex
  }

  /**
    * Compiles the given typed ast to an executable ast.
    */
  def compile(): Validation[CompilationResult, CompilationMessage] = {
    val (result, errors) = check()
    if (errors.isEmpty) {
      codeGen(result.get)
    } else {
      Validation.Failure(Chain.from(errors))
    }
  }

  /**
    * Clears all caches used for incremental compilation.
    */
  def clearCaches(): Unit = {
    this.cachedLexerTokens = Map.empty
    this.cachedParserCst = SyntaxTree.empty
    this.cachedWeederAst = WeededAst.empty
    this.cachedDesugarAst = DesugaredAst.empty
    this.cachedKinderAst = KindedAst.empty
    this.cachedResolverAst = ResolvedAst.empty
    this.cachedTyperAst = TypedAst.empty
    this.changeSet = ChangeSet.Everything
    this.cachedErrors = Nil
  }

  /**
    * Enters the phase with the given name.
    */
  def phaseNew[A, B](phase: String)(f: => (A, B))(implicit d: Debug[A]): (A, B) = {
    // Initialize the phase time object.
    currentPhase = PhaseTime(phase, 0)

    if (options.progress) {
      progressBar.observe(currentPhase.phase, "")
    }

    // Measure the execution time.
    val t = System.nanoTime()
    val (root, errs) = f
    val e = System.nanoTime() - t

    // Update the phase time.
    currentPhase = currentPhase.copy(time = e)

    // And add it to the list of executed phases.
    phaseTimers += currentPhase

    if (this.options.xprintphases) {
      d.output(phase, root)(this)
    }

    // Return the result computed by the phase.
    (root, errs)
  }

  /**
    * Enters the phase with the given name.
    */
  def phase[A](phase: String)(f: => A)(implicit d: Debug[A]): A = {
    // Initialize the phase time object.
    currentPhase = PhaseTime(phase, 0)

    if (options.progress) {
      progressBar.observe(currentPhase.phase, "")
    }

    // Measure the execution time.
    val t = System.nanoTime()
    val r = f
    val e = System.nanoTime() - t

    // Update the phase time.
    currentPhase = currentPhase.copy(time = e)

    // And add it to the list of executed phases.
    phaseTimers += currentPhase

    if (this.options.xprintphases) {
      d.output(phase, r)(this)
    }

    // Return the result computed by the phase.
    r
  }

  /**
    * Returns the total compilation time in nanoseconds.
    */
  def getTotalTime: Long = phaseTimers.foldLeft(0L) {
    case (acc, phase) => acc + phase.time
  }

  /**
    * Registers the given Flix event listener `l`.
    */
  def addListener(l: FlixListener): Unit = {
    listeners.addOne(l)
  }

  /**
    * Emits the given Flix event to all registered listeners.
    */
  def emitEvent(e: FlixEvent): Unit = {
    listeners.foreach(_.notify(e))
  }

  /**
    * Parses the given `name` into a Path.
    * If `name` is a file:// URI, it is parsed as a URI; otherwise it is parsed directly.
    */
  private def parsePath(name: String): Path = {
    if (name.startsWith("file://")) {
      java.nio.file.Paths.get(new java.net.URI(name))
    } else {
      Path.of(name)
    }
  }

  /**
    * Returns a list of inputs constructed from the strings and paths passed to Flix.
    */
  private def getInputs: List[Input] = {
    val lib = options.lib match {
      case LibLevel.Nix => Nil
      case LibLevel.Min => getLibraryInputs(Library.CoreLibrary)
      case LibLevel.All => getLibraryInputs(Library.CoreLibrary ++ Library.StandardLibrary)
    }
    inputs.values.toList ::: lib
  }

  /**
    * Returns the inputs for the given list of (path, text) pairs.
    */
  private def getLibraryInputs(l: List[(String, String)]): List[Input] = l.foldLeft(List.empty[Input]) {
    case (xs, (virtualPath, text)) => Input.VirtualFile(Path.of(virtualPath), text, SecurityContext.Unrestricted) :: xs
  }

  /**
    * Initializes the fork-join thread pool.
    */
  private def initForkJoinPool(): Unit = {
    threadPool = new ForkJoinPool(options.threads)
  }

  /**
    * Shuts down the fork-join thread pools.
    */
  private def shutdownForkJoinPool(): Unit = {
    threadPool.shutdown()
  }

  /**
    * Extends the set of known Java classes and interfaces with those in the given JAR-file `p`.
    */
  private def extendKnownJavaClassesAndInterfaces(p: Path): Unit = {
    availableClasses = availableClasses ++ getPackageContent(getClassesAndInterfacesOfJar(p))
  }

  /**
    * Returns all Java classes and interfaces in the current Java Platform.
    */
  private def getJavaPlatformClassesAndInterfaces(): MultiMap[List[String], String] = {
    getPackageContent(ClassList.TheList)
  }

  /**
    * Returns the names of all classes and interfaces in the given JAR-file `p`.
    */
  private def getClassesAndInterfacesOfJar(p: Path): List[String] = {
    Using(new ZipFile(p.toFile)) { zip =>
      val result = mutable.ListBuffer.empty[String]
      val iterator = zip.entries()
      while (iterator.hasMoreElements) {
        val entry = iterator.nextElement()
        val name = entry.getName
        if (name.endsWith(".class")) {
          result += name
        }
      }
      result.toList
    }.get
  }

  /**
    * Returns a multimap from Java packages to sub-packages, classes, and interfaces.
    */
  private def getPackageContent(l: List[String]): MultiMap[List[String], String] = {
    l.foldLeft[MultiMap[List[String], String]](MultiMap.empty) {
      case (acc, clazz) =>
        // Given a string `java/util/zip/ZipUtils.class` we convert it to the list `java :: util :: zip :: ZipUtils`.
        // We strip both the ".class" and ".java" suffix. Order should not matter.
        val clazzPath = clazz.stripSuffix(".class").stripSuffix(".java").split('/').toList

        // Create a multimap from all package prefixes to their sub packages and classes.
        // For example, if we have `java.lang.String`, we want to compute:
        // Nil                  => {java}
        // List("java")         => {lang}
        // List("java", "lang") => {String}
        clazzPath.inits.foldLeft(acc) {
          // Case 1: Nonempty path: split prefix and package
          case (acc1, prefix :+ pkg) => acc1 + (prefix -> pkg)
          // Case 2: Empty path: skip it
          case (acc1, _) => acc1
        }
    }
  }

}
