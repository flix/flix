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
import ca.uwaterloo.flix.language.ast.shared.{AvailableClasses, Origin, SecurityContext, Source, SourceName}
import ca.uwaterloo.flix.language.dbg.AstPrinter
import ca.uwaterloo.flix.language.fmt.FormatOptions
import ca.uwaterloo.flix.language.jvm.{ByteBuddyJavaTypeProvider, DependencyClassPath, ExternalJarLoader, JavaTypeProvider}
import ca.uwaterloo.flix.language.phase.*
import ca.uwaterloo.flix.language.phase.jvm.CodeGen
import ca.uwaterloo.flix.language.phase.monomorph.Specialization
import ca.uwaterloo.flix.language.phase.monomorph2.Monomorpher2
import ca.uwaterloo.flix.language.phase.optimizer.{LambdaDrop, Optimizer}
import ca.uwaterloo.flix.language.verifier.TokenVerifier
import ca.uwaterloo.flix.language.{CompilationMessage, GenSym}
import ca.uwaterloo.flix.runtime.CompilationResult
import ca.uwaterloo.flix.tools.compilertop.{CompilerTop, Profiler}
import ca.uwaterloo.flix.util.*
import ca.uwaterloo.flix.util.Formatter.NoFormatter
import ca.uwaterloo.flix.util.tc.Debug

import java.net.URI
import java.nio.charset.Charset
import java.nio.file.{Files, Path}
import java.util.zip.ZipFile
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
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
  *
  * The packages and JARs are immutable: they are registered once at construction and cannot be
  * changed afterwards. If they change, a new Flix compiler instance must be created.
  *
  * @param pkgs   the Flix packages (`.fpkg`) to compile.
  * @param jars   the JAR files whose classes are available to Java interop.
  * @param mounts the mount table of the root project, as its `flix.toml` declares it. Empty when
  *               the project has no manifest.
  */
class Flix(pkgs: List[InstalledPackage] = Nil, jars: List[Path] = Nil, mounts: Map[String, String] = Map.empty) extends AutoCloseable {

  /**
    * The mount table of the root project.
    */
  val rootMounts: Map[String, String] = mounts

  /**
    * The mount table of each package, by package identifier.
    */
  val packageMounts: Map[String, Map[String, String]] = pkgs.map(pkg => pkg.id -> pkg.mounts).toMap

  /**
    * The packages that something in the dependency graph mounts.
    *
    * A mounted package is named under its own root and is reachable only through its mount. A
    * package that nothing mounts keeps sharing the root namespace, as it did before mounts
    * existed. Transitional: every package is mounted once a mount is required.
    */
  val mountedPackages: Set[String] =
    (rootMounts.values ++ packageMounts.values.flatMap(_.values)).toSet

  /**
    * Whether [[close]] has been called. A closed instance cannot compile.
    */
  private var closed: Boolean = false

  /**
    * The registered sources, by name: the files of the packages, registered in the constructor, and
    * the sources added by the caller. The sources of the bundled library are kept separately, see
    * [[librarySources]].
    */
  private val sources = mutable.Map.empty[SourceName, Source]

  /**
    * The sources of the bundled library, by library level. Built once per level, on first use.
    */
  private val librarySources = mutable.Map.empty[LibLevel, List[Source]]

  /**
    * The set of sources changed since last compilation.
    */
  private var changeSet: ChangeSet = ChangeSet.Everything

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
    * Returns the weeded ast.
    */
  def getWeededAst: WeededAst.Root = cachedWeederAst

  /**
    * A cache of error messages for incremental compilation.
    */
  private var cachedErrors: List[CompilationMessage] = Nil

  /**
    * A map to track the time spent in each phase and sub-phase.
    */
  var phaseTimers: ArrayBuffer[PhaseTime] = ArrayBuffer.empty

  /**
    * Optional profiler that records where compile time is spent. Installed
    * by [[setOptions]] when the compiler profiler is enabled; absent on the
    * default path so [[profile]] is a no-op with no measurement overhead.
    */
  private var profiler: Option[Profiler] = None

  /** The live compiler profiler TUI, if it has been started. */
  private var compilerTop: Option[CompilerTop] = None

  /** Returns the currently installed profiler, or `None`. */
  def getProfiler: Option[Profiler] = profiler

  /** Installs (or removes, when `None`) the profiler that backs [[profile]]. */
  def setProfiler(p: Option[Profiler]): Unit = profiler = p

  /**
    * Records the time spent running `thunk` against `sym`, attributed to
    * the currently running phase. When no profiler is installed, `thunk`
    * runs unchanged.
    */
  def profile[A](sym: Symbol.DefnSym, loc: SourceLocation)(thunk: => A): A =
    profiler match {
      case Some(p) => p.track(sym, loc)(thunk)
      case None    => thunk
    }

  /**
    * The current phase we are in. Initially `None`. Volatile so the compiler
    * profiler renderer thread sees each store made by the compile thread in
    * [[phase]].
    */
  @volatile private var currentPhase: Option[PhaseTime] = None

  /**
    * Name of the currently-executing phase, or `None` before the first
    * phase has started or after [[compile]] has reset state.
    */
  def getCurrentPhaseName: Option[String] =
    currentPhase.map(_.phase)

  /**
    * The progress bar.
    */
  private val progressBar: ProgressBar = new ProgressBar(this)

  /**
    * The currently registered event listeners.
    */
  private val listeners: ArrayBuffer[FlixListener] = ArrayBuffer.empty

  /**
    * The default assumed charset.
    */
  val defaultCharset: Charset = Charset.forName("UTF-8")

  // Register the source files of the packages. The packages are read once, here.
  for (pkg <- pkgs) {
    FileOps.isValidFpkgFile(pkg.path) match {
      case Result.Err(e: Throwable) => throw e
      case Result.Ok(()) =>
        for (source <- getSourcesOfPkg(pkg)) {
          sources += source.sourceName -> source
        }
    }
  }

  /**
    * The current Flix options.
    */
  var options: Options = Options.Default

  /**
    * The thread pool for `this` Flix instance.
    */
  var threadPool: ThreadPool = _

  /**
    * The symbol generator associated with this Flix instance.
    */
  val genSym = new GenSym()

  /**
    * The default output formatter.
    */
  private var formatter: Formatter = NoFormatter

  /**
    * The normalized paths of the JARs.
    */
  private val jarPaths: List[Path] = {
    val result = mutable.ArrayBuffer.empty[Path]
    for (p <- jars) {
      FileOps.isValidJarFile(p) match {
        case Result.Err(e: Throwable) => throw e
        case Result.Ok(()) => result += p.normalize()
      }
    }
    result.toList
  }

  /**
    * The set of known Java classes and interfaces: those of the Java platform and those of the JARs.
    */
  val availableClasses: AvailableClasses = {
    if (jarPaths.isEmpty) {
      AvailableClasses.Platform
    } else {
      val jarClasses = jarPaths.flatMap(getClassesAndInterfacesOfJar)
      AvailableClasses.Platform ++ AvailableClasses.fromClassFiles(jarClasses)
    }
  }

  /**
    * A class loader for loading the JARs.
    */
  val jarLoader = new ExternalJarLoader(jarPaths.map(_.toUri.toURL).toArray)

  /**
    * The class files of the JARs.
    *
    * Read directly rather than through [[jarLoader]]: a class loader constructed at run time
    * cannot serve resources inside a GraalVM native image.
    */
  private val dependencyClassPath = new DependencyClassPath(jarPaths)

  /** The descriptor-based Java metadata provider owned by this compiler instance. */
  val javaTypeProvider: JavaTypeProvider = ByteBuddyJavaTypeProvider.fromDependencyClassPath(dependencyClassPath, jarLoader)

  /**
    * Adds the source `text` under the path `p`, replacing any source already registered under it.
    *
    * The path names the source; it need not exist on disk. To add a file from disk, see [[addFile]].
    *
    * @param p    the path that names the source.
    * @param text the Flix source code.
    * @param sctx the security context the source is compiled under.
    */
  def addSource(p: Path, text: String, sctx: SecurityContext): Flix = {
    if (p == null)
      throw new IllegalArgumentException("'p' must be non-null.")
    if (text == null)
      throw new IllegalArgumentException("'text' must be non-null.")
    if (sctx == null)
      throw new IllegalArgumentException("'sctx' must be non-null.")
    register(Source.fromString(SourceName.PathName(p), Origin.User, sctx, text))
    this
  }

  /**
    * Adds the source `text` under the URI `uri`, replacing any source already registered under it.
    *
    * Language servers name sources by the URI the client uses, so that locations sent back to the
    * client refer to the same document.
    *
    * @param uri  the URI that names the source.
    * @param text the Flix source code.
    * @param sctx the security context the source is compiled under.
    */
  def addSource(uri: URI, text: String, sctx: SecurityContext): Flix = {
    if (uri == null)
      throw new IllegalArgumentException("'uri' must be non-null.")
    if (text == null)
      throw new IllegalArgumentException("'text' must be non-null.")
    if (sctx == null)
      throw new IllegalArgumentException("'sctx' must be non-null.")
    register(Source.fromString(SourceName.UriName(uri), Origin.User, sctx, text))
    this
  }

  /**
    * Removes the source named by the path `p`, if any.
    */
  def remSource(p: Path): Flix = {
    if (p == null)
      throw new IllegalArgumentException("'p' must be non-null.")
    unregister(SourceName.PathName(p))
    this
  }

  /**
    * Removes the source named by the URI `uri`, if any.
    */
  def remSource(uri: URI): Flix = {
    if (uri == null)
      throw new IllegalArgumentException("'uri' must be non-null.")
    unregister(SourceName.UriName(uri))
    this
  }

  /**
    * Adds the Flix source file at `p`. The file is read immediately and registered under its
    * normalized path.
    *
    * @param p    the path to the Flix source file. Must be a readable `.flix` file.
    * @param sctx the security context the source is compiled under.
    */
  def addFile(p: Path, sctx: SecurityContext): Flix = {
    isValidFlixFile(p) match {
      case Result.Err(e: Throwable) => throw e
      case Result.Ok(()) =>
        val text = new String(Files.readAllBytes(p), defaultCharset)
        addSource(p.normalize(), text, sctx)
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
    * Removes the Flix source file at `p`, if it was added with [[addFile]].
    *
    * @param p the path to the Flix source file. Must be a `.flix` file.
    */
  def remFile(p: Path): Flix = {
    if (!p.getFileName.toString.endsWith(".flix"))
      throw new IllegalArgumentException(s"'$p' must be a *.flix file.")
    remSource(p.normalize())
  }

  /**
    * Registers `source`, replacing any source already registered under its name.
    *
    * If a source is replaced, its name is marked as changed. Re-registering a source with the same
    * origin, security context, and text changes nothing and marks nothing.
    */
  private def register(source: Source): Unit = sources.get(source.sourceName) match {
    case None =>
      sources += source.sourceName -> source
    case Some(old) if old.origin == source.origin && old.sctx == source.sctx && java.util.Arrays.equals(old.data, source.data) => // nop
    case Some(_) =>
      changeSet = changeSet.markChanged(source.sourceName, cachedTyperAst.dependencyGraph)
      sources += source.sourceName -> source
  }

  /**
    * Unregisters the source with the given `name`, if any.
    *
    * The name is marked as changed, so that everything that depended on the source is recompiled,
    * and the source is forgotten. The caches of the incremental phases drop it at the next
    * compilation, since they keep only entries that are still present.
    */
  private def unregister(name: SourceName): Unit = sources.get(name) match {
    case None => // nop
    case Some(_) =>
      changeSet = changeSet.markChanged(name, cachedTyperAst.dependencyGraph)
      sources -= name
  }

  /**
    * Sets the options used for this Flix instance.
    */
  def setOptions(opts: Options): Flix = {
    if (opts == null)
      throw new IllegalArgumentException("'opts' must be non-null.")
    options = opts
    if (opts.compilerTop && compilerTop.isEmpty) {
      val p = new Profiler(() => getCurrentPhaseName)
      setProfiler(Some(p))
      // The profiler subscribes to compiler events (e.g. NewConstraintsDef)
      // to track signals that are awkward to thread through `track()`.
      addListener(p)
      val t = new CompilerTop(this, p)
      t.start()
      compilerTop = Some(t)
    }
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
    if (closed)
      throw new IllegalStateException("The Flix instance has been closed.")

    // Mark this object as implicit.
    implicit val flix: Flix = this

    // Begin drawing the progress bar (if enabled).
    progressBar.start()

    // Initialize the thread pool.
    initThreadPool()

    // Reset the phase information.
    phaseTimers = ArrayBuffer.empty
    currentPhase = None

    // Reset the phase list file if relevant
    if (this.options.xprintphases) {
      AstPrinter.resetPhaseFile()
    }

    // We mark all sources that contain compilation errors as dirty.
    // Hence if a file contains an error it will be recompiled -- giving it a chance to disappear.
    for (e <- cachedErrors) {
      changeSet = changeSet.markChanged(e.loc.source.sourceName, cachedTyperAst.dependencyGraph)
    }

    // The default entry point
    val entryPoint = flix.options.entryPoint

    // The global collection of errors
    val errors = mutable.ArrayBuffer.empty[CompilationMessage]

    val readRoot = ReadAst.Root(getSources.map(src => src -> ()).toMap)

    val (afterLexer, lexerErrors) = Lexer.run(readRoot, cachedLexerTokens, changeSet)
    errors ++= lexerErrors
    if (flix.options.xverify) {
      TokenVerifier.verify(afterLexer)
    }

    val (afterParser, parserErrors) = Parser2.run(afterLexer, cachedParserCst, changeSet)
    errors ++= parserErrors

    val (weederResult, weederErrors) = Weeder2.run(readRoot, entryPoint, afterParser, cachedWeederAst, changeSet)
    errors ++= weederErrors

    val result = weederResult match {
      case None => None

      case Some(afterWeeder) =>
        val afterDesugar = Desugar.run(afterWeeder, cachedDesugarAst, changeSet)

        val (afterNamer, nameErrors) = Namer.run(afterDesugar)
        errors ++= nameErrors

        val (afterResolver, resolutionErrors) = Resolver.run(afterNamer, cachedResolverAst, changeSet)
        errors ++= resolutionErrors

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

        val (afterPatMatch, patMatchErrors) = PatMatch2.run(afterStratifier, cachedTyperAst, changeSet)
        errors ++= patMatchErrors

        val (afterRedundancy, redundancyErrors) = Redundancy.run(afterPatMatch)
        errors ++= redundancyErrors

        val (_, safetyErrors) = Safety.run(afterRedundancy, cachedTyperAst, changeSet)
        errors ++= safetyErrors

        val (afterTerminator, terminationErrors) = Terminator.run(afterRedundancy, cachedTyperAst, changeSet)
        errors ++= terminationErrors

        val (afterDependencies, _) = Dependencies.run(afterTerminator, cachedTyperAst, changeSet)

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

    // Shutdown the thread pool.
    shutdownThreadPool()

    // Reset the progress bar.
    progressBar.complete()

    // Stop the live compiler profiler TUI only if there are errors and no
    // `codeGen` will follow. On the success path, leave it running so
    // `codeGen` can continue updating it through the mid-end and backend phases.
    if (errors.nonEmpty) {
      compilerTop.foreach(_.stop())
    }

    // Return the result (which could contain soft failures).
    (result, errors.toList)
  } catch {
    case ex: InternalCompilerException =>
      progressBar.complete()
      CrashHandler.handleCrash(ex)(this)
      throw ex
    case ex: Throwable =>
      progressBar.complete()
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
  def codeGen(typedAst: TypedAst.Root): CompilationResult = try {
    if (closed)
      throw new IllegalStateException("The Flix instance has been closed.")

    // Mark this object as implicit.
    implicit val flix: Flix = this

    // Begin drawing the progress bar (if enabled).
    progressBar.start()

    // Initialize the thread pool.
    initThreadPool()

    var treeShaker1Ast = TreeShaker1.run(typedAst)
    // Note: Do not null typedAst. It is used later.

    var monomorpherAst =
      if (options.xnewmono) Monomorpher2.run(treeShaker1Ast)
      else Specialization.run(treeShaker1Ast)
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

    var eraserAst = Eraser.run(tailPosAst)
    tailPosAst = null // Explicitly null-out such that the memory becomes eligible for GC.

    var reducerAst = Reducer.run(eraserAst)
    eraserAst = null // Explicitly null-out such that the memory becomes eligible for GC.

    // Generate JVM classes.
    val bytecodeAst = CodeGen.run(reducerAst)
    reducerAst = null // Explicitly null-out such that the memory becomes eligible for GC.

    val totalTime = flix.getTotalTime

    // Construct the compilation result. The generated classes are not loaded into the JVM;
    // that is the caller's responsibility (see [[ca.uwaterloo.flix.runtime.JvmLoader]]).
    val totalSize = bytecodeAst.classes.values.map(_.bytecode.length).sum
    val result = new CompilationResult(bytecodeAst, totalTime, totalSize, this)

    // Shutdown the thread pool.
    shutdownThreadPool()

    // Reset the progress bar.
    progressBar.complete()

    // Stop the live compiler profiler TUI, if it is running.
    compilerTop.foreach(_.stop())

    // Return the result.
    result
  } catch {
    case ex: InternalCompilerException =>
      progressBar.complete()
      CrashHandler.handleCrash(ex)(this)
      throw ex
    case ex: Throwable =>
      progressBar.complete()
      CrashHandler.handleCrash(ex)(this)
      throw ex
  }

  /**
    * Compiles the given typed ast to an executable ast.
    */
  def compile(): Result[CompilationResult, List[CompilationMessage]] = {
    val (result, errors) = check()
    if (errors.isEmpty) {
      Result.Ok(codeGen(result.get))
    } else {
      Result.Err(errors)
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
    * Releases the resources held by this instance: the open JAR files of the dependency class path
    * and the class loader for external JARs.
    *
    * Classes already loaded through [[jarLoader]] remain usable, but no further classes can be loaded
    * from the JARs. The instance must not be used for compilation after it has been closed.
    */
  override def close(): Unit = {
    closed = true
    javaTypeProvider.close()
    jarLoader.close()
  }

  /**
    * Enters the phase with the given name.
    *
    * Runs `f`, records its execution time, and, if `--Xprint-phases` is enabled,
    * hands the result to `d`. Phases returning a `(root, errors)` pair get their
    * [[Debug]] instance from [[Debug.debugPair]], which debugs only the root.
    */
  def phase[A](phase: String)(f: => A)(implicit d: Debug[A]): A = {
    // Initialize the phase time object.
    currentPhase = Some(PhaseTime(phase, 0))

    progressBar.observe(phase)

    // Measure the execution time.
    val t = System.nanoTime()
    val r = f
    val e = System.nanoTime() - t

    // Update the phase time and add it to the list of executed phases.
    val finished = PhaseTime(phase, e)
    currentPhase = Some(finished)
    phaseTimers += finished

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
    * Returns the sources to compile: the registered sources followed by the sources of the bundled
    * library selected by `options.lib`.
    */
  private def getSources: List[Source] = sources.values.toList ::: getLibrarySources(options.lib)

  /**
    * Returns the sources of the bundled library at the given `level`, building them on first use.
    */
  private def getLibrarySources(level: LibLevel): List[Source] = librarySources.getOrElseUpdate(level, level match {
    case LibLevel.Nix => Nil
    case LibLevel.Min => mkLibrarySources(Library.CoreLibrary)
    case LibLevel.All => mkLibrarySources(Library.CoreLibrary ++ Library.StandardLibrary)
  })

  /**
    * Returns the library sources for the given list of (virtual path, text) pairs.
    */
  private def mkLibrarySources(l: List[(String, String)]): List[Source] = l.foldLeft(List.empty[Source]) {
    case (xs, (virtualPath, text)) =>
      Source.fromString(SourceName.PathName(Path.of(virtualPath)), Origin.Library, SecurityContext.Unrestricted, text) :: xs
  }

  /**
    * Initializes the thread pool.
    */
  private def initThreadPool(): Unit = {
    threadPool = new ThreadPool(options.threads)
  }

  /**
    * Shuts down the thread pool.
    */
  private def shutdownThreadPool(): Unit = {
    threadPool.shutdown()
  }

  /**
    * Returns the `.flix` source files inside `pkg`, each stamped with the identifier and the
    * security context of the package.
    */
  private def getSourcesOfPkg(pkg: InstalledPackage): List[Source] = {
    val p = pkg.path
    Using(new ZipFile(p.toFile)) { zip =>
      val result = mutable.ArrayBuffer.empty[Source]
      val iterator = zip.entries()
      while (iterator.hasMoreElements) {
        val entry = iterator.nextElement()
        val name = entry.getName
        if (name.endsWith(".flix")) {
          val bytes = StreamOps.readAllBytes(zip.getInputStream(entry))
          val text = new String(bytes, defaultCharset)
          result += Source.fromString(SourceName.PackageEntry(p, name), Origin.Package(pkg.id), pkg.sctx, text)
        }
      }
      result.toList
    }.get
  }

  /**
    * Returns the names of all classes and interfaces in the given JAR-file `p`.
    */
  private def getClassesAndInterfacesOfJar(p: Path): List[String] = {
    Using(new ZipFile(p.toFile)) { zip =>
      val result = mutable.ArrayBuffer.empty[String]
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

}
