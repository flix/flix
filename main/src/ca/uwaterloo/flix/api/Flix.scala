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
import ca.uwaterloo.flix.language.phase.optimizer.{LambdaDrop, Optimizer}
import ca.uwaterloo.flix.language.{CompilationMessage, GenSym}
import ca.uwaterloo.flix.runtime.CompilationResult
import ca.uwaterloo.flix.tools.Summary
import ca.uwaterloo.flix.util.Formatter.NoFormatter
import ca.uwaterloo.flix.util.*
import ca.uwaterloo.flix.util.collection.{Chain, MapOps, MultiMap}
import ca.uwaterloo.flix.util.tc.Debug

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
    * A cache of error messages for incremental compilation.
    */
  private var cachedErrors: List[CompilationMessage] = Nil

  /**
    * A sequence of internal inputs to be parsed into Flix ASTs.
    *
    * The core library *must* be present for any program to compile.
    */
  private val coreLibrary = List(
    // Prelude
    "Prelude.flix" -> "/src/library/Prelude.flix",

    // Comparison
    "Comparison.flix" -> "/src/library/Comparison.flix",

    // Coerce
    "Coerce.flix" -> "/src/library/Coerce.flix",

    // Operators
    "Neg.flix" -> "/src/library/Neg.flix",
    "Add.flix" -> "/src/library/Add.flix",
    "Sub.flix" -> "/src/library/Sub.flix",
    "Mul.flix" -> "/src/library/Mul.flix",
    "Div.flix" -> "/src/library/Div.flix",
    "Bool.flix" -> "/src/library/Bool.flix",

    // Threads
    "Thread.flix" -> "/src/library/Thread.flix",
    "Time.flix" -> "/src/library/Time.flix",

    // Built-in
    "Eq.flix" -> "/src/library/Eq.flix",
    "Hash.flix" -> "/src/library/Hash.flix",
    "Order.flix" -> "/src/library/Order.flix",

    // Lattices
    "PartialOrder.flix" -> "/src/library/PartialOrder.flix",
    "LowerBound.flix" -> "/src/library/LowerBound.flix",
    "UpperBound.flix" -> "/src/library/UpperBound.flix",
    "JoinLattice.flix" -> "/src/library/JoinLattice.flix",
    "MeetLattice.flix" -> "/src/library/MeetLattice.flix",

    // String
    "ToString.flix" -> "/src/library/ToString.flix",

    // Reflect
    "Reflect.flix" -> "/src/library/Reflect.flix",

    // Debug
    "Debug.flix" -> "/src/library/Debug.flix",

    // References
    "Ref.flix" -> "/src/library/Ref.flix",
  )

  /**
    * A sequence of internal inputs to be parsed into Flix ASTs.
    *
    * The standard library is not required to be present for at least some programs to compile.
    */
  private val standardLibrary = List(
    "Array.flix" -> "/src/library/Array.flix",
    "Assert.flix" -> "/src/library/Assert.flix",
    "BigDecimal.flix" -> "/src/library/BigDecimal.flix",
    "BigInt.flix" -> "/src/library/BigInt.flix",
    "Box.flix" -> "/src/library/Box.flix",
    "BPlusTree.flix" -> "/src/library/BPlusTree.flix",
    "Chain.flix" -> "/src/library/Chain.flix",
    "Char.flix" -> "/src/library/Char.flix",
    "CodePoint.flix" -> "/src/library/CodePoint.flix",
    "Console.flix" -> "/src/library/Console.flix",
    "DelayList.flix" -> "/src/library/DelayList.flix",
    "DelayMap.flix" -> "/src/library/DelayMap.flix",
    "Down.flix" -> "/src/library/Down.flix",
    "Float32.flix" -> "/src/library/Float32.flix",
    "Float64.flix" -> "/src/library/Float64.flix",
    "Int8.flix" -> "/src/library/Int8.flix",
    "Int16.flix" -> "/src/library/Int16.flix",
    "Int32.flix" -> "/src/library/Int32.flix",
    "Int64.flix" -> "/src/library/Int64.flix",
    "Iterable.flix" -> "/src/library/Iterable.flix",
    "Iterator.flix" -> "/src/library/Iterator.flix",
    "KeyNotFound.flix" -> "/src/library/KeyNotFound.flix",
    "List.flix" -> "/src/library/List.flix",
    "Map.flix" -> "/src/library/Map.flix",
    "Nec.flix" -> "/src/library/Nec.flix",
    "Nel.flix" -> "/src/library/Nel.flix",
    "Object.flix" -> "/src/library/Object.flix",
    "Option.flix" -> "/src/library/Option.flix",
    "OutOfBounds.flix" -> "/src/library/OutOfBounds.flix",
    "Random.flix" -> "/src/library/Random.flix",
    "Result.flix" -> "/src/library/Result.flix",
    "Set.flix" -> "/src/library/Set.flix",
    "String.flix" -> "/src/library/String.flix",
    "MultiMap.flix" -> "/src/library/MultiMap.flix",

    "MutPriorityQueue.flix" -> "/src/library/MutPriorityQueue.flix",
    "MutDeque.flix" -> "/src/library/MutDeque.flix",
    "MutDisjointSets.flix" -> "/src/library/MutDisjointSets.flix",
    "MutList.flix" -> "/src/library/MutList.flix",
    "MutSet.flix" -> "/src/library/MutSet.flix",
    "MutMap.flix" -> "/src/library/MutMap.flix",

    "IoError.flix" -> "/src/library/IoError.flix",
    "Reader.flix" -> "/src/library/Reader.flix",
    "Writer.flix" -> "/src/library/Writer.flix",

    "Environment.flix" -> "/src/library/Environment.flix",

    "Applicative.flix" -> "/src/library/Applicative.flix",
    "CommutativeGroup.flix" -> "/src/library/CommutativeGroup.flix",
    "CommutativeMonoid.flix" -> "/src/library/CommutativeMonoid.flix",
    "CommutativeSemiGroup.flix" -> "/src/library/CommutativeSemiGroup.flix",
    "Foldable.flix" -> "/src/library/Foldable.flix",
    "FromString.flix" -> "/src/library/FromString.flix",
    "Functor.flix" -> "/src/library/Functor.flix",
    "Filterable.flix" -> "/src/library/Filterable.flix",
    "Group.flix" -> "/src/library/Group.flix",
    "Identity.flix" -> "/src/library/Identity.flix",
    "Indexable.flix" -> "/src/library/Indexable.flix",
    "IndexableMut.flix" -> "/src/library/IndexableMut.flix",
    "Monad.flix" -> "/src/library/Monad.flix",
    "MonadZero.flix" -> "/src/library/MonadZero.flix",
    "MonadZip.flix" -> "/src/library/MonadZip.flix",
    "Monoid.flix" -> "/src/library/Monoid.flix",
    "Reducible.flix" -> "/src/library/Reducible.flix",
    "SemiGroup.flix" -> "/src/library/SemiGroup.flix",
    "Traversable.flix" -> "/src/library/Traversable.flix",
    "Witherable.flix" -> "/src/library/Witherable.flix",
    "UnorderedFoldable.flix" -> "/src/library/UnorderedFoldable.flix",
    "Collectable.flix" -> "/src/library/Collectable.flix",
    "MutCollectable.flix" -> "/src/library/MutCollectable.flix",

    "Validation.flix" -> "/src/library/Validation.flix",

    "StringBuilder.flix" -> "/src/library/StringBuilder.flix",
    "RedBlackTree.flix" -> "/src/library/RedBlackTree.flix",
    "GetOpt.flix" -> "/src/library/GetOpt.flix",
    "Chalk.flix" -> "/src/library/Chalk.flix",

    "Channel.flix" -> "/src/library/Channel.flix",
    "Concurrent/Channel.flix" -> "/src/library/Concurrent/Channel.flix",
    "Concurrent/Condition.flix" -> "/src/library/Concurrent/Condition.flix",
    "Concurrent/CyclicBarrier.flix" -> "/src/library/Concurrent/CyclicBarrier.flix",
    "Concurrent/ReentrantLock.flix" -> "/src/library/Concurrent/ReentrantLock.flix",

    "Time/Duration.flix" -> "/src/library/Time/Duration.flix",
    "Time/Epoch.flix" -> "/src/library/Time/Epoch.flix",
    "Time/Instant.flix" -> "/src/library/Time/Instant.flix",

    "Fixpoint/Phase/Stratifier.flix" -> "/src/library/Fixpoint/Phase/Stratifier.flix",
    "Fixpoint/Phase/Compiler.flix" -> "/src/library/Fixpoint/Phase/Compiler.flix",
    "Fixpoint/Phase/Simplifier.flix" -> "/src/library/Fixpoint/Phase/Simplifier.flix",
    "Fixpoint/Phase/IndexSelection.flix" -> "/src/library/Fixpoint/Phase/IndexSelection.flix",
    "Fixpoint/Phase/VarsToIndices.flix" -> "/src/library/Fixpoint/Phase/VarsToIndices.flix",
    "Fixpoint/Boxable.flix" -> "/src/library/Fixpoint/Boxable.flix",
    "Fixpoint/Boxed.flix" -> "/src/library/Fixpoint/Boxed.flix",
    "Fixpoint/Debugging.flix" -> "/src/library/Fixpoint/Debugging.flix",
    "Fixpoint/Interpreter.flix" -> "/src/library/Fixpoint/Interpreter.flix",
    "Fixpoint/Options.flix" -> "/src/library/Fixpoint/Options.flix",
    "Fixpoint/PredSymsOf.flix" -> "/src/library/Fixpoint/PredSymsOf.flix",
    "Fixpoint/Solver.flix" -> "/src/library/Fixpoint/Solver.flix",
    "Fixpoint/SubstitutePredSym.flix" -> "/src/library/Fixpoint/SubstitutePredSym.flix",

    "Fixpoint/Ast/Datalog.flix" -> "/src/library/Fixpoint/Ast/Datalog.flix",
    "Fixpoint/Ast/Shared.flix" -> "/src/library/Fixpoint/Ast/Shared.flix",
    "Fixpoint/Ast/PrecedenceGraph.flix" -> "/src/library/Fixpoint/Ast/PrecedenceGraph.flix",
    "Fixpoint/Ast/Ram.flix" -> "/src/library/Fixpoint/Ast/Ram.flix",


    "Fixpoint/Toggle.flix" -> "/src/library/Fixpoint/Toggle.flix",
    "Fixpoint/SolverApi.flix" -> "/src/library/Fixpoint/SolverApi.flix",


    "Fixpoint3/AtomicCounter.flix" -> "/src/library/Fixpoint3/AtomicCounter.flix",
    "Fixpoint3/Boxed.flix" -> "/src/library/Fixpoint3/Boxed.flix",
    "Fixpoint3/BoxingType.flix" -> "/src/library/Fixpoint3/BoxingType.flix",
    "Fixpoint3/Counter.flix" -> "/src/library/Fixpoint3/Counter.flix",
    "Fixpoint3/Util.flix" -> "/src/library/Fixpoint3/Util.flix",
    "Fixpoint3/Predicates.flix" -> "/src/library/Fixpoint3/Predicates.flix",
    "Fixpoint3/ReadWriteLock.flix" -> "/src/library/Fixpoint3/ReadWriteLock.flix",
    "Fixpoint3/Solver.flix" -> "/src/library/Fixpoint3/Solver.flix",
    "Fixpoint3/UniqueInts.flix" -> "/src/library/Fixpoint3/UniqueInts.flix",

    "Fixpoint3/Ast/Ram.flix" -> "/src/library/Fixpoint3/Ast/Ram.flix",
    "Fixpoint3/Ast/ExecutableRam.flix" -> "/src/library/Fixpoint3/Ast/ExecutableRam.flix",

    "Fixpoint3/Phase/RenamePredSyms.flix" -> "/src/library/Fixpoint3/Phase/RenamePredSyms.flix",

    "Abort.flix" -> "/src/library/Abort.flix",
    "Clock.flix" -> "/src/library/Clock.flix",
    "Dns.flix" -> "/src/library/Dns.flix",
    "DnsWithResult.flix" -> "/src/library/DnsWithResult.flix",
    "Http.flix" -> "/src/library/Http.flix",
    "HttpWithResult.flix" -> "/src/library/HttpWithResult.flix",
    "Exit.flix" -> "/src/library/Exit.flix",
    "Eff/BiasedCoin.flix" -> "/src/library/Eff/BiasedCoin.flix",
    "Eff/RandomCoin.flix" -> "/src/library/Eff/RandomCoin.flix",
    "Logger.flix" -> "/src/library/Logger.flix",
    "FileRead.flix" -> "/src/library/FileRead.flix",
    "FileReadWithResult.flix" -> "/src/library/FileReadWithResult.flix",
    "FileWrite.flix" -> "/src/library/FileWrite.flix",
    "FileWriteWithResult.flix" -> "/src/library/FileWriteWithResult.flix",
    "IpAddr.flix" -> "/src/library/IpAddr.flix",
    "Ipv4Addr.flix" -> "/src/library/Ipv4Addr.flix",
    "Ipv6Addr.flix" -> "/src/library/Ipv6Addr.flix",
    "Ping.flix" -> "/src/library/Ping.flix",
    "PingWithResult.flix" -> "/src/library/PingWithResult.flix",
    "ProcessHandle.flix" -> "/src/library/ProcessHandle.flix",
    "Process.flix" -> "/src/library/Process.flix",
    "ProcessWithResult.flix" -> "/src/library/ProcessWithResult.flix",
    "Severity.flix" -> "/src/library/Severity.flix",
    "TcpBind.flix" -> "/src/library/TcpBind.flix",
    "TcpAccept.flix" -> "/src/library/TcpAccept.flix",
    "TcpAcceptWithResult.flix" -> "/src/library/TcpAcceptWithResult.flix",
    "TcpServer.flix" -> "/src/library/TcpServer.flix",
    "TcpSocket.flix" -> "/src/library/TcpSocket.flix",
    "TcpBindWithResult.flix" -> "/src/library/TcpBindWithResult.flix",
    "TimeUnit.flix" -> "/src/library/TimeUnit.flix",

    "Graph.flix" -> "/src/library/Graph.flix",
    "Vector.flix" -> "/src/library/Vector.flix",
    "Regex.flix" -> "/src/library/Regex.flix",
    "Adaptor.flix" -> "/src/library/Adaptor.flix",
    "ToJava.flix" -> "/src/library/ToJava.flix",
    "ToFlix.flix" -> "/src/library/ToFlix.flix",
  )

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
    * Adds the given string `text` with the given `name`.
    */
  def addSourceCode(name: String, text: String)(implicit sctx: SecurityContext): Flix = {
    if (name == null)
      throw new IllegalArgumentException("'name' must be non-null.")
    if (text == null)
      throw new IllegalArgumentException("'text' must be non-null.")
    if (sctx == null)
      throw new IllegalArgumentException("'sctx' must be non-null.")
    addInput(name, Input.Text(name, text, sctx))
    this
  }

  /**
    * Removes the source code with the given `name`.
    */
  def remSourceCode(name: String): Flix = {
    if (name == null)
      throw new IllegalArgumentException("'name' must be non-null.")
    remInput(name, Input.Text(name, "", /* unused */ SecurityContext.NoPermissions))
    this
  }

  /**
    * Adds the given path `p` as Flix source file.
    */
  def addFlix(p: Path)(implicit sctx: SecurityContext): Flix = {
    if (p == null)
      throw new IllegalArgumentException(s"'p' must be non-null.")
    if (!Files.exists(p))
      throw new IllegalArgumentException(s"'$p' must be a file.")
    if (!Files.isRegularFile(p))
      throw new IllegalArgumentException(s"'$p' must be a regular file.")
    if (!Files.isReadable(p))
      throw new IllegalArgumentException(s"'$p' must be a readable file.")
    if (!p.getFileName.toString.endsWith(".flix"))
      throw new IllegalArgumentException(s"'$p' must be a *.flix file.")

    addInput(p.toString, Input.TxtFile(p, sctx))
    this
  }

  /**
    * Adds the given path `p` as a Flix package file.
    */
  def addPkg(p: Path)(implicit sctx: SecurityContext): Flix = {
    if (p == null)
      throw new IllegalArgumentException(s"'p' must be non-null.")
    if (!Files.exists(p))
      throw new IllegalArgumentException(s"'$p' must be a file.")
    if (!Files.isRegularFile(p))
      throw new IllegalArgumentException(s"'$p' must be a regular file.")
    if (!Files.isReadable(p))
      throw new IllegalArgumentException(s"'$p' must be a readable file.")
    if (!p.getFileName.toString.endsWith(".fpkg"))
      throw new IllegalArgumentException(s"'$p' must be a *.pkg file.")

    addInput(p.toString, Input.PkgFile(p, sctx))
    this
  }

  /**
    * Removes the given path `p` as a Flix source file.
    */
  def remFlix(p: Path)(implicit sctx: SecurityContext): Flix = {
    if (!p.getFileName.toString.endsWith(".flix"))
      throw new IllegalArgumentException(s"'$p' must be a *.flix file.")

    remInput(p.toString, Input.TxtFile(p, sctx))
    this
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
    extendKnownJavaClassesAndInterfaces(p)
    this
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
      inputs += name -> Input.Text(name, "", /* unused */ SecurityContext.NoPermissions)
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
    * Converts a list of compiler error messages to a list of printable messages.
    * Decides whether or not to append the explanation.
    */
  def mkMessages(errors: List[CompilationMessage]): List[String] = {
    if (options.explain)
      errors.sortBy(_.loc).map(cm => cm.messageWithLoc(formatter) + cm.explain(formatter).getOrElse(""))
    else
      errors.sortBy(_.loc).map(cm => cm.messageWithLoc(formatter))
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
      val i = e.loc.sp1.source.input
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
    */
  def codeGen(typedAst: TypedAst.Root): Validation[CompilationResult, CompilationMessage] = try {
    // Mark this object as implicit.
    implicit val flix: Flix = this

    // Initialize fork-join thread pool.
    initForkJoinPool()

    val loweringAst = Lowering.run(typedAst)
    val treeShaker1Ast = TreeShaker1.run(loweringAst)
    val monomorpherAst = Monomorpher.run(treeShaker1Ast)
    val lambdaDropAst = LambdaDrop.run(monomorpherAst)
    val optimizerAst = Optimizer.run(lambdaDropAst)
    val simplifierAst = Simplifier.run(optimizerAst)
    val closureConvAst = ClosureConv.run(simplifierAst)
    val lambdaLiftAst = LambdaLift.run(closureConvAst)
    val treeShaker2Ast = TreeShaker2.run(lambdaLiftAst)
    val effectBinderAst = EffectBinder.run(treeShaker2Ast)

    val tailPosAst = TailPos.run(effectBinderAst)
    flix.emitEvent(FlixEvent.AfterTailPos(tailPosAst))

    val eraserAst = Eraser.run(tailPosAst)
    val reducerAst = Reducer.run(eraserAst)
    val varOffsetsAst = VarOffsets.run(reducerAst)
    val (backendAst, classes) = JvmBackend.run(varOffsetsAst)
    val totalTime = flix.getTotalTime
    JvmWriter.run(classes)
    val (loadedAst, loadRes) = JvmLoader.run(backendAst, classes)
    val result = new CompilationResult(loadedAst, loadRes.main, loadRes.defs, totalTime, loadRes.byteSize)

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
    * Enters the phase with the given name.
    */
  def phaseNew[A, B](phase: String)(f: => (A, B))(implicit d: Debug[A]): (A, B) = {
    // Initialize the phase time object.
    currentPhase = PhaseTime(phase, 0)

    if (options.progress) {
      progressBar.observe(currentPhase.phase, "", sample = false)
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
    * A callback to indicate that work has started on the given subtask.
    */
  def subtask(subtask: String, sample: Boolean = false): Unit = {
    if (options.progress) {
      progressBar.observe(currentPhase.phase, subtask, sample)
    }
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
    * Returns a list of inputs constructed from the strings and paths passed to Flix.
    */
  private def getInputs: List[Input] = {
    val lib = options.lib match {
      case LibLevel.Nix => Nil
      case LibLevel.Min => getLibraryInputs(coreLibrary.map{ case (mod, path) => (mod, LocalResource.get(path))})
      case LibLevel.All => getLibraryInputs(coreLibrary.map{ case (mod, path) => (mod, LocalResource.get(path))} ++ standardLibrary.map{ case (mod, path) => (mod, LocalResource.get(path))})
    }
    inputs.values.toList ::: lib
  }

  /**
    * Returns the inputs for the given list of (path, text) pairs.
    */
  private def getLibraryInputs(l: List[(String, String)]): List[Input] = l.foldLeft(List.empty[Input]) {
    case (xs, (virtualPath, text)) => Input.Text(virtualPath, text, SecurityContext.AllPermissions) :: xs
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
