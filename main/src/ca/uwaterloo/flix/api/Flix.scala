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
import ca.uwaterloo.flix.language.dbg.AstPrinter
import ca.uwaterloo.flix.language.fmt.FormatOptions
import ca.uwaterloo.flix.language.phase._
import ca.uwaterloo.flix.language.phase.jvm.JvmBackend
import ca.uwaterloo.flix.language.{CompilationMessage, GenSym}
import ca.uwaterloo.flix.runtime.CompilationResult
import ca.uwaterloo.flix.tools.Summary
import ca.uwaterloo.flix.util.Formatter.NoFormatter
import ca.uwaterloo.flix.util._
import ca.uwaterloo.flix.util.collection.Chain

import java.nio.charset.Charset
import java.nio.file.{Files, Path}
import java.util.concurrent.ForkJoinPool
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions

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
    * A cache of ASTs for incremental compilation.
    */
  private var cachedLexerTokens: Map[Ast.Source, Array[Token]] = Map.empty
  private var cachedParserAst:  ParsedAst.Root = ParsedAst.empty
  private var cachedParserCst:  SyntaxTree.Root = SyntaxTree.empty
  private var cachedWeederAst: WeededAst.Root = WeededAst.empty
  private var cachedWeederAst2: WeededAst.Root = WeededAst.empty
  private var cachedDesugarAst: DesugaredAst.Root = DesugaredAst.empty
  private var cachedKinderAst: KindedAst.Root = KindedAst.empty
  private var cachedResolverAst: ResolvedAst.Root = ResolvedAst.empty
  private var cachedTyperAst: TypedAst.Root = TypedAst.empty

  def getParserAst: ParsedAst.Root = cachedParserAst

  def getParserCst: SyntaxTree.Root  = cachedParserCst

  def getWeederAst: WeededAst.Root = cachedWeederAst

  def getWeederAst2: WeededAst.Root = cachedWeederAst2

  def getDesugarAst: DesugaredAst.Root = cachedDesugarAst

  def getKinderAst: KindedAst.Root = cachedKinderAst

  def getResolverAst: ResolvedAst.Root = cachedResolverAst

  def getTyperAst: TypedAst.Root = cachedTyperAst

  /**
    * A cache of ASTs for debugging.
    */
  private var cachedLoweringAst: LoweredAst.Root = LoweredAst.empty
  private var cachedTreeShaker1Ast: LoweredAst.Root = LoweredAst.empty
  private var cachedMonomorpherAst: MonoAst.Root = MonoAst.empty
  private var cachedMonoTypesAst: MonoAst.Root = MonoAst.empty
  private var cachedSimplifierAst: SimplifiedAst.Root = SimplifiedAst.empty
  private var cachedClosureConvAst: SimplifiedAst.Root = SimplifiedAst.empty
  private var cachedLambdaLiftAst: LiftedAst.Root = LiftedAst.empty
  private var cachedOptimizerAst: LiftedAst.Root = LiftedAst.empty
  private var cachedTreeShaker2Ast: LiftedAst.Root = LiftedAst.empty
  private var cachedEffectBinderAst: ReducedAst.Root = ReducedAst.empty
  private var cachedTailPosAst: ReducedAst.Root = ReducedAst.empty
  private var cachedEraserAst: ReducedAst.Root = ReducedAst.empty
  private var cachedReducerAst: ReducedAst.Root = ReducedAst.empty
  private var cachedVarOffsetsAst: ReducedAst.Root = ReducedAst.empty

  def getLoweringAst: LoweredAst.Root = cachedLoweringAst

  def getTreeShaker1Ast: LoweredAst.Root = cachedTreeShaker1Ast

  def getMonomorpherAst: MonoAst.Root = cachedMonomorpherAst

  def getMonoTypesAst: MonoAst.Root = cachedMonoTypesAst

  def getSimplifierAst: SimplifiedAst.Root = cachedSimplifierAst

  def getClosureConvAst: SimplifiedAst.Root = cachedClosureConvAst

  def getLambdaLiftAst: LiftedAst.Root = cachedLambdaLiftAst

  def getOptimizerAst: LiftedAst.Root = cachedOptimizerAst

  def getTreeShaker2Ast: LiftedAst.Root = cachedTreeShaker2Ast

  def getEffectBinderAst: ReducedAst.Root = cachedEffectBinderAst

  def getTailPosAst: ReducedAst.Root = cachedTailPosAst

  def getEraserAst: ReducedAst.Root = cachedEraserAst

  def getReducerAst: ReducedAst.Root = cachedReducerAst

  def getVarOffsetsAst: ReducedAst.Root = cachedVarOffsetsAst

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

    // Coerce
    "Coerce.flix" -> LocalResource.get("/src/library/Coerce.flix"),

    // Operators
    "Neg.flix" -> LocalResource.get("/src/library/Neg.flix"),
    "Add.flix" -> LocalResource.get("/src/library/Add.flix"),
    "Sub.flix" -> LocalResource.get("/src/library/Sub.flix"),
    "Mul.flix" -> LocalResource.get("/src/library/Mul.flix"),
    "Div.flix" -> LocalResource.get("/src/library/Div.flix"),
    "Bool.flix" -> LocalResource.get("/src/library/Bool.flix"),

    // Channels and Threads
    "Channel.flix" -> LocalResource.get("/src/library/Channel.flix"),
    "Thread.flix" -> LocalResource.get("/src/library/Thread.flix"),
    "Time.flix" -> LocalResource.get("/src/library/Time.flix"),

    // Built-in
    "Eq.flix" -> LocalResource.get("/src/library/Eq.flix"),
    "Hash.flix" -> LocalResource.get("/src/library/Hash.flix"),
    "Sendable.flix" -> LocalResource.get("/src/library/Sendable.flix"),
    "Order.flix" -> LocalResource.get("/src/library/Order.flix"),

    // Lattices
    "PartialOrder.flix" -> LocalResource.get("/src/library/PartialOrder.flix"),
    "LowerBound.flix" -> LocalResource.get("/src/library/LowerBound.flix"),
    "UpperBound.flix" -> LocalResource.get("/src/library/UpperBound.flix"),
    "JoinLattice.flix" -> LocalResource.get("/src/library/JoinLattice.flix"),
    "MeetLattice.flix" -> LocalResource.get("/src/library/MeetLattice.flix"),

    // String
    "ToString.flix" -> LocalResource.get("/src/library/ToString.flix"),

    // Reflect
    "Reflect.flix" -> LocalResource.get("/src/library/Reflect.flix"),

    // Debug
    "Debug.flix" -> LocalResource.get("/src/library/Debug.flix"),

    // References
    "Ref.flix" -> LocalResource.get("/src/library/Ref.flix"),
  )

  /**
    * A sequence of internal inputs to be parsed into Flix ASTs.
    *
    * The standard library is not required to be present for at least some programs to compile.
    */
  private val standardLibrary = List(
    "Array.flix" -> LocalResource.get("/src/library/Array.flix"),
    "Assert.flix" -> LocalResource.get("/src/library/Assert.flix"),
    "Benchmark.flix" -> LocalResource.get("/src/library/Benchmark.flix"),
    "BigDecimal.flix" -> LocalResource.get("/src/library/BigDecimal.flix"),
    "BigInt.flix" -> LocalResource.get("/src/library/BigInt.flix"),
    "Boxable.flix" -> LocalResource.get("/src/library/Boxable.flix"),
    "Boxed.flix" -> LocalResource.get("/src/library/Boxed.flix"),
    "Chain.flix" -> LocalResource.get("/src/library/Chain.flix"),
    "Char.flix" -> LocalResource.get("/src/library/Char.flix"),
    "CodePoint.flix" -> LocalResource.get("/src/library/CodePoint.flix"),
    "Console.flix" -> LocalResource.get("/src/library/Console.flix"),
    "DelayList.flix" -> LocalResource.get("/src/library/DelayList.flix"),
    "DelayMap.flix" -> LocalResource.get("/src/library/DelayMap.flix"),
    "Down.flix" -> LocalResource.get("/src/library/Down.flix"),
    "Float32.flix" -> LocalResource.get("/src/library/Float32.flix"),
    "Float64.flix" -> LocalResource.get("/src/library/Float64.flix"),
    "Int8.flix" -> LocalResource.get("/src/library/Int8.flix"),
    "Int16.flix" -> LocalResource.get("/src/library/Int16.flix"),
    "Int32.flix" -> LocalResource.get("/src/library/Int32.flix"),
    "Int64.flix" -> LocalResource.get("/src/library/Int64.flix"),
    "Iterable.flix" -> LocalResource.get("/src/library/Iterable.flix"),
    "Iterator.flix" -> LocalResource.get("/src/library/Iterator.flix"),
    "List.flix" -> LocalResource.get("/src/library/List.flix"),
    "Map.flix" -> LocalResource.get("/src/library/Map.flix"),
    "Nec.flix" -> LocalResource.get("/src/library/Nec.flix"),
    "Nel.flix" -> LocalResource.get("/src/library/Nel.flix"),
    "Object.flix" -> LocalResource.get("/src/library/Object.flix"),
    "Option.flix" -> LocalResource.get("/src/library/Option.flix"),
    "Random.flix" -> LocalResource.get("/src/library/Random.flix"),
    "Result.flix" -> LocalResource.get("/src/library/Result.flix"),
    "Set.flix" -> LocalResource.get("/src/library/Set.flix"),
    "String.flix" -> LocalResource.get("/src/library/String.flix"),
    "System.flix" -> LocalResource.get("/src/library/System.flix"),
    "MultiMap.flix" -> LocalResource.get("/src/library/MultiMap.flix"),

    "MutQueue.flix" -> LocalResource.get("/src/library/MutQueue.flix"),
    "MutDeque.flix" -> LocalResource.get("/src/library/MutDeque.flix"),
    "MutDisjointSets.flix" -> LocalResource.get("/src/library/MutDisjointSets.flix"),
    "MutList.flix" -> LocalResource.get("/src/library/MutList.flix"),
    "MutSet.flix" -> LocalResource.get("/src/library/MutSet.flix"),
    "MutMap.flix" -> LocalResource.get("/src/library/MutMap.flix"),

    "Files.flix" -> LocalResource.get("/src/library/Files.flix"),
    "IOError.flix" -> LocalResource.get("/src/library/IOError.flix"),
    "Reader.flix" -> LocalResource.get("/src/library/Reader.flix"),
    "File.flix" -> LocalResource.get("/src/library/File.flix"),

    "Environment.flix" -> LocalResource.get("/src/library/Environment.flix"),

    "Applicative.flix" -> LocalResource.get("/src/library/Applicative.flix"),
    "CommutativeGroup.flix" -> LocalResource.get("/src/library/CommutativeGroup.flix"),
    "CommutativeMonoid.flix" -> LocalResource.get("/src/library/CommutativeMonoid.flix"),
    "CommutativeSemiGroup.flix" -> LocalResource.get("/src/library/CommutativeSemiGroup.flix"),
    "Foldable.flix" -> LocalResource.get("/src/library/Foldable.flix"),
    "FromString.flix" -> LocalResource.get("/src/library/FromString.flix"),
    "Functor.flix" -> LocalResource.get("/src/library/Functor.flix"),
    "Filterable.flix" -> LocalResource.get("/src/library/Filterable.flix"),
    "Group.flix" -> LocalResource.get("/src/library/Group.flix"),
    "Identity.flix" -> LocalResource.get("/src/library/Identity.flix"),
    "Monad.flix" -> LocalResource.get("/src/library/Monad.flix"),
    "MonadZero.flix" -> LocalResource.get("/src/library/MonadZero.flix"),
    "MonadZip.flix" -> LocalResource.get("/src/library/MonadZip.flix"),
    "Monoid.flix" -> LocalResource.get("/src/library/Monoid.flix"),
    "Reducible.flix" -> LocalResource.get("/src/library/Reducible.flix"),
    "SemiGroup.flix" -> LocalResource.get("/src/library/SemiGroup.flix"),
    "Traversable.flix" -> LocalResource.get("/src/library/Traversable.flix"),
    "Witherable.flix" -> LocalResource.get("/src/library/Witherable.flix"),
    "UnorderedFoldable.flix" -> LocalResource.get("/src/library/UnorderedFoldable.flix"),
    "Collectable.flix" -> LocalResource.get("/src/library/Collectable.flix"),

    "Validation.flix" -> LocalResource.get("/src/library/Validation.flix"),

    "StringBuilder.flix" -> LocalResource.get("/src/library/StringBuilder.flix"),
    "RedBlackTree.flix" -> LocalResource.get("/src/library/RedBlackTree.flix"),
    "GetOpt.flix" -> LocalResource.get("/src/library/GetOpt.flix"),

    "Concurrent/Channel.flix" -> LocalResource.get("/src/library/Concurrent/Channel.flix"),
    "Concurrent/Condition.flix" -> LocalResource.get("/src/library/Concurrent/Condition.flix"),
    "Concurrent/CyclicBarrier.flix" -> LocalResource.get("/src/library/Concurrent/CyclicBarrier.flix"),
    "Concurrent/ReentrantLock.flix" -> LocalResource.get("/src/library/Concurrent/ReentrantLock.flix"),

    "Time/Duration.flix" -> LocalResource.get("/src/library/Time/Duration.flix"),
    "Time/Epoch.flix" -> LocalResource.get("/src/library/Time/Epoch.flix"),
    "Time/Instant.flix" -> LocalResource.get("/src/library/Time/Instant.flix"),

    "Fixpoint/Phase/Stratifier.flix" -> LocalResource.get("/src/library/Fixpoint/Phase/Stratifier.flix"),
    "Fixpoint/Phase/Compiler.flix" -> LocalResource.get("/src/library/Fixpoint/Phase/Compiler.flix"),
    "Fixpoint/Phase/Simplifier.flix" -> LocalResource.get("/src/library/Fixpoint/Phase/Simplifier.flix"),
    "Fixpoint/Phase/IndexSelection.flix" -> LocalResource.get("/src/library/Fixpoint/Phase/IndexSelection.flix"),
    "Fixpoint/Phase/VarsToIndices.flix" -> LocalResource.get("/src/library/Fixpoint/Phase/VarsToIndices.flix"),
    "Fixpoint/Debugging.flix" -> LocalResource.get("/src/library/Fixpoint/Debugging.flix"),
    "Fixpoint/Interpreter.flix" -> LocalResource.get("/src/library/Fixpoint/Interpreter.flix"),
    "Fixpoint/Options.flix" -> LocalResource.get("/src/library/Fixpoint/Options.flix"),
    "Fixpoint/PredSymsOf.flix" -> LocalResource.get("/src/library/Fixpoint/PredSymsOf.flix"),
    "Fixpoint/Solver.flix" -> LocalResource.get("/src/library/Fixpoint/Solver.flix"),
    "Fixpoint/SubstitutePredSym.flix" -> LocalResource.get("/src/library/Fixpoint/SubstitutePredSym.flix"),

    "Fixpoint/Ast/Datalog.flix" -> LocalResource.get("/src/library/Fixpoint/Ast/Datalog.flix"),
    "Fixpoint/Ast/Shared.flix" -> LocalResource.get("/src/library/Fixpoint/Ast/Shared.flix"),
    "Fixpoint/Ast/PrecedenceGraph.flix" -> LocalResource.get("/src/library/Fixpoint/Ast/PrecedenceGraph.flix"),
    "Fixpoint/Ast/Ram.flix" -> LocalResource.get("/src/library/Fixpoint/Ast/Ram.flix"),

    "Eff/Random.flix" -> LocalResource.get("/src/library/Eff/Random.flix"),

    "Graph.flix" -> LocalResource.get("/src/library/Graph.flix"),
    "Vector.flix" -> LocalResource.get("/src/library/Vector.flix"),
    "Regex.flix" -> LocalResource.get("/src/library/Regex.flix"),
    "Adaptor.flix" -> LocalResource.get("/src/library/Adaptor.flix"),
    "ToJava.flix" -> LocalResource.get("/src/library/ToJava.flix"),
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
  def addSourceCode(name: String, text: String): Flix = {
    if (name == null)
      throw new IllegalArgumentException("'name' must be non-null.")
    if (text == null)
      throw new IllegalArgumentException("'text' must be non-null.")
    addInput(name, Input.Text(name, text, stable = false))
    this
  }

  /**
    * Removes the source code with the given `name`.
    */
  def remSourceCode(name: String): Flix = {
    if (name == null)
      throw new IllegalArgumentException("'name' must be non-null.")
    remInput(name, Input.Text(name, "", stable = false))
    this
  }

  /**
    * Adds the given path `p` as Flix source file.
    */
  def addFlix(p: Path): Flix = {
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

    addInput(p.toString, Input.TxtFile(p))
    this
  }

  /**
    * Adds the given path `p` as a Flix package file.
    */
  def addPkg(p: Path): Flix = {
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

    addInput(p.toString, Input.PkgFile(p))
    this
  }

  /**
    * Removes the given path `p` as a Flix source file.
    */
  def remFlix(p: Path): Flix = {
    if (!p.getFileName.toString.endsWith(".flix"))
      throw new IllegalArgumentException(s"'$p' must be a *.flix file.")

    remInput(p.toString, Input.TxtFile(p))
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
    * Removes the given `input` under the given `name`.
    *
    * Note: Removing an input means to replace it by the empty string.
    */
  private def remInput(name: String, input: Input): Unit = inputs.get(name) match {
    case None => // nop
    case Some(_) =>
      changeSet = changeSet.markChanged(input)
      inputs += name -> Input.Text(name, "", stable = false)
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
  def mkMessages(errors: Chain[CompilationMessage]): List[String] = {
    if (options.explain)
      errors.toSeq.sortBy(_.loc).map(cm => cm.message(formatter) + cm.explain(formatter).getOrElse("")).toList
    else
      errors.toSeq.sortBy(_.loc).map(cm => cm.message(formatter)).toList
  }

  /**
    * Compiles the Flix program and returns a typed ast.
    */
  def check(): Validation[TypedAst.Root, CompilationMessage] = try {
    import Validation.Implicit.AsMonad

    // Mark this object as implicit.
    implicit val flix: Flix = this

    // Initialize fork-join thread pool.
    initForkJoinPool()

    // Reset the phase information.
    phaseTimers = ListBuffer.empty

    // The default entry point
    val entryPoint = flix.options.entryPoint

    implicit class MappableValidation[A, B](v: Validation[A, B]) {
      implicit def map[C](f: A => C): Validation[C, B] = Validation.mapN(v)(f)
    }

    /** Remember to update [[AstPrinter]] about the list of phases. */
    val result = for {
      afterReader <- Reader.run(getInputs)
      afterLexer <- Lexer.run(afterReader, cachedLexerTokens, changeSet)
      afterParser <- Parser.run(afterReader, entryPoint, cachedParserAst, changeSet)
      afterWeeder <- Weeder.run(afterParser, cachedWeederAst, changeSet)

      // Plan for migrating to new parser + weeder:
      // Stage 1 [ACTIVE]
      // Run new pipeline but throw out _both results and errors_. We just want hard throws to surface.
      // Parser2 and Weeder2 only ever sees code that the old pipeline considers ok.
      //
      // Stage 2
      // Run new pipeline by default, but make the old one available through --XParser option.
      //
      // Stage 3
      // Full migration, remove old parser and weeder.
      afterParser2 <- Parser2.runSilent(afterLexer, cachedParserCst, changeSet)
      afterWeeder2 <- Weeder2.runSilent(afterReader, entryPoint, afterParser2, cachedWeederAst, changeSet)

      afterDesugar = Desugar.run(afterWeeder, cachedDesugarAst, changeSet)
      afterNamer <- Namer.run(afterDesugar)
      afterResolver <- Resolver.run(afterNamer, cachedResolverAst, changeSet)
      afterKinder <- Kinder.run(afterResolver, cachedKinderAst, changeSet)
      afterDeriver <- Deriver.run(afterKinder)
      afterTyper <- Typer.run(afterDeriver, cachedTyperAst, changeSet)
      _ <- Regions.run(afterTyper)
      afterEntryPoint <- EntryPoint.run(afterTyper)
      _ <- Instances.run(afterEntryPoint, cachedTyperAst, changeSet)
      afterPredDeps <- PredDeps.run(afterEntryPoint)
      afterStratifier <- Stratifier.run(afterPredDeps)
      afterPatMatch <- PatMatch.run(afterStratifier)
      afterRedundancy <- Redundancy.run(afterPatMatch)
      afterSafety <- Safety.run(afterRedundancy)
    } yield {
      // Update caches for incremental compilation.
      if (options.incremental) {
        this.cachedLexerTokens = afterLexer
        this.cachedParserAst = afterParser
        this.cachedParserCst = afterParser2
        this.cachedWeederAst = afterWeeder
        this.cachedWeederAst2 = afterWeeder2
        this.cachedDesugarAst = afterDesugar
        this.cachedKinderAst = afterKinder
        this.cachedResolverAst = afterResolver
        this.cachedTyperAst = afterTyper
      }
      afterSafety
    }

    // Write formatted asts to disk based on options.
    // (Possible duplicate files in codeGen will just be empty and overwritten there)
    AstPrinter.printAsts()

    // Shutdown fork-join thread pool.
    shutdownForkJoinPool()

    // Reset the progress bar.
    progressBar.complete()

    // Print summary?
    if (options.xsummary) {
      Summary.printSummary(result)
    }

    // Return the result (which could contain soft failures).
    result
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

    /** Remember to update [[AstPrinter]] about the list of phases. */
    cachedLoweringAst = Lowering.run(typedAst)
    cachedTreeShaker1Ast = TreeShaker1.run(cachedLoweringAst)
    cachedMonomorpherAst = Monomorpher.run(cachedTreeShaker1Ast)
    cachedMonoTypesAst = MonoTypes.run(cachedMonomorpherAst)
    cachedSimplifierAst = Simplifier.run(cachedMonoTypesAst)
    cachedClosureConvAst = ClosureConv.run(cachedSimplifierAst)
    cachedLambdaLiftAst = LambdaLift.run(cachedClosureConvAst)
    cachedOptimizerAst = Optimizer.run(cachedLambdaLiftAst)
    cachedTreeShaker2Ast = TreeShaker2.run(cachedOptimizerAst)
    cachedEffectBinderAst = EffectBinder.run(cachedTreeShaker2Ast)
    cachedTailPosAst = TailPos.run(cachedEffectBinderAst)
    Verifier.run(cachedTailPosAst)
    cachedEraserAst = Eraser.run(cachedTailPosAst)
    cachedReducerAst = Reducer.run(cachedEraserAst)
    cachedVarOffsetsAst = VarOffsets.run(cachedReducerAst)
    val result = JvmBackend.run(cachedVarOffsetsAst)

    // Write formatted asts to disk based on options.
    AstPrinter.printAsts()

    // Shutdown fork-join thread pool.
    shutdownForkJoinPool()

    // Reset the progress bar.
    progressBar.complete()

    // Return the result.
    Validation.success(result)
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
    val result = check().toHardFailure
    Validation.flatMapN(result)(codeGen)
  }

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

}
