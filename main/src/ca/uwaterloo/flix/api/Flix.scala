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

    // Reflect
    "Reflect.flix" -> LocalResource.get("/src/library/Reflect.flix"),

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
    "BigDecimal.flix" -> LocalResource.get("/src/library/BigDecimal.flix"),
    "BigInt.flix" -> LocalResource.get("/src/library/BigInt.flix"),
    "Box.flix" -> LocalResource.get("/src/library/Box.flix"),
    "BPlusTree.flix" -> LocalResource.get("/src/library/BPlusTree.flix"),
    "BufReader.flix" -> LocalResource.get("/src/library/BufReader.flix"),
    "Chain.flix" -> LocalResource.get("/src/library/Chain.flix"),
    "Char.flix" -> LocalResource.get("/src/library/Char.flix"),
    "CodePoint.flix" -> LocalResource.get("/src/library/CodePoint.flix"),
    "Console.flix" -> LocalResource.get("/src/library/Console.flix"),
    "DelayList.flix" -> LocalResource.get("/src/library/DelayList.flix"),
    "DelayMap.flix" -> LocalResource.get("/src/library/DelayMap.flix"),
    "Discrete.flix" -> LocalResource.get("/src/library/Discrete.flix"),
    "Down.flix" -> LocalResource.get("/src/library/Down.flix"),
    "Float32.flix" -> LocalResource.get("/src/library/Float32.flix"),
    "Float64.flix" -> LocalResource.get("/src/library/Float64.flix"),
    "Int8.flix" -> LocalResource.get("/src/library/Int8.flix"),
    "Int16.flix" -> LocalResource.get("/src/library/Int16.flix"),
    "Int32.flix" -> LocalResource.get("/src/library/Int32.flix"),
    "Int64.flix" -> LocalResource.get("/src/library/Int64.flix"),
    "Iterable.flix" -> LocalResource.get("/src/library/Iterable.flix"),
    "Iterator.flix" -> LocalResource.get("/src/library/Iterator.flix"),
    "KeyNotFound.flix" -> LocalResource.get("/src/library/KeyNotFound.flix"),
    "List.flix" -> LocalResource.get("/src/library/List.flix"),
    "Map.flix" -> LocalResource.get("/src/library/Map.flix"),
    "Nec.flix" -> LocalResource.get("/src/library/Nec.flix"),
    "Nel.flix" -> LocalResource.get("/src/library/Nel.flix"),
    "Object.flix" -> LocalResource.get("/src/library/Object.flix"),
    "Option.flix" -> LocalResource.get("/src/library/Option.flix"),
    "OutOfBounds.flix" -> LocalResource.get("/src/library/OutOfBounds.flix"),
    "Random.flix" -> LocalResource.get("/src/library/Random.flix"),
    "Range.flix" -> LocalResource.get("/src/library/Range.flix"),
    "Result.flix" -> LocalResource.get("/src/library/Result.flix"),
    "Set.flix" -> LocalResource.get("/src/library/Set.flix"),
    "String.flix" -> LocalResource.get("/src/library/String.flix"),
    "MultiMap.flix" -> LocalResource.get("/src/library/MultiMap.flix"),

    "MutPriorityQueue.flix" -> LocalResource.get("/src/library/MutPriorityQueue.flix"),
    "MutDeque.flix" -> LocalResource.get("/src/library/MutDeque.flix"),
    "MutDisjointSets.flix" -> LocalResource.get("/src/library/MutDisjointSets.flix"),
    "MutHashMap.flix" -> LocalResource.get("/src/library/MutHashMap.flix"),
    "MutHashSet.flix" -> LocalResource.get("/src/library/MutHashSet.flix"),
    "MutList.flix" -> LocalResource.get("/src/library/MutList.flix"),
    "MutSet.flix" -> LocalResource.get("/src/library/MutSet.flix"),
    "MutMap.flix" -> LocalResource.get("/src/library/MutMap.flix"),

    "CharacterSet.flix" -> LocalResource.get("/src/library/CharacterSet.flix"),
    "EncodingWriter.flix" -> LocalResource.get("/src/library/EncodingWriter.flix"),
    "DecodingReader.flix" -> LocalResource.get("/src/library/DecodingReader.flix"),
    "IoError.flix" -> LocalResource.get("/src/library/IoError.flix"),
    "Peekable.flix" -> LocalResource.get("/src/library/Peekable.flix"),
    "Readable.flix" -> LocalResource.get("/src/library/Readable.flix"),
    "Writable.flix" -> LocalResource.get("/src/library/Writable.flix"),

    "Env.flix" -> LocalResource.get("/src/library/Env.flix"),
    "Debug.flix" -> LocalResource.get("/src/library/Debug.flix"),

    "Applicative.flix" -> LocalResource.get("/src/library/Applicative.flix"),
    "CommutativeGroup.flix" -> LocalResource.get("/src/library/CommutativeGroup.flix"),
    "CommutativeMonoid.flix" -> LocalResource.get("/src/library/CommutativeMonoid.flix"),
    "CommutativeSemiGroup.flix" -> LocalResource.get("/src/library/CommutativeSemiGroup.flix"),
    "Foldable.flix" -> LocalResource.get("/src/library/Foldable.flix"),
    "ForEach.flix" -> LocalResource.get("/src/library/ForEach.flix"),
    "FromString.flix" -> LocalResource.get("/src/library/FromString.flix"),
    "Functor.flix" -> LocalResource.get("/src/library/Functor.flix"),
    "Filterable.flix" -> LocalResource.get("/src/library/Filterable.flix"),
    "Group.flix" -> LocalResource.get("/src/library/Group.flix"),
    "Identity.flix" -> LocalResource.get("/src/library/Identity.flix"),
    "Indexable.flix" -> LocalResource.get("/src/library/Indexable.flix"),
    "IndexableMut.flix" -> LocalResource.get("/src/library/IndexableMut.flix"),
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

    "Channel.flix" -> LocalResource.get("/src/library/Channel.flix"),
    "Concurrent/Channel.flix" -> LocalResource.get("/src/library/Concurrent/Channel.flix"),
    "Concurrent/Condition.flix" -> LocalResource.get("/src/library/Concurrent/Condition.flix"),
    "Concurrent/CyclicBarrier.flix" -> LocalResource.get("/src/library/Concurrent/CyclicBarrier.flix"),
    "Concurrent/ReentrantLock.flix" -> LocalResource.get("/src/library/Concurrent/ReentrantLock.flix"),

    "Fixpoint3/Boxable.flix" -> LocalResource.get("/src/library/Fixpoint3/Boxable.flix"),
    "Fixpoint3/Boxed.flix" -> LocalResource.get("/src/library/Fixpoint3/Boxed.flix"),
    "Fixpoint3/Boxing.flix" -> LocalResource.get("/src/library/Fixpoint3/Boxing.flix"),
    "Fixpoint3/BoxingType.flix" -> LocalResource.get("/src/library/Fixpoint3/BoxingType.flix"),
    "Fixpoint3/Counter.flix" -> LocalResource.get("/src/library/Fixpoint3/Counter.flix"),
    "Fixpoint3/Debugging.flix" -> LocalResource.get("/src/library/Fixpoint3/Debugging.flix"),
    "Fixpoint3/Interpreter.flix" -> LocalResource.get("/src/library/Fixpoint3/Interpreter.flix"),
    "Fixpoint3/Options.flix" -> LocalResource.get("/src/library/Fixpoint3/Options.flix"),
    "Fixpoint3/PrecedenceGraph.flix" -> LocalResource.get("/src/library/Fixpoint3/PrecedenceGraph.flix"),
    "Fixpoint3/Predicate.flix" -> LocalResource.get("/src/library/Fixpoint3/Predicate.flix"),
    "Fixpoint3/PredSymsOf.flix" -> LocalResource.get("/src/library/Fixpoint3/PredSymsOf.flix"),
    "Fixpoint3/ProvenanceReconstruct.flix" -> LocalResource.get("/src/library/Fixpoint3/ProvenanceReconstruct.flix"),
    "Fixpoint3/ReadWriteLock.flix" -> LocalResource.get("/src/library/Fixpoint3/ReadWriteLock.flix"),
    "Fixpoint3/Solver.flix" -> LocalResource.get("/src/library/Fixpoint3/Solver.flix"),
    "Fixpoint3/SubstitutePredSym.flix" -> LocalResource.get("/src/library/Fixpoint3/SubstitutePredSym.flix"),
    "Fixpoint3/TypeInfo.flix" -> LocalResource.get("/src/library/Fixpoint3/TypeInfo.flix"),
    "Fixpoint3/UniqueInts.flix" -> LocalResource.get("/src/library/Fixpoint3/UniqueInts.flix"),
    "Fixpoint3/Util.flix" -> LocalResource.get("/src/library/Fixpoint3/Util.flix"),

    "Fixpoint3/Ast/Datalog.flix" -> LocalResource.get("/src/library/Fixpoint3/Ast/Datalog.flix"),
    "Fixpoint3/Ast/ExecutableRam.flix" -> LocalResource.get("/src/library/Fixpoint3/Ast/ExecutableRam.flix"),
    "Fixpoint3/Ast/Ram.flix" -> LocalResource.get("/src/library/Fixpoint3/Ast/Ram.flix"),
    "Fixpoint3/Ast/Shared.flix" -> LocalResource.get("/src/library/Fixpoint3/Ast/Shared.flix"),

    "Fixpoint3/Phase/Compiler.flix" -> LocalResource.get("/src/library/Fixpoint3/Phase/Compiler.flix"),
    "Fixpoint3/Phase/Hoisting.flix" -> LocalResource.get("/src/library/Fixpoint3/Phase/Hoisting.flix"),
    "Fixpoint3/Phase/IndexSelection.flix" -> LocalResource.get("/src/library/Fixpoint3/Phase/IndexSelection.flix"),
    "Fixpoint3/Phase/Lowering.flix" -> LocalResource.get("/src/library/Fixpoint3/Phase/Lowering.flix"),
    "Fixpoint3/Phase/ProvenanceAugment.flix" -> LocalResource.get("/src/library/Fixpoint3/Phase/ProvenanceAugment.flix"),
    "Fixpoint3/Phase/RenamePredSyms.flix" -> LocalResource.get("/src/library/Fixpoint3/Phase/RenamePredSyms.flix"),
    "Fixpoint3/Phase/Simplifier.flix" -> LocalResource.get("/src/library/Fixpoint3/Phase/Simplifier.flix"),
    "Fixpoint3/Phase/Stratifier.flix" -> LocalResource.get("/src/library/Fixpoint3/Phase/Stratifier.flix"),

    "Abort.flix" -> LocalResource.get("/src/library/Abort.flix"),
    "Clock.flix" -> LocalResource.get("/src/library/Clock.flix"),
    "Dns.flix" -> LocalResource.get("/src/library/Dns.flix"),
    "DnsWithResult.flix" -> LocalResource.get("/src/library/DnsWithResult.flix"),
    "Http.flix" -> LocalResource.get("/src/library/Http.flix"),
    "HttpWithResult.flix" -> LocalResource.get("/src/library/HttpWithResult.flix"),
    "Exit.flix" -> LocalResource.get("/src/library/Exit.flix"),
    "Logger.flix" -> LocalResource.get("/src/library/Logger.flix"),
    "FileRead.flix" -> LocalResource.get("/src/library/FileRead.flix"),
    "FileReadWithResult.flix" -> LocalResource.get("/src/library/FileReadWithResult.flix"),
    "FileWrite.flix" -> LocalResource.get("/src/library/FileWrite.flix"),
    "FileWriteWithResult.flix" -> LocalResource.get("/src/library/FileWriteWithResult.flix"),
    "IpAddr.flix" -> LocalResource.get("/src/library/IpAddr.flix"),
    "Ipv4Addr.flix" -> LocalResource.get("/src/library/Ipv4Addr.flix"),
    "Ipv6Addr.flix" -> LocalResource.get("/src/library/Ipv6Addr.flix"),
    "Ping.flix" -> LocalResource.get("/src/library/Ping.flix"),
    "PingWithResult.flix" -> LocalResource.get("/src/library/PingWithResult.flix"),
    "ProcessHandle.flix" -> LocalResource.get("/src/library/ProcessHandle.flix"),
    "Process.flix" -> LocalResource.get("/src/library/Process.flix"),
    "ProcessWithResult.flix" -> LocalResource.get("/src/library/ProcessWithResult.flix"),
    "Severity.flix" -> LocalResource.get("/src/library/Severity.flix"),
    "Shuffle.flix" -> LocalResource.get("/src/library/Shuffle.flix"),
    "SocketAddr.flix" -> LocalResource.get("/src/library/SocketAddr.flix"),
    "SocketAddrV4.flix" -> LocalResource.get("/src/library/SocketAddrV4.flix"),
    "SocketAddrV6.flix" -> LocalResource.get("/src/library/SocketAddrV6.flix"),
    "TcpAccept.flix" -> LocalResource.get("/src/library/TcpAccept.flix"),
    "TcpAcceptWithResult.flix" -> LocalResource.get("/src/library/TcpAcceptWithResult.flix"),
    "TcpBind.flix" -> LocalResource.get("/src/library/TcpBind.flix"),
    "TcpBindWithResult.flix" -> LocalResource.get("/src/library/TcpBindWithResult.flix"),
    "TcpConnect.flix" -> LocalResource.get("/src/library/TcpConnect.flix"),
    "TcpConnectWithResult.flix" -> LocalResource.get("/src/library/TcpConnectWithResult.flix"),
    "TcpServer.flix" -> LocalResource.get("/src/library/TcpServer.flix"),
    "TcpSocket.flix" -> LocalResource.get("/src/library/TcpSocket.flix"),
    "TimeUnit.flix" -> LocalResource.get("/src/library/TimeUnit.flix"),

    "Graph.flix" -> LocalResource.get("/src/library/Graph.flix"),
    "Vector.flix" -> LocalResource.get("/src/library/Vector.flix"),
    "Regex.flix" -> LocalResource.get("/src/library/Regex.flix"),
    "RichString.flix" -> LocalResource.get("/src/library/RichString.flix"),
    "Formattable.flix" -> LocalResource.get("/src/library/Formattable.flix"),
    "Adaptor.flix" -> LocalResource.get("/src/library/Adaptor.flix"),
    "ToJava.flix" -> LocalResource.get("/src/library/ToJava.flix"),
    "ToFlix.flix" -> LocalResource.get("/src/library/ToFlix.flix"),
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
    remInput(name, Input.Text(name, "", /* unused */ SecurityContext.Plain))
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
    isValidFpkgFile(p) match {
      case Result.Err(e: Throwable) => throw e
      case Result.Ok(()) =>
        addInput(p.toString, Input.PkgFile(p, sctx))
        this
    }
  }

  /**
    * Checks that `p` is a valid `.fpkg` filepath.
    * `p` is valid if the following holds:
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
    if (!pNorm.getFileName.toString.endsWith(".fpkg")) {
      return Result.Err(new IllegalArgumentException(s"'$pNorm' must be a .fpkg file."))
    }
    if (!FileOps.isZipArchive(pNorm)) {
      return Result.Err(new IllegalArgumentException(s"'$pNorm' must be a zip archive."))
    }
    Result.Ok(())
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
      inputs += name -> Input.Text(name, "", /* unused */ SecurityContext.Plain)
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

    var loweringAst = Lowering.run(typedAst)
    // Note: Do not null typedAst. It is used later.

    var treeShaker1Ast = TreeShaker1.run(loweringAst)
    loweringAst = null // Explicitly null-out such that the memory becomes eligible for GC.

    var monomorpherAst = Specialization.run(treeShaker1Ast)
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
    val (result, allErrors) = check()
    if (allErrors.isEmpty) {
      codeGen(result.get)
    } else {
      val nonShadowedErrors = CompilationMessage.filterShadowedMessages(allErrors)
      Validation.Failure(Chain.from(nonShadowedErrors))
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
  private def getLibraryInputs(l: List[(String, String)]): List[Input] = l.foldLeft(List.empty[Input]) {
    case (xs, (virtualPath, text)) => Input.Text(virtualPath, text, SecurityContext.Unrestricted) :: xs
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
