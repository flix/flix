/*
 * Copyright 2025 Magnus Madsen
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

import ca.uwaterloo.flix.util.LocalResource

object Library {

  /**
    * A sequence of internal inputs to be parsed into Flix ASTs.
    *
    * The core library *must* be present for any program to compile.
    */
  val CoreLibraryBase = List(
    "Add.flix" -> LocalResource.get("/src/library/Add.flix"),
    "Bool.flix" -> LocalResource.get("/src/library/Bool.flix"),
    "Coerce.flix" -> LocalResource.get("/src/library/Coerce.flix"),
    "Comparison.flix" -> LocalResource.get("/src/library/Comparison.flix"),
    "Div.flix" -> LocalResource.get("/src/library/Div.flix"),
    "Eq.flix" -> LocalResource.get("/src/library/Eq.flix"),
    "Exn.flix" -> LocalResource.get("/src/library/Exn.flix"),
    "Hash.flix" -> LocalResource.get("/src/library/Hash.flix"),
    "JoinLattice.flix" -> LocalResource.get("/src/library/JoinLattice.flix"),
    "LowerBound.flix" -> LocalResource.get("/src/library/LowerBound.flix"),
    "MeetLattice.flix" -> LocalResource.get("/src/library/MeetLattice.flix"),
    "Mul.flix" -> LocalResource.get("/src/library/Mul.flix"),
    "Neg.flix" -> LocalResource.get("/src/library/Neg.flix"),
    "Order.flix" -> LocalResource.get("/src/library/Order.flix"),
    "PartialOrder.flix" -> LocalResource.get("/src/library/PartialOrder.flix"),
    "Prelude.flix" -> LocalResource.get("/src/library/Prelude.flix"),
    "Ref.flix" -> LocalResource.get("/src/library/Ref.flix"),
    "Reflect.flix" -> LocalResource.get("/src/library/Reflect.flix"),
    "Sub.flix" -> LocalResource.get("/src/library/Sub.flix"),
    "ToString.flix" -> LocalResource.get("/src/library/ToString.flix"),
    "UpperBound.flix" -> LocalResource.get("/src/library/UpperBound.flix"),
  )

  /**
    * JVM-only core overlays. These files are loaded only when the stdlib profile is `jvm`.
    */
  val CoreLibraryJvmOverlay = List(
    "jvm/Reflect.flix" -> LocalResource.get("/src/library/jvm/Reflect.flix"),
  )

  /**
    * The core library used by the default JVM target (portable base + JVM overlays).
    */
  val CoreLibrary: List[(String, String)] = CoreLibraryBase ++ CoreLibraryJvmOverlay

  /**
    * A sequence of internal inputs to be parsed into Flix ASTs.
    *
    * The standard library is not required to be present for at least some programs to compile.
    */
  val StandardLibraryBase = List(
    "Abort.flix" -> LocalResource.get("/src/library/Abort.flix"),
    "Adaptor.flix" -> LocalResource.get("/src/library/Adaptor.flix"),
    "Applicative.flix" -> LocalResource.get("/src/library/Applicative.flix"),
    "Array.flix" -> LocalResource.get("/src/library/Array.flix"),
    "Assert.flix" -> LocalResource.get("/src/library/Assert.flix"),
    "BigDecimal.flix" -> LocalResource.get("/src/library/BigDecimal.flix"),
    "BigInt.flix" -> LocalResource.get("/src/library/BigInt.flix"),
    "Box.flix" -> LocalResource.get("/src/library/Box.flix"),
    "BPlusTree.flix" -> LocalResource.get("/src/library/BPlusTree.flix"),
    "BPlusTree/AtomicCounter.flix" -> LocalResource.get("/src/library/BPlusTree/AtomicCounter.flix"),
    "BPlusTree/Lock.flix" -> LocalResource.get("/src/library/BPlusTree/Lock.flix"),
    "BPlusTree/Node.flix" -> LocalResource.get("/src/library/BPlusTree/Node.flix"),
    "BufReader.flix" -> LocalResource.get("/src/library/BufReader.flix"),
    "Chain.flix" -> LocalResource.get("/src/library/Chain.flix"),
    "Channel.flix" -> LocalResource.get("/src/library/Channel.flix"),
    "Char.flix" -> LocalResource.get("/src/library/Char.flix"),
    "CharacterSet.flix" -> LocalResource.get("/src/library/CharacterSet.flix"),
    "Clock.flix" -> LocalResource.get("/src/library/Clock.flix"),
    "CodePoint.flix" -> LocalResource.get("/src/library/CodePoint.flix"),
    "Collectable.flix" -> LocalResource.get("/src/library/Collectable.flix"),
    "CommutativeGroup.flix" -> LocalResource.get("/src/library/CommutativeGroup.flix"),
    "CommutativeMonoid.flix" -> LocalResource.get("/src/library/CommutativeMonoid.flix"),
    "CommutativeSemiGroup.flix" -> LocalResource.get("/src/library/CommutativeSemiGroup.flix"),
    "Concurrent.flix" -> LocalResource.get("/src/library/Concurrent.flix"),
    "Concurrent/Channel.flix" -> LocalResource.get("/src/library/Concurrent/Channel.flix"),
    "Concurrent/Condition.flix" -> LocalResource.get("/src/library/Concurrent/Condition.flix"),
    "Concurrent/CyclicBarrier.flix" -> LocalResource.get("/src/library/Concurrent/CyclicBarrier.flix"),
    "Concurrent/ReentrantLock.flix" -> LocalResource.get("/src/library/Concurrent/ReentrantLock.flix"),
    "Console.flix" -> LocalResource.get("/src/library/Console.flix"),
    "Debug.flix" -> LocalResource.get("/src/library/Debug.flix"),
    "DecodingReader.flix" -> LocalResource.get("/src/library/DecodingReader.flix"),
    "DelayList.flix" -> LocalResource.get("/src/library/DelayList.flix"),
    "DelayMap.flix" -> LocalResource.get("/src/library/DelayMap.flix"),
    "Discrete.flix" -> LocalResource.get("/src/library/Discrete.flix"),
    "Dns.flix" -> LocalResource.get("/src/library/Dns.flix"),
    "DnsWithResult.flix" -> LocalResource.get("/src/library/DnsWithResult.flix"),
    "Down.flix" -> LocalResource.get("/src/library/Down.flix"),
    "EncodingWriter.flix" -> LocalResource.get("/src/library/EncodingWriter.flix"),
    "Env.flix" -> LocalResource.get("/src/library/Env.flix"),
    "Exit.flix" -> LocalResource.get("/src/library/Exit.flix"),
    "FileRead.flix" -> LocalResource.get("/src/library/FileRead.flix"),
    "FileReadWithResult.flix" -> LocalResource.get("/src/library/FileReadWithResult.flix"),
    "FileWrite.flix" -> LocalResource.get("/src/library/FileWrite.flix"),
    "FileWriteWithResult.flix" -> LocalResource.get("/src/library/FileWriteWithResult.flix"),
    "Filterable.flix" -> LocalResource.get("/src/library/Filterable.flix"),
    "Float32.flix" -> LocalResource.get("/src/library/Float32.flix"),
    "Float64.flix" -> LocalResource.get("/src/library/Float64.flix"),
    "Foldable.flix" -> LocalResource.get("/src/library/Foldable.flix"),
    "ForEach.flix" -> LocalResource.get("/src/library/ForEach.flix"),
    "Formattable.flix" -> LocalResource.get("/src/library/Formattable.flix"),
    "FromString.flix" -> LocalResource.get("/src/library/FromString.flix"),
    "Functor.flix" -> LocalResource.get("/src/library/Functor.flix"),
    "GetOpt.flix" -> LocalResource.get("/src/library/GetOpt.flix"),
    "Graph.flix" -> LocalResource.get("/src/library/Graph.flix"),
    "Group.flix" -> LocalResource.get("/src/library/Group.flix"),
    "Http.flix" -> LocalResource.get("/src/library/Http.flix"),
    "Http/Response.flix" -> LocalResource.get("/src/library/Http/Response.flix"),
    "HttpWithResult.flix" -> LocalResource.get("/src/library/HttpWithResult.flix"),
    "Identity.flix" -> LocalResource.get("/src/library/Identity.flix"),
    "Indexable.flix" -> LocalResource.get("/src/library/Indexable.flix"),
    "IndexableMut.flix" -> LocalResource.get("/src/library/IndexableMut.flix"),
    "Int16.flix" -> LocalResource.get("/src/library/Int16.flix"),
    "Int32.flix" -> LocalResource.get("/src/library/Int32.flix"),
    "Int64.flix" -> LocalResource.get("/src/library/Int64.flix"),
    "Int8.flix" -> LocalResource.get("/src/library/Int8.flix"),
    "IoError.flix" -> LocalResource.get("/src/library/IoError.flix"),
    "IpAddr.flix" -> LocalResource.get("/src/library/IpAddr.flix"),
    "Ipv4Addr.flix" -> LocalResource.get("/src/library/Ipv4Addr.flix"),
    "Ipv6Addr.flix" -> LocalResource.get("/src/library/Ipv6Addr.flix"),
    "Iterable.flix" -> LocalResource.get("/src/library/Iterable.flix"),
    "Iterator.flix" -> LocalResource.get("/src/library/Iterator.flix"),
    "KeyNotFound.flix" -> LocalResource.get("/src/library/KeyNotFound.flix"),
    "List.flix" -> LocalResource.get("/src/library/List.flix"),
    "Logger.flix" -> LocalResource.get("/src/library/Logger.flix"),
    "Map.flix" -> LocalResource.get("/src/library/Map.flix"),
    "Monad.flix" -> LocalResource.get("/src/library/Monad.flix"),
    "MonadZero.flix" -> LocalResource.get("/src/library/MonadZero.flix"),
    "MonadZip.flix" -> LocalResource.get("/src/library/MonadZip.flix"),
    "Monoid.flix" -> LocalResource.get("/src/library/Monoid.flix"),
    "MultiMap.flix" -> LocalResource.get("/src/library/MultiMap.flix"),
    "MutDeque.flix" -> LocalResource.get("/src/library/MutDeque.flix"),
    "MutDisjointSets.flix" -> LocalResource.get("/src/library/MutDisjointSets.flix"),
    "MutDisjointSets/Node.flix" -> LocalResource.get("/src/library/MutDisjointSets/Node.flix"),
    "MutHashMap.flix" -> LocalResource.get("/src/library/MutHashMap.flix"),
    "MutHashMap/Entry.flix" -> LocalResource.get("/src/library/MutHashMap/Entry.flix"),
    "MutHashSet.flix" -> LocalResource.get("/src/library/MutHashSet.flix"),
    "MutList.flix" -> LocalResource.get("/src/library/MutList.flix"),
    "MutMap.flix" -> LocalResource.get("/src/library/MutMap.flix"),
    "MutPriorityQueue.flix" -> LocalResource.get("/src/library/MutPriorityQueue.flix"),
    "MutSet.flix" -> LocalResource.get("/src/library/MutSet.flix"),
    "Nec.flix" -> LocalResource.get("/src/library/Nec.flix"),
    "Nel.flix" -> LocalResource.get("/src/library/Nel.flix"),
    "Object.flix" -> LocalResource.get("/src/library/Object.flix"),
    "Option.flix" -> LocalResource.get("/src/library/Option.flix"),
    "OutOfBounds.flix" -> LocalResource.get("/src/library/OutOfBounds.flix"),
    "Peekable.flix" -> LocalResource.get("/src/library/Peekable.flix"),
    "Ping.flix" -> LocalResource.get("/src/library/Ping.flix"),
    "PingWithResult.flix" -> LocalResource.get("/src/library/PingWithResult.flix"),
    "Process.flix" -> LocalResource.get("/src/library/Process.flix"),
    "ProcessHandle.flix" -> LocalResource.get("/src/library/ProcessHandle.flix"),
    "ProcessWithResult.flix" -> LocalResource.get("/src/library/ProcessWithResult.flix"),
    "Random.flix" -> LocalResource.get("/src/library/Random.flix"),
    "Range.flix" -> LocalResource.get("/src/library/Range.flix"),
    "Readable.flix" -> LocalResource.get("/src/library/Readable.flix"),
    "RedBlackTree.flix" -> LocalResource.get("/src/library/RedBlackTree.flix"),
    "Reducible.flix" -> LocalResource.get("/src/library/Reducible.flix"),
    "Regex.flix" -> LocalResource.get("/src/library/Regex.flix"),
    "Result.flix" -> LocalResource.get("/src/library/Result.flix"),
    "RichString.flix" -> LocalResource.get("/src/library/RichString.flix"),
    "SemiGroup.flix" -> LocalResource.get("/src/library/SemiGroup.flix"),
    "Set.flix" -> LocalResource.get("/src/library/Set.flix"),
    "Severity.flix" -> LocalResource.get("/src/library/Severity.flix"),
    "Shuffle.flix" -> LocalResource.get("/src/library/Shuffle.flix"),
    "SocketAddr.flix" -> LocalResource.get("/src/library/SocketAddr.flix"),
    "SocketAddrV4.flix" -> LocalResource.get("/src/library/SocketAddrV4.flix"),
    "SocketAddrV6.flix" -> LocalResource.get("/src/library/SocketAddrV6.flix"),
    "String.flix" -> LocalResource.get("/src/library/String.flix"),
    "StringBuilder.flix" -> LocalResource.get("/src/library/StringBuilder.flix"),
    "Sync.flix" -> LocalResource.get("/src/library/Sync.flix"),
    "Sync/CountDownLatch.flix" -> LocalResource.get("/src/library/Sync/CountDownLatch.flix"),
    "Sync/CyclicBarrier.flix" -> LocalResource.get("/src/library/Sync/CyclicBarrier.flix"),
    "Sync/Condition.flix" -> LocalResource.get("/src/library/Sync/Condition.flix"),
    "Sync/ReentrantLock.flix" -> LocalResource.get("/src/library/Sync/ReentrantLock.flix"),
    "Sync/Semaphore.flix" -> LocalResource.get("/src/library/Sync/Semaphore.flix"),
    "TcpAccept.flix" -> LocalResource.get("/src/library/TcpAccept.flix"),
    "TcpAcceptWithResult.flix" -> LocalResource.get("/src/library/TcpAcceptWithResult.flix"),
    "TcpBind.flix" -> LocalResource.get("/src/library/TcpBind.flix"),
    "TcpBindWithResult.flix" -> LocalResource.get("/src/library/TcpBindWithResult.flix"),
    "TcpConnect.flix" -> LocalResource.get("/src/library/TcpConnect.flix"),
    "TcpConnectWithResult.flix" -> LocalResource.get("/src/library/TcpConnectWithResult.flix"),
    "TcpServer.flix" -> LocalResource.get("/src/library/TcpServer.flix"),
    "TcpSocket.flix" -> LocalResource.get("/src/library/TcpSocket.flix"),
    "Timer.flix" -> LocalResource.get("/src/library/Timer.flix"),
    "TimeUnit.flix" -> LocalResource.get("/src/library/TimeUnit.flix"),
    "Traversable.flix" -> LocalResource.get("/src/library/Traversable.flix"),
    "UnorderedFoldable.flix" -> LocalResource.get("/src/library/UnorderedFoldable.flix"),
    "Validation.flix" -> LocalResource.get("/src/library/Validation.flix"),
    "Vector.flix" -> LocalResource.get("/src/library/Vector.flix"),
    "Witherable.flix" -> LocalResource.get("/src/library/Witherable.flix"),
    "Writable.flix" -> LocalResource.get("/src/library/Writable.flix"),
  )

  /**
    * The portable standard library baseline (loaded when the stdlib profile is `portable`).
    *
    * This list is intentionally conservative: it excludes JVM-only modules that rely on Java interop.
    *
    * Note: JVM-only overlays live in [[StandardLibraryJvmOverlay]].
    */
  private val StandardLibraryPortableExclude: Set[String] = Set(
    "Adaptor.flix",
    "Assert.flix",
    "BPlusTree.flix",
    "BPlusTree/AtomicCounter.flix",
    "BPlusTree/Lock.flix",
    "BPlusTree/Node.flix",
    "BigDecimal.flix",
    "BigInt.flix",
    "Box.flix",
    "CharacterSet.flix",
    "CodePoint.flix",
    "Concurrent.flix",
    "Concurrent/Channel.flix",
    "Concurrent/Condition.flix",
    "Concurrent/CyclicBarrier.flix",
    "Concurrent/ReentrantLock.flix",
    "Debug.flix",
    "DecodingReader.flix",
    "EncodingWriter.flix",
  )

  /**
    * Portable-only overlays. These files are loaded only when the stdlib profile is `portable`.
    *
    * They provide default handlers and implementations that do not rely on Java interop.
    */
  private val StandardLibraryPortableOverlay = List(
    "portable/BPlusTree.flix" -> LocalResource.get("/src/library/portable/BPlusTree.flix"),
    "portable/Assert.flix" -> LocalResource.get("/src/library/portable/Assert.flix"),
    "portable/Clock.flix" -> LocalResource.get("/src/library/portable/Clock.flix"),
    "portable/Random.flix" -> LocalResource.get("/src/library/portable/Random.flix"),
    "portable/BigDecimal.flix" -> LocalResource.get("/src/library/portable/BigDecimal.flix"),
    "portable/BigInt.flix" -> LocalResource.get("/src/library/portable/BigInt.flix"),
    "portable/CharacterSet.flix" -> LocalResource.get("/src/library/portable/CharacterSet.flix"),
    "portable/CodePoint.flix" -> LocalResource.get("/src/library/portable/CodePoint.flix"),
    "portable/DecodingReader.flix" -> LocalResource.get("/src/library/portable/DecodingReader.flix"),
    "portable/Debug.flix" -> LocalResource.get("/src/library/portable/Debug.flix"),
    "portable/EncodingWriter.flix" -> LocalResource.get("/src/library/portable/EncodingWriter.flix"),
    "portable/Float32.flix" -> LocalResource.get("/src/library/portable/Float32.flix"),
    "portable/Float64.flix" -> LocalResource.get("/src/library/portable/Float64.flix"),
    "portable/FileRead.flix" -> LocalResource.get("/src/library/portable/FileRead.flix"),
    "portable/FileReadWithResult.flix" -> LocalResource.get("/src/library/portable/FileReadWithResult.flix"),
    "portable/FileWrite.flix" -> LocalResource.get("/src/library/portable/FileWrite.flix"),
    "portable/FileWriteWithResult.flix" -> LocalResource.get("/src/library/portable/FileWriteWithResult.flix"),
    "portable/Int8.flix" -> LocalResource.get("/src/library/portable/Int8.flix"),
    "portable/Int16.flix" -> LocalResource.get("/src/library/portable/Int16.flix"),
    "portable/Int32.flix" -> LocalResource.get("/src/library/portable/Int32.flix"),
    "portable/Int64.flix" -> LocalResource.get("/src/library/portable/Int64.flix"),
  )

  private val Fixpoint3LibraryPortableOverlay = List(
    "Fixpoint3.flix" -> LocalResource.get("/src/library/portable/Fixpoint3.flix"),
    "Fixpoint3/Ast.flix" -> LocalResource.get("/src/library/Fixpoint3/Ast.flix"),
    "Fixpoint3/Ast/Datalog.flix" -> LocalResource.get("/src/library/Fixpoint3/Ast/Datalog.flix"),
    "Fixpoint3/Ast/ExecutableRam.flix" -> LocalResource.get("/src/library/Fixpoint3/Ast/ExecutableRam.flix"),
    "Fixpoint3/Ast/Ram.flix" -> LocalResource.get("/src/library/Fixpoint3/Ast/Ram.flix"),
    "Fixpoint3/Ast/Shared.flix" -> LocalResource.get("/src/library/Fixpoint3/Ast/Shared.flix"),
    "Fixpoint3/Boxable.flix" -> LocalResource.get("/src/library/portable/Fixpoint3/Boxable.flix"),
    "Fixpoint3/Boxing.flix" -> LocalResource.get("/src/library/portable/Fixpoint3/Boxing.flix"),
    "Fixpoint3/BoxingType.flix" -> LocalResource.get("/src/library/Fixpoint3/BoxingType.flix"),
    "Fixpoint3/Counter.flix" -> LocalResource.get("/src/library/Fixpoint3/Counter.flix"),
    "Fixpoint3/Debugging.flix" -> LocalResource.get("/src/library/Fixpoint3/Debugging.flix"),
    "Fixpoint3/Interpreter.flix" -> LocalResource.get("/src/library/Fixpoint3/Interpreter.flix"),
    "Fixpoint3/Options.flix" -> LocalResource.get("/src/library/portable/Fixpoint3/Options.flix"),
    "Fixpoint3/Phase.flix" -> LocalResource.get("/src/library/Fixpoint3/Phase.flix"),
    "Fixpoint3/Phase/Compiler.flix" -> LocalResource.get("/src/library/Fixpoint3/Phase/Compiler.flix"),
    "Fixpoint3/Phase/Hoisting.flix" -> LocalResource.get("/src/library/Fixpoint3/Phase/Hoisting.flix"),
    "Fixpoint3/Phase/IndexSelection.flix" -> LocalResource.get("/src/library/Fixpoint3/Phase/IndexSelection.flix"),
    "Fixpoint3/Phase/IndexSelection/AutomaticIndexSelection.flix" -> LocalResource.get("/src/library/Fixpoint3/Phase/IndexSelection/AutomaticIndexSelection.flix"),
    "Fixpoint3/Phase/Lowering.flix" -> LocalResource.get("/src/library/Fixpoint3/Phase/Lowering.flix"),
    "Fixpoint3/Phase/ProvenanceAugment.flix" -> LocalResource.get("/src/library/Fixpoint3/Phase/ProvenanceAugment.flix"),
    "Fixpoint3/Phase/RenamePredSyms.flix" -> LocalResource.get("/src/library/Fixpoint3/Phase/RenamePredSyms.flix"),
    "Fixpoint3/Phase/Simplifier.flix" -> LocalResource.get("/src/library/Fixpoint3/Phase/Simplifier.flix"),
    "Fixpoint3/Phase/Stratifier.flix" -> LocalResource.get("/src/library/Fixpoint3/Phase/Stratifier.flix"),
    "Fixpoint3/PrecedenceGraph.flix" -> LocalResource.get("/src/library/Fixpoint3/PrecedenceGraph.flix"),
    "Fixpoint3/PrecedenceGraph/MutGraph.flix" -> LocalResource.get("/src/library/Fixpoint3/PrecedenceGraph/MutGraph.flix"),
    "Fixpoint3/Predicate.flix" -> LocalResource.get("/src/library/Fixpoint3/Predicate.flix"),
    "Fixpoint3/ProvenanceReconstruct.flix" -> LocalResource.get("/src/library/Fixpoint3/ProvenanceReconstruct.flix"),
    "Fixpoint3/ProvenanceReconstruct/ProofTree.flix" -> LocalResource.get("/src/library/Fixpoint3/ProvenanceReconstruct/ProofTree.flix"),
    "Fixpoint3/ReadWriteLock.flix" -> LocalResource.get("/src/library/portable/Fixpoint3/ReadWriteLock.flix"),
    "Fixpoint3/Solver.flix" -> LocalResource.get("/src/library/Fixpoint3/Solver.flix"),
    "Fixpoint3/TypeInfo.flix" -> LocalResource.get("/src/library/Fixpoint3/TypeInfo.flix"),
    "Fixpoint3/UniqueInts.flix" -> LocalResource.get("/src/library/Fixpoint3/UniqueInts.flix"),
    "Fixpoint3/Util.flix" -> LocalResource.get("/src/library/Fixpoint3/Util.flix"),
  )

  val StandardLibraryPortable: List[(String, String)] =
    StandardLibraryBase.filterNot { case (virtualPath, _) => StandardLibraryPortableExclude.contains(virtualPath) } ++ StandardLibraryPortableOverlay ++ Fixpoint3LibraryPortableOverlay

  /**
    * JVM-only standard overlays. These files are loaded only when the stdlib profile is `jvm`.
    */
  private val Fixpoint3LibraryJvmOverlay = List(
    "Fixpoint3.flix" -> LocalResource.get("/src/library/Fixpoint3.flix"),
    "Fixpoint3/Ast.flix" -> LocalResource.get("/src/library/Fixpoint3/Ast.flix"),
    "Fixpoint3/Ast/Datalog.flix" -> LocalResource.get("/src/library/Fixpoint3/Ast/Datalog.flix"),
    "Fixpoint3/Ast/ExecutableRam.flix" -> LocalResource.get("/src/library/Fixpoint3/Ast/ExecutableRam.flix"),
    "Fixpoint3/Ast/Ram.flix" -> LocalResource.get("/src/library/Fixpoint3/Ast/Ram.flix"),
    "Fixpoint3/Ast/Shared.flix" -> LocalResource.get("/src/library/Fixpoint3/Ast/Shared.flix"),
    "Fixpoint3/Boxable.flix" -> LocalResource.get("/src/library/Fixpoint3/Boxable.flix"),
    "Fixpoint3/Boxing.flix" -> LocalResource.get("/src/library/Fixpoint3/Boxing.flix"),
    "Fixpoint3/BoxingType.flix" -> LocalResource.get("/src/library/Fixpoint3/BoxingType.flix"),
    "Fixpoint3/Counter.flix" -> LocalResource.get("/src/library/Fixpoint3/Counter.flix"),
    "Fixpoint3/Debugging.flix" -> LocalResource.get("/src/library/Fixpoint3/Debugging.flix"),
    "Fixpoint3/Interpreter.flix" -> LocalResource.get("/src/library/Fixpoint3/Interpreter.flix"),
    "Fixpoint3/Options.flix" -> LocalResource.get("/src/library/Fixpoint3/Options.flix"),
    "Fixpoint3/Phase.flix" -> LocalResource.get("/src/library/Fixpoint3/Phase.flix"),
    "Fixpoint3/Phase/Compiler.flix" -> LocalResource.get("/src/library/Fixpoint3/Phase/Compiler.flix"),
    "Fixpoint3/Phase/Hoisting.flix" -> LocalResource.get("/src/library/Fixpoint3/Phase/Hoisting.flix"),
    "Fixpoint3/Phase/IndexSelection.flix" -> LocalResource.get("/src/library/Fixpoint3/Phase/IndexSelection.flix"),
    "Fixpoint3/Phase/IndexSelection/AutomaticIndexSelection.flix" -> LocalResource.get("/src/library/Fixpoint3/Phase/IndexSelection/AutomaticIndexSelection.flix"),
    "Fixpoint3/Phase/Lowering.flix" -> LocalResource.get("/src/library/Fixpoint3/Phase/Lowering.flix"),
    "Fixpoint3/Phase/ProvenanceAugment.flix" -> LocalResource.get("/src/library/Fixpoint3/Phase/ProvenanceAugment.flix"),
    "Fixpoint3/Phase/RenamePredSyms.flix" -> LocalResource.get("/src/library/Fixpoint3/Phase/RenamePredSyms.flix"),
    "Fixpoint3/Phase/Simplifier.flix" -> LocalResource.get("/src/library/Fixpoint3/Phase/Simplifier.flix"),
    "Fixpoint3/Phase/Stratifier.flix" -> LocalResource.get("/src/library/Fixpoint3/Phase/Stratifier.flix"),
    "Fixpoint3/PrecedenceGraph.flix" -> LocalResource.get("/src/library/Fixpoint3/PrecedenceGraph.flix"),
    "Fixpoint3/PrecedenceGraph/MutGraph.flix" -> LocalResource.get("/src/library/Fixpoint3/PrecedenceGraph/MutGraph.flix"),
    "Fixpoint3/Predicate.flix" -> LocalResource.get("/src/library/Fixpoint3/Predicate.flix"),
    "Fixpoint3/ProvenanceReconstruct.flix" -> LocalResource.get("/src/library/Fixpoint3/ProvenanceReconstruct.flix"),
    "Fixpoint3/ProvenanceReconstruct/ProofTree.flix" -> LocalResource.get("/src/library/Fixpoint3/ProvenanceReconstruct/ProofTree.flix"),
    "Fixpoint3/ReadWriteLock.flix" -> LocalResource.get("/src/library/Fixpoint3/ReadWriteLock.flix"),
    "Fixpoint3/Solver.flix" -> LocalResource.get("/src/library/Fixpoint3/Solver.flix"),
    "Fixpoint3/TypeInfo.flix" -> LocalResource.get("/src/library/Fixpoint3/TypeInfo.flix"),
    "Fixpoint3/UniqueInts.flix" -> LocalResource.get("/src/library/Fixpoint3/UniqueInts.flix"),
    "Fixpoint3/Util.flix" -> LocalResource.get("/src/library/Fixpoint3/Util.flix"),
  )

  val StandardLibraryJvmOverlay = List(
    "jvm/Add.flix" -> LocalResource.get("/src/library/jvm/Add.flix"),
    "jvm/Chain.flix" -> LocalResource.get("/src/library/jvm/Chain.flix"),
    "jvm/Char.flix" -> LocalResource.get("/src/library/jvm/Char.flix"),
    "jvm/Clock.flix" -> LocalResource.get("/src/library/jvm/Clock.flix"),
    "jvm/CommutativeGroup.flix" -> LocalResource.get("/src/library/jvm/CommutativeGroup.flix"),
    "jvm/CommutativeMonoid.flix" -> LocalResource.get("/src/library/jvm/CommutativeMonoid.flix"),
    "jvm/CommutativeSemiGroup.flix" -> LocalResource.get("/src/library/jvm/CommutativeSemiGroup.flix"),
    "jvm/Div.flix" -> LocalResource.get("/src/library/jvm/Div.flix"),
    "jvm/Dns.flix" -> LocalResource.get("/src/library/jvm/Dns.flix"),
    "jvm/DnsWithResult.flix" -> LocalResource.get("/src/library/jvm/DnsWithResult.flix"),
    "jvm/Eq.flix" -> LocalResource.get("/src/library/jvm/Eq.flix"),
    "jvm/FileRead.flix" -> LocalResource.get("/src/library/jvm/FileRead.flix"),
    "jvm/FileReadWithResult.flix" -> LocalResource.get("/src/library/jvm/FileReadWithResult.flix"),
    "jvm/FileWrite.flix" -> LocalResource.get("/src/library/jvm/FileWrite.flix"),
    "jvm/FileWriteWithResult.flix" -> LocalResource.get("/src/library/jvm/FileWriteWithResult.flix"),
    "jvm/Formattable.flix" -> LocalResource.get("/src/library/jvm/Formattable.flix"),
    "jvm/FromString.flix" -> LocalResource.get("/src/library/jvm/FromString.flix"),
    "jvm/Group.flix" -> LocalResource.get("/src/library/jvm/Group.flix"),
    "jvm/Hash.flix" -> LocalResource.get("/src/library/jvm/Hash.flix"),
    "jvm/Float32.flix" -> LocalResource.get("/src/library/jvm/Float32.flix"),
    "jvm/Float64.flix" -> LocalResource.get("/src/library/jvm/Float64.flix"),
    "jvm/Int8.flix" -> LocalResource.get("/src/library/jvm/Int8.flix"),
    "jvm/Int16.flix" -> LocalResource.get("/src/library/jvm/Int16.flix"),
    "jvm/Int32.flix" -> LocalResource.get("/src/library/jvm/Int32.flix"),
    "jvm/Int64.flix" -> LocalResource.get("/src/library/jvm/Int64.flix"),
    "jvm/List.flix" -> LocalResource.get("/src/library/jvm/List.flix"),
    "jvm/Map.flix" -> LocalResource.get("/src/library/jvm/Map.flix"),
    "jvm/MeetLattice.flix" -> LocalResource.get("/src/library/jvm/MeetLattice.flix"),
    "jvm/JoinLattice.flix" -> LocalResource.get("/src/library/jvm/JoinLattice.flix"),
    "jvm/Monoid.flix" -> LocalResource.get("/src/library/jvm/Monoid.flix"),
    "jvm/Mul.flix" -> LocalResource.get("/src/library/jvm/Mul.flix"),
    "jvm/Neg.flix" -> LocalResource.get("/src/library/jvm/Neg.flix"),
    "jvm/Nec.flix" -> LocalResource.get("/src/library/jvm/Nec.flix"),
    "jvm/Nel.flix" -> LocalResource.get("/src/library/jvm/Nel.flix"),
    "jvm/Random.flix" -> LocalResource.get("/src/library/jvm/Random.flix"),
    "jvm/Shuffle.flix" -> LocalResource.get("/src/library/jvm/Shuffle.flix"),
    "jvm/Order.flix" -> LocalResource.get("/src/library/jvm/Order.flix"),
    "jvm/PartialOrder.flix" -> LocalResource.get("/src/library/jvm/PartialOrder.flix"),
    "jvm/Ping.flix" -> LocalResource.get("/src/library/jvm/Ping.flix"),
    "jvm/PingWithResult.flix" -> LocalResource.get("/src/library/jvm/PingWithResult.flix"),
    "jvm/Process.flix" -> LocalResource.get("/src/library/jvm/Process.flix"),
    "jvm/Result.flix" -> LocalResource.get("/src/library/jvm/Result.flix"),
    "jvm/SemiGroup.flix" -> LocalResource.get("/src/library/jvm/SemiGroup.flix"),
    "jvm/Set.flix" -> LocalResource.get("/src/library/jvm/Set.flix"),
    "jvm/Sub.flix" -> LocalResource.get("/src/library/jvm/Sub.flix"),
    "jvm/ToString.flix" -> LocalResource.get("/src/library/jvm/ToString.flix"),
    "jvm/ToFlix.flix" -> LocalResource.get("/src/library/jvm/ToFlix.flix"),
    "jvm/ToJava.flix" -> LocalResource.get("/src/library/jvm/ToJava.flix"),
    "jvm/Vector.flix" -> LocalResource.get("/src/library/jvm/Vector.flix"),
    "jvm/Readable.flix" -> LocalResource.get("/src/library/jvm/Readable.flix"),
    "jvm/Writable.flix" -> LocalResource.get("/src/library/jvm/Writable.flix"),
  ) ++ Fixpoint3LibraryJvmOverlay

  /**
    * The standard library used by the default JVM target (portable base + JVM overlays).
    */
  val StandardLibrary: List[(String, String)] = StandardLibraryBase ++ StandardLibraryJvmOverlay

}
