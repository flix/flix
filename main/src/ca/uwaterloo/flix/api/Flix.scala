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
  private var cachedParserAst: ParsedAst.Root = ParsedAst.empty
  private var cachedWeederAst: WeededAst.Root = WeededAst.empty
  private var cachedDesugarAst: DesugaredAst.Root = DesugaredAst.empty
  private var cachedKinderAst: KindedAst.Root = KindedAst.empty
  private var cachedResolverAst: ResolvedAst.Root = ResolvedAst.empty
  private var cachedTyperAst: TypedAst.Root = TypedAst.empty

  def getParserAst: ParsedAst.Root = cachedParserAst

  def getWeederAst: WeededAst.Root = cachedWeederAst

  def getDesugarAst: DesugaredAst.Root = cachedDesugarAst

  def getKinderAst: KindedAst.Root = cachedKinderAst

  def getResolverAst: ResolvedAst.Root = cachedResolverAst

  def getTyperAst: TypedAst.Root = cachedTyperAst

  /**
    * A cache of ASTs for debugging.
    */
  private var cachedLoweringAst: LoweredAst.Root = LoweredAst.empty
  private var cachedTreeShaker1Ast: LoweredAst.Root = LoweredAst.empty
  private var cachedMonoDefsAst: LoweredAst.Root = LoweredAst.empty
  private var cachedMonoTypesAst: LoweredAst.Root = LoweredAst.empty
  private var cachedSimplifierAst: SimplifiedAst.Root = SimplifiedAst.empty
  private var cachedClosureConvAst: SimplifiedAst.Root = SimplifiedAst.empty
  private var cachedLambdaLiftAst: LiftedAst.Root = LiftedAst.empty
  private var cachedTailrecAst: LiftedAst.Root = LiftedAst.empty
  private var cachedOptimizerAst: LiftedAst.Root = LiftedAst.empty
  private var cachedTreeShaker2Ast: LiftedAst.Root = LiftedAst.empty
  private var cachedEffectBinderAst: ReducedAst.Root = ReducedAst.empty
  private var cachedEraserAst: ReducedAst.Root = ReducedAst.empty
  private var cachedReducerAst: ReducedAst.Root = ReducedAst.empty
  private var cachedVarOffsetsAst: ReducedAst.Root = ReducedAst.empty

  def getLoweringAst: LoweredAst.Root = cachedLoweringAst

  def getTreeShaker1Ast: LoweredAst.Root = cachedTreeShaker1Ast

  def getMonoDefsAst: LoweredAst.Root = cachedMonoDefsAst

  def getMonoTypesAst: LoweredAst.Root = cachedMonoTypesAst

  def getSimplifierAst: SimplifiedAst.Root = cachedSimplifierAst

  def getClosureConvAst: SimplifiedAst.Root = cachedClosureConvAst

  def getLambdaLiftAst: LiftedAst.Root = cachedLambdaLiftAst

  def getTailrecAst: LiftedAst.Root = cachedTailrecAst

  def getOptimizerAst: LiftedAst.Root = cachedOptimizerAst

  def getTreeShaker2Ast: LiftedAst.Root = cachedTreeShaker2Ast

  def getEffectBinderAst: ReducedAst.Root = cachedEffectBinderAst

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

    //////////////// TODO: REMOVE ALL THESE INCLUDES
    //////////////// ADD TEST FILES ALSO TO TEST PARSER
//    "Test.Assoc.Eff.Contravariance.flix" -> LocalResource.get("/test/flix/Test.Assoc.Eff.Contravariance.flix"),
    "Test.Assoc.Eff.Covariance.flix" -> LocalResource.get("/test/flix/Test.Assoc.Eff.Covariance.flix"),
    "Test.Assoc.Type.flix" -> LocalResource.get("/test/flix/Test.Assoc.Type.flix"),
    "Test.Currying.flix" -> LocalResource.get("/test/flix/Test.Currying.flix"),
//    "Test.Dec.Class.flix" -> LocalResource.get("/test/flix/Test.Dec.Class.flix"),
    "Test.Dec.Effect.flix" -> LocalResource.get("/test/flix/Test.Dec.Effect.flix"),
    "Test.Dec.Enum.Singleton.flix" -> LocalResource.get("/test/flix/Test.Dec.Enum.Singleton.flix"),
    "Test.Dec.Enum.flix" -> LocalResource.get("/test/flix/Test.Dec.Enum.flix"),
    "Test.Dec.Mod.flix" -> LocalResource.get("/test/flix/Test.Dec.Mod.flix"),
//    "Test.Dec.RestrictableTag.flix" -> LocalResource.get("/test/flix/Test.Dec.RestrictableTag.flix"),
    "Test.Dec.TopLevel.flix" -> LocalResource.get("/test/flix/Test.Dec.TopLevel.flix"),
    "Test.Dec.Trait.flix" -> LocalResource.get("/test/flix/Test.Dec.Trait.flix"),
    "Test.Dec.TypeAlias.flix" -> LocalResource.get("/test/flix/Test.Dec.TypeAlias.flix"),
//    "Test.Def.ChooseStar.Simple.flix" -> LocalResource.get("/test/flix/Test.Def.ChooseStar.Simple.flix"),
//    "Test.Def.Generalization.flix" -> LocalResource.get("/test/flix/Test.Def.Generalization.flix"),
//    "Test.Def.Op.flix" -> LocalResource.get("/test/flix/Test.Def.Op.flix"),
    "Test.Derives.Eq.flix" -> LocalResource.get("/test/flix/Test.Derives.Eq.flix"),
    "Test.Derives.Hash.flix" -> LocalResource.get("/test/flix/Test.Derives.Hash.flix"),
    "Test.Derives.Order.flix" -> LocalResource.get("/test/flix/Test.Derives.Order.flix"),
    "Test.Derives.Sendable.flix" -> LocalResource.get("/test/flix/Test.Derives.Sendable.flix"),
    "Test.Derives.ToString.flix" -> LocalResource.get("/test/flix/Test.Derives.ToString.flix"),
    "Test.Eff.Advanced.flix" -> LocalResource.get("/test/flix/Test.Eff.Advanced.flix"),
    "Test.Eff.Polymorphism.flix" -> LocalResource.get("/test/flix/Test.Eff.Polymorphism.flix"),
    "Test.Eff.Simplification.flix" -> LocalResource.get("/test/flix/Test.Eff.Simplification.flix"),
    "Test.Equality.BigDecimal.flix" -> LocalResource.get("/test/flix/Test.Equality.BigDecimal.flix"),
    "Test.Equality.BigInt.flix" -> LocalResource.get("/test/flix/Test.Equality.BigInt.flix"),
    "Test.Equality.Bool.flix" -> LocalResource.get("/test/flix/Test.Equality.Bool.flix"),
    "Test.Equality.Char.flix" -> LocalResource.get("/test/flix/Test.Equality.Char.flix"),
    "Test.Equality.Float32.flix" -> LocalResource.get("/test/flix/Test.Equality.Float32.flix"),
    "Test.Equality.Float64.flix" -> LocalResource.get("/test/flix/Test.Equality.Float64.flix"),
    "Test.Equality.Int16.flix" -> LocalResource.get("/test/flix/Test.Equality.Int16.flix"),
    "Test.Equality.Int32.flix" -> LocalResource.get("/test/flix/Test.Equality.Int32.flix"),
    "Test.Equality.Int64.flix" -> LocalResource.get("/test/flix/Test.Equality.Int64.flix"),
    "Test.Equality.Int8.flix" -> LocalResource.get("/test/flix/Test.Equality.Int8.flix"),
    "Test.Equality.Map.flix" -> LocalResource.get("/test/flix/Test.Equality.Map.flix"),
    "Test.Equality.Set.flix" -> LocalResource.get("/test/flix/Test.Equality.Set.flix"),
    "Test.Equality.String.flix" -> LocalResource.get("/test/flix/Test.Equality.String.flix"),
    "Test.Equality.Tag.flix" -> LocalResource.get("/test/flix/Test.Equality.Tag.flix"),
    "Test.Equality.Tuple.flix" -> LocalResource.get("/test/flix/Test.Equality.Tuple.flix"),
    "Test.Equality.Unit.flix" -> LocalResource.get("/test/flix/Test.Equality.Unit.flix"),
//    "Test.Exp.ApplicativeFor.flix" -> LocalResource.get("/test/flix/Test.Exp.ApplicativeFor.flix"),
    "Test.Exp.Apply.Named.flix" -> LocalResource.get("/test/flix/Test.Exp.Apply.Named.flix"),
    "Test.Exp.Apply.Tail.flix" -> LocalResource.get("/test/flix/Test.Exp.Apply.Tail.flix"),
    "Test.Exp.ArrayLength.flix" -> LocalResource.get("/test/flix/Test.Exp.ArrayLength.flix"),
    "Test.Exp.ArrayLit.flix" -> LocalResource.get("/test/flix/Test.Exp.ArrayLit.flix"),
    "Test.Exp.ArrayLoad.flix" -> LocalResource.get("/test/flix/Test.Exp.ArrayLoad.flix"),
    "Test.Exp.ArrayNew.flix" -> LocalResource.get("/test/flix/Test.Exp.ArrayNew.flix"),
    "Test.Exp.ArrayStore.flix" -> LocalResource.get("/test/flix/Test.Exp.ArrayStore.flix"),
//    "Test.Exp.Ascribe.flix" -> LocalResource.get("/test/flix/Test.Exp.Ascribe.flix"),
    "Test.Exp.BigDecimal.flix" -> LocalResource.get("/test/flix/Test.Exp.BigDecimal.flix"),
    "Test.Exp.BigInt.flix" -> LocalResource.get("/test/flix/Test.Exp.BigInt.flix"),
//    "Test.Exp.Binary.Arithmetic.flix" -> LocalResource.get("/test/flix/Test.Exp.Binary.Arithmetic.flix"),
    "Test.Exp.Binary.Bitwise.flix" -> LocalResource.get("/test/flix/Test.Exp.Binary.Bitwise.flix"),
//    "Test.Exp.Binary.Comparison.flix" -> LocalResource.get("/test/flix/Test.Exp.Binary.Comparison.flix"),
    "Test.Exp.Binary.Logic.flix" -> LocalResource.get("/test/flix/Test.Exp.Binary.Logic.flix"),
    "Test.Exp.Binary.Spaceship.flix" -> LocalResource.get("/test/flix/Test.Exp.Binary.Spaceship.flix"),
    "Test.Exp.Block.flix" -> LocalResource.get("/test/flix/Test.Exp.Block.flix"),
//    "Test.Exp.Char.flix" -> LocalResource.get("/test/flix/Test.Exp.Char.flix"),
    "Test.Exp.CheckedEffectCast.flix" -> LocalResource.get("/test/flix/Test.Exp.CheckedEffectCast.flix"),
    "Test.Exp.CheckedTypeCast.flix" -> LocalResource.get("/test/flix/Test.Exp.CheckedTypeCast.flix"),
//    "Test.Exp.Choose.Polymorphic.flix" -> LocalResource.get("/test/flix/Test.Exp.Choose.Polymorphic.flix"),
//    "Test.Exp.Choose.Recursive.flix" -> LocalResource.get("/test/flix/Test.Exp.Choose.Recursive.flix"),
//    "Test.Exp.Choose.Simple.flix" -> LocalResource.get("/test/flix/Test.Exp.Choose.Simple.flix"),
//    "Test.Exp.Choose.SimpleTerms.flix" -> LocalResource.get("/test/flix/Test.Exp.Choose.SimpleTerms.flix"),
//    "Test.Exp.ChooseStar.Polymorphic.flix" -> LocalResource.get("/test/flix/Test.Exp.ChooseStar.Polymorphic.flix"),
    "Test.Exp.ChooseStar.Recursive.flix" -> LocalResource.get("/test/flix/Test.Exp.ChooseStar.Recursive.flix"),
//    "Test.Exp.ChooseStar.Simple.flix" -> LocalResource.get("/test/flix/Test.Exp.ChooseStar.Simple.flix"),
    "Test.Exp.ChooseStar.SimpleTerms.flix" -> LocalResource.get("/test/flix/Test.Exp.ChooseStar.SimpleTerms.flix"),
    "Test.Exp.Concurrency.Buffered.flix" -> LocalResource.get("/test/flix/Test.Exp.Concurrency.Buffered.flix"),
//    "Test.Exp.Concurrency.Select.flix" -> LocalResource.get("/test/flix/Test.Exp.Concurrency.Select.flix"),
    "Test.Exp.Concurrency.Spawn.flix" -> LocalResource.get("/test/flix/Test.Exp.Concurrency.Spawn.flix"),
    "Test.Exp.Concurrency.Unbuffered.flix" -> LocalResource.get("/test/flix/Test.Exp.Concurrency.Unbuffered.flix"),
    "Test.Exp.Discard.flix" -> LocalResource.get("/test/flix/Test.Exp.Discard.flix"),
    "Test.Exp.Effect.2.flix" -> LocalResource.get("/test/flix/Test.Exp.Effect.2.flix"),
//    "Test.Exp.Effect.flix" -> LocalResource.get("/test/flix/Test.Exp.Effect.flix"),
    "Test.Exp.Fixpoint.Compose.flix" -> LocalResource.get("/test/flix/Test.Exp.Fixpoint.Compose.flix"),
//    "Test.Exp.Fixpoint.Constraint.flix" -> LocalResource.get("/test/flix/Test.Exp.Fixpoint.Constraint.flix"),
//    "Test.Exp.Fixpoint.Lambda.flix" -> LocalResource.get("/test/flix/Test.Exp.Fixpoint.Lambda.flix"),
//    "Test.Exp.Fixpoint.Project.flix" -> LocalResource.get("/test/flix/Test.Exp.Fixpoint.Project.flix"),
    "Test.Exp.Fixpoint.Query.flix" -> LocalResource.get("/test/flix/Test.Exp.Fixpoint.Query.flix"),
    "Test.Exp.Fixpoint.Solve.Lattice.flix" -> LocalResource.get("/test/flix/Test.Exp.Fixpoint.Solve.Lattice.flix"),
    "Test.Exp.Fixpoint.Solve.flix" -> LocalResource.get("/test/flix/Test.Exp.Fixpoint.Solve.flix"),
    "Test.Exp.Fixpoint.flix" -> LocalResource.get("/test/flix/Test.Exp.Fixpoint.flix"),
    "Test.Exp.Float32.flix" -> LocalResource.get("/test/flix/Test.Exp.Float32.flix"),
    "Test.Exp.Float64.flix" -> LocalResource.get("/test/flix/Test.Exp.Float64.flix"),
    "Test.Exp.ForEach.flix" -> LocalResource.get("/test/flix/Test.Exp.ForEach.flix"),
//    "Test.Exp.ForEachYield.flix" -> LocalResource.get("/test/flix/Test.Exp.ForEachYield.flix"),
    "Test.Exp.Force.flix" -> LocalResource.get("/test/flix/Test.Exp.Force.flix"),
//    "Test.Exp.Hole.flix" -> LocalResource.get("/test/flix/Test.Exp.Hole.flix"),
//    "Test.Exp.HoleWithExp.flix" -> LocalResource.get("/test/flix/Test.Exp.HoleWithExp.flix"),
    "Test.Exp.IfThenElse.flix" -> LocalResource.get("/test/flix/Test.Exp.IfThenElse.flix"),
//    "Test.Exp.Infix.flix" -> LocalResource.get("/test/flix/Test.Exp.Infix.flix"),
    "Test.Exp.Instanceof.flix" -> LocalResource.get("/test/flix/Test.Exp.Instanceof.flix"),
    "Test.Exp.Int16.flix" -> LocalResource.get("/test/flix/Test.Exp.Int16.flix"),
    "Test.Exp.Int32.flix" -> LocalResource.get("/test/flix/Test.Exp.Int32.flix"),
    "Test.Exp.Int64.flix" -> LocalResource.get("/test/flix/Test.Exp.Int64.flix"),
    "Test.Exp.Int8.flix" -> LocalResource.get("/test/flix/Test.Exp.Int8.flix"),
    "Test.Exp.Interpolation.flix" -> LocalResource.get("/test/flix/Test.Exp.Interpolation.flix"),
//    "Test.Exp.Jvm.GetField.flix" -> LocalResource.get("/test/flix/Test.Exp.Jvm.GetField.flix"),
//    "Test.Exp.Jvm.GetFieldDoubleNestedClass.flix" -> LocalResource.get("/test/flix/Test.Exp.Jvm.GetFieldDoubleNestedClass.flix"),
//    "Test.Exp.Jvm.GetFieldStaticInnerClass.flix" -> LocalResource.get("/test/flix/Test.Exp.Jvm.GetFieldStaticInnerClass.flix"),
//    "Test.Exp.Jvm.GetStaticField.flix" -> LocalResource.get("/test/flix/Test.Exp.Jvm.GetStaticField.flix"),
//    "Test.Exp.Jvm.GetStaticFieldStaticInnerClass.flix" -> LocalResource.get("/test/flix/Test.Exp.Jvm.GetStaticFieldStaticInnerClass.flix"),
//    "Test.Exp.Jvm.InvokeConstructor.flix" -> LocalResource.get("/test/flix/Test.Exp.Jvm.InvokeConstructor.flix"),
//    "Test.Exp.Jvm.InvokeMethod.flix" -> LocalResource.get("/test/flix/Test.Exp.Jvm.InvokeMethod.flix"),
//    "Test.Exp.Jvm.InvokeStaticMethod.flix" -> LocalResource.get("/test/flix/Test.Exp.Jvm.InvokeStaticMethod.flix"),
//    "Test.Exp.Jvm.NewObject.flix" -> LocalResource.get("/test/flix/Test.Exp.Jvm.NewObject.flix"),
//    "Test.Exp.Jvm.PutField.flix" -> LocalResource.get("/test/flix/Test.Exp.Jvm.PutField.flix"),
//    "Test.Exp.Jvm.PutStaticField.flix" -> LocalResource.get("/test/flix/Test.Exp.Jvm.PutStaticField.flix"),
//    "Test.Exp.Jvm.TryCatch.flix" -> LocalResource.get("/test/flix/Test.Exp.Jvm.TryCatch.flix"),
//    "Test.Exp.Lambda.Match.flix" -> LocalResource.get("/test/flix/Test.Exp.Lambda.Match.flix"),
    "Test.Exp.Lazy.flix" -> LocalResource.get("/test/flix/Test.Exp.Lazy.flix"),
    "Test.Exp.Let.Match.flix" -> LocalResource.get("/test/flix/Test.Exp.Let.Match.flix"),
    "Test.Exp.Let.Rec.flix" -> LocalResource.get("/test/flix/Test.Exp.Let.Rec.flix"),
    "Test.Exp.Let.flix" -> LocalResource.get("/test/flix/Test.Exp.Let.flix"),
//    "Test.Exp.List.flix" -> LocalResource.get("/test/flix/Test.Exp.List.flix"),
    "Test.Exp.Match.Bool.flix" -> LocalResource.get("/test/flix/Test.Exp.Match.Bool.flix"),
//    "Test.Exp.Match.Char.flix" -> LocalResource.get("/test/flix/Test.Exp.Match.Char.flix"),
    "Test.Exp.Match.Float32.flix" -> LocalResource.get("/test/flix/Test.Exp.Match.Float32.flix"),
    "Test.Exp.Match.Float64.flix" -> LocalResource.get("/test/flix/Test.Exp.Match.Float64.flix"),
    "Test.Exp.Match.Guard.flix" -> LocalResource.get("/test/flix/Test.Exp.Match.Guard.flix"),
    "Test.Exp.Match.Int16.flix" -> LocalResource.get("/test/flix/Test.Exp.Match.Int16.flix"),
    "Test.Exp.Match.Int32.flix" -> LocalResource.get("/test/flix/Test.Exp.Match.Int32.flix"),
    "Test.Exp.Match.Int64.flix" -> LocalResource.get("/test/flix/Test.Exp.Match.Int64.flix"),
    "Test.Exp.Match.Int8.flix" -> LocalResource.get("/test/flix/Test.Exp.Match.Int8.flix"),
    "Test.Exp.Match.List.flix" -> LocalResource.get("/test/flix/Test.Exp.Match.List.flix"),
//    "Test.Exp.Match.Record.flix" -> LocalResource.get("/test/flix/Test.Exp.Match.Record.flix"),
    "Test.Exp.Match.String.flix" -> LocalResource.get("/test/flix/Test.Exp.Match.String.flix"),
    "Test.Exp.Match.Tag.flix" -> LocalResource.get("/test/flix/Test.Exp.Match.Tag.flix"),
    "Test.Exp.Match.Unit.flix" -> LocalResource.get("/test/flix/Test.Exp.Match.Unit.flix"),
    "Test.Exp.Match.Wild.flix" -> LocalResource.get("/test/flix/Test.Exp.Match.Wild.flix"),
//    "Test.Exp.MonadicFor.flix" -> LocalResource.get("/test/flix/Test.Exp.MonadicFor.flix"),
//    "Test.Exp.New.flix" -> LocalResource.get("/test/flix/Test.Exp.New.flix"),
    "Test.Exp.Null.flix" -> LocalResource.get("/test/flix/Test.Exp.Null.flix"),
    "Test.Exp.ParYield.flix" -> LocalResource.get("/test/flix/Test.Exp.ParYield.flix"),
//    "Test.Exp.Record.Extend.flix" -> LocalResource.get("/test/flix/Test.Exp.Record.Extend.flix"),
//    "Test.Exp.Record.Literal.flix" -> LocalResource.get("/test/flix/Test.Exp.Record.Literal.flix"),
//    "Test.Exp.Record.Multiple.flix" -> LocalResource.get("/test/flix/Test.Exp.Record.Multiple.flix"),
//    "Test.Exp.Record.Polymorphism.flix" -> LocalResource.get("/test/flix/Test.Exp.Record.Polymorphism.flix"),
//    "Test.Exp.Record.Restrict.flix" -> LocalResource.get("/test/flix/Test.Exp.Record.Restrict.flix"),
//    "Test.Exp.Record.Select.flix" -> LocalResource.get("/test/flix/Test.Exp.Record.Select.flix"),
//    "Test.Exp.Record.Update.flix" -> LocalResource.get("/test/flix/Test.Exp.Record.Update.flix"),
    "Test.Exp.Ref.Assign.flix" -> LocalResource.get("/test/flix/Test.Exp.Ref.Assign.flix"),
//    "Test.Exp.Ref.Deref.flix" -> LocalResource.get("/test/flix/Test.Exp.Ref.Deref.flix"),
    "Test.Exp.Ref.Precedence.flix" -> LocalResource.get("/test/flix/Test.Exp.Ref.Precedence.flix"),
    "Test.Exp.Ref.Ref.flix" -> LocalResource.get("/test/flix/Test.Exp.Ref.Ref.flix"),
//    "Test.Exp.Regex.flix" -> LocalResource.get("/test/flix/Test.Exp.Regex.flix"),
    "Test.Exp.Regions.flix" -> LocalResource.get("/test/flix/Test.Exp.Regions.flix"),
    "Test.Exp.Scope.flix" -> LocalResource.get("/test/flix/Test.Exp.Scope.flix"),
    "Test.Exp.Stm.flix" -> LocalResource.get("/test/flix/Test.Exp.Stm.flix"),
//    "Test.Exp.String.flix" -> LocalResource.get("/test/flix/Test.Exp.String.flix"),
    "Test.Exp.Tag.Lambda.flix" -> LocalResource.get("/test/flix/Test.Exp.Tag.Lambda.flix"),
//    "Test.Exp.Tag.flix" -> LocalResource.get("/test/flix/Test.Exp.Tag.flix"),
    "Test.Exp.TryWith.flix" -> LocalResource.get("/test/flix/Test.Exp.TryWith.flix"),
//    "Test.Exp.Tuple.flix" -> LocalResource.get("/test/flix/Test.Exp.Tuple.flix"),
//    "Test.Exp.TypeMatch.flix" -> LocalResource.get("/test/flix/Test.Exp.TypeMatch.flix"),
//    "Test.Exp.Unary.Arithmetic.flix" -> LocalResource.get("/test/flix/Test.Exp.Unary.Arithmetic.flix"),
//    "Test.Exp.Unary.Bitwise.flix" -> LocalResource.get("/test/flix/Test.Exp.Unary.Bitwise.flix"),
    "Test.Exp.Unary.Logic.flix" -> LocalResource.get("/test/flix/Test.Exp.Unary.Logic.flix"),
    "Test.Exp.UncheckedEffectCast.flix" -> LocalResource.get("/test/flix/Test.Exp.UncheckedEffectCast.flix"),
    "Test.Exp.UncheckedMaskingCast.flix" -> LocalResource.get("/test/flix/Test.Exp.UncheckedMaskingCast.flix"),
    "Test.Exp.UncheckedTypeCast.flix" -> LocalResource.get("/test/flix/Test.Exp.UncheckedTypeCast.flix"),
    "Test.Exp.Unit.flix" -> LocalResource.get("/test/flix/Test.Exp.Unit.flix"),
    "Test.Handler.Ask.AtomicInteger.flix" -> LocalResource.get("/test/flix/Test.Handler.Ask.AtomicInteger.flix"),
    "Test.Handler.Ask.Bool.flix" -> LocalResource.get("/test/flix/Test.Handler.Ask.Bool.flix"),
    "Test.Handler.Ask.Char.flix" -> LocalResource.get("/test/flix/Test.Handler.Ask.Char.flix"),
    "Test.Handler.Ask.Enum.flix" -> LocalResource.get("/test/flix/Test.Handler.Ask.Enum.flix"),
    "Test.Handler.Ask.Float32.flix" -> LocalResource.get("/test/flix/Test.Handler.Ask.Float32.flix"),
    "Test.Handler.Ask.Float64.flix" -> LocalResource.get("/test/flix/Test.Handler.Ask.Float64.flix"),
    "Test.Handler.Ask.Int16.flix" -> LocalResource.get("/test/flix/Test.Handler.Ask.Int16.flix"),
    "Test.Handler.Ask.Int32.flix" -> LocalResource.get("/test/flix/Test.Handler.Ask.Int32.flix"),
    "Test.Handler.Ask.Int64.flix" -> LocalResource.get("/test/flix/Test.Handler.Ask.Int64.flix"),
    "Test.Handler.Ask.Int8.flix" -> LocalResource.get("/test/flix/Test.Handler.Ask.Int8.flix"),
    "Test.Handler.Ask.String.flix" -> LocalResource.get("/test/flix/Test.Handler.Ask.String.flix"),
    "Test.Handler.Ask.Unit.flix" -> LocalResource.get("/test/flix/Test.Handler.Ask.Unit.flix"),
    "Test.Handler.Gen.AtomicInteger.flix" -> LocalResource.get("/test/flix/Test.Handler.Gen.AtomicInteger.flix"),
    "Test.Handler.Gen.Bool.flix" -> LocalResource.get("/test/flix/Test.Handler.Gen.Bool.flix"),
    "Test.Handler.Gen.Char.flix" -> LocalResource.get("/test/flix/Test.Handler.Gen.Char.flix"),
    "Test.Handler.Gen.Enum.flix" -> LocalResource.get("/test/flix/Test.Handler.Gen.Enum.flix"),
    "Test.Handler.Gen.Float32.flix" -> LocalResource.get("/test/flix/Test.Handler.Gen.Float32.flix"),
    "Test.Handler.Gen.Float64.flix" -> LocalResource.get("/test/flix/Test.Handler.Gen.Float64.flix"),
    "Test.Handler.Gen.Int16.flix" -> LocalResource.get("/test/flix/Test.Handler.Gen.Int16.flix"),
    "Test.Handler.Gen.Int32.flix" -> LocalResource.get("/test/flix/Test.Handler.Gen.Int32.flix"),
    "Test.Handler.Gen.Int64.flix" -> LocalResource.get("/test/flix/Test.Handler.Gen.Int64.flix"),
    "Test.Handler.Gen.Int8.flix" -> LocalResource.get("/test/flix/Test.Handler.Gen.Int8.flix"),
    "Test.Handler.Gen.String.flix" -> LocalResource.get("/test/flix/Test.Handler.Gen.String.flix"),
    "Test.Handler.Gen.Unit.flix" -> LocalResource.get("/test/flix/Test.Handler.Gen.Unit.flix"),
    "Test.Handler.MultiShot.AtomicInteger.flix" -> LocalResource.get("/test/flix/Test.Handler.MultiShot.AtomicInteger.flix"),
    "Test.Handler.MultiShot.Bool.flix" -> LocalResource.get("/test/flix/Test.Handler.MultiShot.Bool.flix"),
    "Test.Handler.MultiShot.Char.flix" -> LocalResource.get("/test/flix/Test.Handler.MultiShot.Char.flix"),
    "Test.Handler.MultiShot.Enum.flix" -> LocalResource.get("/test/flix/Test.Handler.MultiShot.Enum.flix"),
    "Test.Handler.MultiShot.Float32.flix" -> LocalResource.get("/test/flix/Test.Handler.MultiShot.Float32.flix"),
    "Test.Handler.MultiShot.Float64.flix" -> LocalResource.get("/test/flix/Test.Handler.MultiShot.Float64.flix"),
    "Test.Handler.MultiShot.Int16.flix" -> LocalResource.get("/test/flix/Test.Handler.MultiShot.Int16.flix"),
    "Test.Handler.MultiShot.Int32.flix" -> LocalResource.get("/test/flix/Test.Handler.MultiShot.Int32.flix"),
    "Test.Handler.MultiShot.Int64.flix" -> LocalResource.get("/test/flix/Test.Handler.MultiShot.Int64.flix"),
    "Test.Handler.MultiShot.Int8.flix" -> LocalResource.get("/test/flix/Test.Handler.MultiShot.Int8.flix"),
    "Test.Handler.MultiShot.String.flix" -> LocalResource.get("/test/flix/Test.Handler.MultiShot.String.flix"),
    "Test.Handler.MultiShot.Unit.flix" -> LocalResource.get("/test/flix/Test.Handler.MultiShot.Unit.flix"),
    "Test.Handler.NewObject.flix" -> LocalResource.get("/test/flix/Test.Handler.NewObject.flix"),
    "Test.Handler.OneShot.AtomicInteger.flix" -> LocalResource.get("/test/flix/Test.Handler.OneShot.AtomicInteger.flix"),
    "Test.Handler.OneShot.Bool.flix" -> LocalResource.get("/test/flix/Test.Handler.OneShot.Bool.flix"),
    "Test.Handler.OneShot.Char.flix" -> LocalResource.get("/test/flix/Test.Handler.OneShot.Char.flix"),
    "Test.Handler.OneShot.Enum.flix" -> LocalResource.get("/test/flix/Test.Handler.OneShot.Enum.flix"),
    "Test.Handler.OneShot.Float32.flix" -> LocalResource.get("/test/flix/Test.Handler.OneShot.Float32.flix"),
    "Test.Handler.OneShot.Float64.flix" -> LocalResource.get("/test/flix/Test.Handler.OneShot.Float64.flix"),
    "Test.Handler.OneShot.Int16.flix" -> LocalResource.get("/test/flix/Test.Handler.OneShot.Int16.flix"),
    "Test.Handler.OneShot.Int32.flix" -> LocalResource.get("/test/flix/Test.Handler.OneShot.Int32.flix"),
    "Test.Handler.OneShot.Int64.flix" -> LocalResource.get("/test/flix/Test.Handler.OneShot.Int64.flix"),
    "Test.Handler.OneShot.Int8.flix" -> LocalResource.get("/test/flix/Test.Handler.OneShot.Int8.flix"),
    "Test.Handler.OneShot.String.flix" -> LocalResource.get("/test/flix/Test.Handler.OneShot.String.flix"),
    "Test.Handler.OneShot.Unit.flix" -> LocalResource.get("/test/flix/Test.Handler.OneShot.Unit.flix"),
    "Test.Handler.Spawn.flix" -> LocalResource.get("/test/flix/Test.Handler.Spawn.flix"),
    "Test.Handler.ZeroShot.AtomicInteger.flix" -> LocalResource.get("/test/flix/Test.Handler.ZeroShot.AtomicInteger.flix"),
    "Test.Handler.ZeroShot.Bool.flix" -> LocalResource.get("/test/flix/Test.Handler.ZeroShot.Bool.flix"),
    "Test.Handler.ZeroShot.Char.flix" -> LocalResource.get("/test/flix/Test.Handler.ZeroShot.Char.flix"),
    "Test.Handler.ZeroShot.Enum.flix" -> LocalResource.get("/test/flix/Test.Handler.ZeroShot.Enum.flix"),
    "Test.Handler.ZeroShot.Float32.flix" -> LocalResource.get("/test/flix/Test.Handler.ZeroShot.Float32.flix"),
    "Test.Handler.ZeroShot.Float64.flix" -> LocalResource.get("/test/flix/Test.Handler.ZeroShot.Float64.flix"),
    "Test.Handler.ZeroShot.Int16.flix" -> LocalResource.get("/test/flix/Test.Handler.ZeroShot.Int16.flix"),
    "Test.Handler.ZeroShot.Int32.flix" -> LocalResource.get("/test/flix/Test.Handler.ZeroShot.Int32.flix"),
    "Test.Handler.ZeroShot.Int64.flix" -> LocalResource.get("/test/flix/Test.Handler.ZeroShot.Int64.flix"),
    "Test.Handler.ZeroShot.Int8.flix" -> LocalResource.get("/test/flix/Test.Handler.ZeroShot.Int8.flix"),
    "Test.Handler.ZeroShot.Recursive.flix" -> LocalResource.get("/test/flix/Test.Handler.ZeroShot.Recursive.flix"),
    "Test.Handler.ZeroShot.String.flix" -> LocalResource.get("/test/flix/Test.Handler.ZeroShot.String.flix"),
    "Test.Handler.ZeroShot.Unit.flix" -> LocalResource.get("/test/flix/Test.Handler.ZeroShot.Unit.flix"),
    "Test.Import.flix" -> LocalResource.get("/test/flix/Test.Import.flix"),
//    "Test.Integ.Class.Schema.flix" -> LocalResource.get("/test/flix/Test.Integ.Class.Schema.flix"),
    "Test.Integ.Enum.TypeAlias.flix" -> LocalResource.get("/test/flix/Test.Integ.Enum.TypeAlias.flix"),
    "Test.Integ.Fixpoint.TypeAlias.flix" -> LocalResource.get("/test/flix/Test.Integ.Fixpoint.TypeAlias.flix"),
    "Test.Integ.Trait.Schema.flix" -> LocalResource.get("/test/flix/Test.Integ.Trait.Schema.flix"),
//    "Test.Java.Function.flix" -> LocalResource.get("/test/flix/Test.Java.Function.flix"),
//    "Test.Kind.Class.flix" -> LocalResource.get("/test/flix/Test.Kind.Class.flix"),
    "Test.Kind.Def.flix" -> LocalResource.get("/test/flix/Test.Kind.Def.flix"),
//    "Test.Kind.Enum.flix" -> LocalResource.get("/test/flix/Test.Kind.Enum.flix"),
    "Test.Kind.Instance.flix" -> LocalResource.get("/test/flix/Test.Kind.Instance.flix"),
//    "Test.Kind.Trait.flix" -> LocalResource.get("/test/flix/Test.Kind.Trait.flix"),
    "Test.Kind.TypeAlias.flix" -> LocalResource.get("/test/flix/Test.Kind.TypeAlias.flix"),
    "Test.Predicate.Functional.flix" -> LocalResource.get("/test/flix/Test.Predicate.Functional.flix"),
//    "Test.Predicate.Guard.flix" -> LocalResource.get("/test/flix/Test.Predicate.Guard.flix"),
    "Test.Predicate.Nullary.flix" -> LocalResource.get("/test/flix/Test.Predicate.Nullary.flix"),
    "Test.Stratification.flix" -> LocalResource.get("/test/flix/Test.Stratification.flix"),
    "Test.Term.Apply.flix" -> LocalResource.get("/test/flix/Test.Term.Apply.flix"),
    "Test.Term.Lit.List.flix" -> LocalResource.get("/test/flix/Test.Term.Lit.List.flix"),
    "Test.Term.Lit.Option.flix" -> LocalResource.get("/test/flix/Test.Term.Lit.Option.flix"),
    "Test.Term.Lit.Result.flix" -> LocalResource.get("/test/flix/Test.Term.Lit.Result.flix"),
    "Test.Term.Lit.Set.flix" -> LocalResource.get("/test/flix/Test.Term.Lit.Set.flix"),
//    "Test.Term.Lit.flix" -> LocalResource.get("/test/flix/Test.Term.Lit.flix"),
    "Test.Term.Var.CapturedVar.flix" -> LocalResource.get("/test/flix/Test.Term.Var.CapturedVar.flix"),
    "Test.Term.Var.QuantVar.flix" -> LocalResource.get("/test/flix/Test.Term.Var.QuantVar.flix"),
    "Test.Term.Var.WildVar.flix" -> LocalResource.get("/test/flix/Test.Term.Var.WildVar.flix"),
    "Test.Test.flix" -> LocalResource.get("/test/flix/Test.Test.flix"),
    "Test.Type.Bool.flix" -> LocalResource.get("/test/flix/Test.Type.Bool.flix"),
    "Test.Type.Void.flix" -> LocalResource.get("/test/flix/Test.Type.Void.flix"),
//    "Test.Unused.Def.flix" -> LocalResource.get("/test/flix/Test.Unused.Def.flix"),
    "Test.Unused.Tag.flix" -> LocalResource.get("/test/flix/Test.Unused.Tag.flix"),
//    "Test.Unused.Var.flix" -> LocalResource.get("/test/flix/Test.Unused.Var.flix"),
//    "Test.Use.Def.flix" -> LocalResource.get("/test/flix/Test.Use.Def.flix"),
//    "Test.Use.Sig.flix" -> LocalResource.get("/test/flix/Test.Use.Sig.flix"),
//    "Test.Use.Tag.flix" -> LocalResource.get("/test/flix/Test.Use.Tag.flix"),
    "Test.Use.Type.flix" -> LocalResource.get("/test/flix/Test.Use.Type.flix"),
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

      // Debugging pipeline
      afterParser2 <- Parser2.runWithFallback(afterReader, afterLexer, entryPoint, changeSet)
      afterDesugar = Desugar.run(afterParser2, cachedDesugarAst, changeSet)

      // TODO: Use these instead
      //      afterParser2 <- Parser2.run(afterLexer)
      //      afterWeeder2 <- Weeder2.run(afterReader, entryPoint, afterParser2)
      //      afterDesugar = Desugar.run(afterWeeder2, cachedDesugarAst, changeSet)

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
    cachedMonoDefsAst = MonoDefs.run(cachedTreeShaker1Ast)
    cachedMonoTypesAst = MonoTypes.run(cachedMonoDefsAst)
    cachedSimplifierAst = Simplifier.run(cachedMonoTypesAst)
    cachedClosureConvAst = ClosureConv.run(cachedSimplifierAst)
    cachedLambdaLiftAst = LambdaLift.run(cachedClosureConvAst)
    cachedTailrecAst = Tailrec.run(cachedLambdaLiftAst)
    cachedOptimizerAst = Optimizer.run(cachedTailrecAst)
    cachedTreeShaker2Ast = TreeShaker2.run(cachedOptimizerAst)
    cachedEffectBinderAst = EffectBinder.run(cachedTreeShaker2Ast)
    cachedEraserAst = Eraser.run(cachedEffectBinderAst)
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
