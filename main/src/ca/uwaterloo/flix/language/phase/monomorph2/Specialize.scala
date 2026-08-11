/*
 * Copyright 2026 Simon Lykke Andersen
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

package ca.uwaterloo.flix.language.phase.monomorph2

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.TypedAst.Instance
import ca.uwaterloo.flix.language.ast.{MonoAst, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.util.InternalCompilerException

import java.util.concurrent.ConcurrentLinkedQueue
import scala.jdk.CollectionConverters.*

/**
  * Solution-driven specialization: uses the solver's solution to specialize every def/enum/
  * struct/restrictable-enum in a single parallel pass.
  *
  * `run` builds the `SharedContext` lookup tables; [[SpecializeAndLower.visitDef]] does the actual
  * per-def specialize+lower walk, resolving each call/tag/struct site through them.
  */
object Specialize {

  /**
    * The mutable data used throughout specialization.
    *
    * This class is thread-safe.
    */
  private[monomorph2] class SharedContext(
    val defTable: Map[(Symbol.DefnSym, Type), Symbol.DefnSym],
    val allDefs: Map[Symbol.DefnSym, TypedAst.Def],
    val enumTable: Map[(Symbol.EnumSym, List[Type]), Symbol.EnumSym],
    val structTable: Map[(Symbol.StructSym, List[Type]), Symbol.StructSym],
    val restrictableEnumTable: Map[(Symbol.RestrictableEnumSym, List[Type]), Symbol.EnumSym],
    val instances: Map[(Symbol.TraitSym, TypeConstructor), Instance]
  ) {
    private val specializedDefsQueue: ConcurrentLinkedQueue[(Symbol.DefnSym, MonoAst.Def)] = new ConcurrentLinkedQueue()

    /** Records `defn` under its fresh specialized `sym`. */
    def addSpecializedDef(sym: Symbol.DefnSym, defn: MonoAst.Def): Unit =
      specializedDefsQueue.add((sym, defn))

    /** Returns all specialized defs recorded so far. */
    def specializedDefs: Map[Symbol.DefnSym, MonoAst.Def] =
      specializedDefsQueue.asScala.toMap

    /** Diagnostic only, for MonomorphBench's Xmonobench table. */
    private val defCategoryCountsQueue: ConcurrentLinkedQueue[String] = new ConcurrentLinkedQueue()

    /** Increments the count for `category` (one of "regularDefs"/"instanceDefs"/"defaultSigImpls"). */
    def incrementDefCategory(category: String): Unit =
      defCategoryCountsQueue.add(category)

    /** Returns the per-category specialized-def counts. */
    def defCategoryCounts: Map[String, Int] =
      defCategoryCountsQueue.asScala.groupMapReduce(identity)(_ => 1)(_ + _)
  }

  /**
    * Returns the sym to use for a call to `sym` at ground arrow type `groundArrowTpe`.
    */
  private[monomorph2] def lookupSym(sym: Symbol.DefnSym, groundArrowTpe: Type)
                       (implicit sctx: SharedContext): Symbol.DefnSym = {
    val defn = sctx.allDefs.getOrElse(sym, throw InternalCompilerException(s"lookupSym: sym not in allDefs: $sym", sym.loc))
    // instance/default-sig defs can have empty spec.tparams but still need specialization
    // therefore we first look it up in `sctx.defTable`
    sctx.defTable.get((sym, groundArrowTpe)) match {
      case Some(specializedSym) => specializedSym
      case None =>
        if (defn.spec.tparams.isEmpty) {
          defn.sym
        } else {
          throw InternalCompilerException(
            s"Solver gap: no specialization for $sym at type $groundArrowTpe. " +
              "Extend the constraint generator to cover this call site.", sym.loc)
        }
    }
  }

  /**
    * Returns the case sym to use for a `Tag`/`Pattern.Tag` at ground enum type `groundEnumTpe`.
    */
  private[monomorph2] def lookupCaseSym(caseSym: Symbol.CaseSym, groundEnumTpe: Type)(implicit sctx: SharedContext): Symbol.CaseSym = {
    val argTypes = groundEnumTpe.typeArguments
    sctx.enumTable.get((caseSym.enumSym, argTypes)) match {
      case Some(freshEnumSym) => new Symbol.CaseSym(freshEnumSym, caseSym.name, caseSym.ordinal, caseSym.loc)
      case None  =>
        if (argTypes.isEmpty) {
          caseSym
        } else {
          throw InternalCompilerException(
            s"Solver gap: no enum specialization for ${caseSym.enumSym} at $argTypes. " +
              "Extend the constraint generator to cover this call site.", caseSym.loc)
        }
    }
  }

  /**
    * Returns the (regular) case sym for a restrictable tag/pattern at ground restrictable-enum
    * type `groundRestrictableEnumTpe`.
    */
  private[monomorph2] def lookupRestrictableCaseSym(caseSym: Symbol.RestrictableCaseSym, groundRestrictableEnumTpe: Type)(implicit sctx: SharedContext): Symbol.CaseSym = {
    val argTypes = groundRestrictableEnumTpe.typeArguments
    sctx.restrictableEnumTable.get((caseSym.enumSym, argTypes)) match {
      case Some(freshEnumSym) => new Symbol.CaseSym(freshEnumSym, caseSym.name, -1, caseSym.loc)
      case None =>
        throw InternalCompilerException(
          s"Solver gap: no restrictable enum specialization for ${caseSym.enumSym} at $argTypes. " +
            "Extend the constraint generator to cover this call site.", caseSym.loc)
    }
  }

  /**
    * Returns the struct sym for a `StructNew`/`StructGet`/`StructPut` at ground type
    * `groundStructTpe`.
    */
  private[monomorph2] def lookupStructSym(sym: Symbol.StructSym, groundStructTpe: Type)(implicit sctx: SharedContext): Symbol.StructSym = {
    val argTypes = groundStructTpe.typeArguments
    sctx.structTable.get((sym, argTypes)) match {
      case Some(freshStructSym) => freshStructSym
      case None =>
        if (argTypes.isEmpty) {
          sym
        } else {
          throw InternalCompilerException(
            s"Solver gap: no struct specialization for $sym at $argTypes. " +
              "Extend the constraint generator to cover this call site.", groundStructTpe.loc)
        }
    }
  }

  /** Specializes `root` per `solution`, the constraint solver's output. */
  def run(root: TypedAst.Root, solution: Solution)(implicit flix: Flix): MonoAst.Root = ???
}
