/*
 *  Copyright 2026 Magnus Madsen
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.language.ast.shared.RegionScope
import ca.uwaterloo.flix.language.ast.shared.SymUse.AssocTypeSymUse
import ca.uwaterloo.flix.language.ast.{Kind, RigidityEnv, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.phase.typer.TypeConstraint
import ca.uwaterloo.flix.util.collection.Nel

import scala.collection.mutable

private object AtomBimap {

  /**
    * Returns an [[AtomBimap]] numbering the [[EffAtom]]s of `eqs` using [[EffAtom.collectAtoms]].
    * The map also records the type arguments of each polymorphic effect constructor so that the
    * effect applications can be reconstructed after set unification.
    *
    * The atoms are sorted before numbering: the assignment must be deterministic across
    * runs since it determines the solving order in
    * [[ca.uwaterloo.flix.language.phase.unification.set.SetUnification]].
    */
  def fromConstraints(eqs: List[TypeConstraint.Equality])(implicit scope: RegionScope, renv: RigidityEnv): AtomBimap = {
    // The distinct effect atoms that occur in the equations.
    val buf = mutable.HashSet.empty[EffAtom]

    // The arguments used to reconstruct each polymorphic effect after set unification.
    val effectArgs = mutable.Map.empty[Symbol.EffSym, Nel[Type]]
    for (eq <- eqs) {
      // The constraint solver has canonicalized every application; disagreement is an internal error.
      EffAtom.collectAtoms(eq.tpe1, buf, effectArgs, strict = true)
      EffAtom.collectAtoms(eq.tpe2, buf, effectArgs, strict = true)
    }
    fromAtoms(buf, effectArgs.toMap)
  }

  /**
    * Returns an [[AtomBimap]] numbering the [[EffAtom]]s of `tpe` using [[EffAtom.collectAtoms]].
    * The map also records the type arguments of each polymorphic effect constructor so that the
    * effect applications can be reconstructed after simplification.
    */
  def fromType(tpe: Type)(implicit scope: RegionScope, renv: RigidityEnv): AtomBimap = {
    val buf = mutable.HashSet.empty[EffAtom]
    val effectArgs = mutable.Map.empty[Symbol.EffSym, Nel[Type]]
    // A type reconstructed from an ill-typed definition may disagree with itself; the first arguments win.
    EffAtom.collectAtoms(tpe, buf, effectArgs, strict = false)
    fromAtoms(buf, effectArgs.toMap)
  }

  /**
    * Returns an [[AtomBimap]] numbering `atoms` from `0` to `n - 1` in sorted order.
    * `effectArgs` maps each polymorphic effect constructor to its non-empty type argument list.
    */
  private def fromAtoms(atoms: mutable.HashSet[EffAtom], effectArgs: Map[Symbol.EffSym, Nel[Type]]): AtomBimap = {
    val arr = atoms.toArray
    java.util.Arrays.sort(arr, implicitly[Ordering[EffAtom]])
    var forward = Map.empty[EffAtom, Int]
    var i = 0
    while (i < arr.length) {
      forward = forward.updated(arr(i), i)
      i += 1
    }
    new AtomBimap(forward, arr, effectArgs)
  }
}

/**
  * A bidirectional mapping between [[EffAtom]]s and dense indices `0..n-1`.
  *
  * Performance: The forward map is hash-based and the backward map is an array, avoiding
  * the ordered-comparison cost of sorted maps on the hot path of effect unification. The
  * index assignment itself must be deterministic; it is always derived from atoms in
  * sorted order.
  */
private final class AtomBimap(forward: Map[EffAtom, Int], backward: Array[EffAtom], effectArgs: Map[Symbol.EffSym, Nel[Type]]) {

  /** Returns the index of `a`, or -1 if absent (allocation-free). */
  def getForwardIndex(a: EffAtom): Int = forward.getOrElse(a, -1)

  /**
    * Optionally returns the atom at index `i`.
    *
    * Callers probe indices outside `0..n-1` (e.g. slack variables introduced during
    * solving) and rely on `None` for those — the bounds check is load-bearing.
    */
  def getBackward(i: Int): Option[EffAtom] =
    if (i >= 0 && i < backward.length) Some(backward(i)) else None

  /** Returns the [[Type]] represented by `atom` with location `loc`. */
  def toType(atom: EffAtom, loc: SourceLocation): Type = atom match {
    case EffAtom.Eff(sym) =>
      effectArgs.get(sym) match {
        case None => Type.Cst(TypeConstructor.Effect(sym, Kind.Eff), loc)
        case Some(args) =>
          val ts = args.toList
          val kind = Kind.mkArrowTo(ts.map(_.kind), Kind.Eff)
          Type.mkApply(Type.Cst(TypeConstructor.Effect(sym, kind), loc), ts, loc)
      }
    case EffAtom.Region(sym) => Type.Cst(TypeConstructor.Region(sym), loc)
    case EffAtom.VarRigid(sym) => Type.Var(sym, loc)
    case EffAtom.VarFlex(sym) => Type.Var(sym, loc)
    case EffAtom.Assoc(sym, arg0) =>
      Type.AssocType(AssocTypeSymUse(sym, loc), toType(arg0, loc), Kind.Eff, loc)
    case EffAtom.Error(id) => Type.Cst(TypeConstructor.Error(id, Kind.Eff), loc)
  }
}
