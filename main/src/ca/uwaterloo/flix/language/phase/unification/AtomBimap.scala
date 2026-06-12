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
import ca.uwaterloo.flix.language.ast.{RigidityEnv, Type}
import ca.uwaterloo.flix.language.phase.typer.TypeConstraint
import ca.uwaterloo.flix.language.phase.unification.EffUnification3.Atom

import scala.collection.mutable

private object AtomBimap {

  /**
    * Returns an [[AtomBimap]] numbering the [[Atom]]s of `eqs` using [[Atom.collectAtoms]].
    *
    * The atoms are sorted before numbering: the assignment must be deterministic across
    * runs since it determines the solving order in
    * [[ca.uwaterloo.flix.language.phase.unification.set.SetUnification]].
    */
  def fromConstraints(eqs: List[TypeConstraint.Equality])(implicit scope: RegionScope, renv: RigidityEnv): AtomBimap = {
    val buf = mutable.HashSet.empty[Atom]
    for (eq <- eqs) {
      Atom.collectAtoms(eq.tpe1, buf)
      Atom.collectAtoms(eq.tpe2, buf)
    }
    fromAtoms(buf)
  }

  /** Returns an [[AtomBimap]] numbering the [[Atom]]s of `tpe` using [[Atom.collectAtoms]]. */
  def fromType(tpe: Type)(implicit scope: RegionScope, renv: RigidityEnv): AtomBimap = {
    val buf = mutable.HashSet.empty[Atom]
    Atom.collectAtoms(tpe, buf)
    fromAtoms(buf)
  }

  /** Returns an [[AtomBimap]] numbering the given atoms `0..n-1` in sorted order. */
  private def fromAtoms(atoms: mutable.HashSet[Atom]): AtomBimap = {
    val arr = atoms.toArray
    java.util.Arrays.sort(arr, implicitly[Ordering[Atom]])
    var forward = Map.empty[Atom, Int]
    var i = 0
    while (i < arr.length) {
      forward = forward.updated(arr(i), i)
      i += 1
    }
    new AtomBimap(forward, arr)
  }
}

/**
  * A bidirectional mapping between [[Atom]]s and dense indices `0..n-1`.
  *
  * Performance: The forward map is hash-based and the backward map is an array, avoiding
  * the ordered-comparison cost of sorted maps on the hot path of effect unification. The
  * index assignment itself must be deterministic; it is always derived from atoms in
  * sorted order.
  */
private final class AtomBimap(forward: Map[Atom, Int], backward: Array[Atom]) {

  /** Returns the index of `a`, or -1 if absent (allocation-free). */
  def getForwardIndex(a: Atom): Int = forward.getOrElse(a, -1)

  /**
    * Optionally returns the atom at index `i`.
    *
    * Callers probe indices outside `0..n-1` (e.g. slack variables introduced during
    * solving) and rely on `None` for those — the bounds check is load-bearing.
    */
  def getBackward(i: Int): Option[Atom] =
    if (i >= 0 && i < backward.length) Some(backward(i)) else None
}
