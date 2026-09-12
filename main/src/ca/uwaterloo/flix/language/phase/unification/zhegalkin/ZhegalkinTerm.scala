/*
 * Copyright 2024 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.phase.unification.zhegalkin

import ca.uwaterloo.flix.language.phase.unification.shared.BoolLattice

import scala.collection.immutable.SortedSet

/** Represents a Zhegalkin term: c ∩ x1 ∩ x2 ∩ ... ∩ xn */
case class ZhegalkinTerm[T](cst: T, vars: SortedSet[ZhegalkinVar]) {

  /**
    * Returns the free (i.e. flexible) variables in `this` Zhegalkin term.
    */
  def freeVars: SortedSet[ZhegalkinVar] = vars.filter(x => x.flexible)

  /**
    * Maps the given function `f` over the free variables in `this` Zhegalkin term.
    *
    * {{{
    *   map(f, c ∩ x1 ∩ x2 ∩ ... ∩ xn) = c ∩ map(f, x1) ∩ map(f, x2) ∩ ... ∩ map(f, xn)
    * }}}
    *
    */
  def map(f: Int => ZhegalkinExpr[T])(implicit alg: ZhegalkinAlgebra[T], lat: BoolLattice[T]): ZhegalkinExpr[T] = {
    vars.foldLeft(ZhegalkinExpr.mkZhegalkinExpr(cst, Nil)) {
      case (acc, x) =>
        val newX: ZhegalkinExpr[T] = if (x.flexible) f(x.id) else ZhegalkinExpr.mkVar(x)
        ZhegalkinExpr.mkInter(newX, acc)
    }
  }

  /** Returns a human-readable string representation of `this` Zhegalkin term. Must only be used for debugging. */
  override def toString: String =
    if (vars.isEmpty)
      cst.toString
    else
      s"$cst ∩ ${vars.mkString(" ∩ ")}"

}
