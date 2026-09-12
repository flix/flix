/*
 * Copyright 2024 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.phase.unification.zhegalkin

import ca.uwaterloo.flix.language.phase.unification.shared.{BoolAlg, BoolLattice, BoolSubstitution}

import scala.collection.immutable.SortedSet

class ZhegalkinAlgebra[T](lat: BoolLattice[T]) extends BoolAlg[ZhegalkinExpr[T]] {

  /** A Zhegalkin expression that represents the empty set, i.e. the zero element of the algebra. */
  val zero: ZhegalkinExpr[T] = ZhegalkinExpr(lat.Bot, Nil)

  /** A Zhegalkin expression that represents the universe, i.e. the one element of the algebra. */
  val one: ZhegalkinExpr[T] = ZhegalkinExpr(lat.Top, Nil)

  /** Zhegalkin Cache. */
  val Cache: ZhegalkinCache[T] = new ZhegalkinCache[T]

  override def isEquivBot(f: ZhegalkinExpr[T]): Boolean = ZhegalkinExpr.isEmpty(f)(this)

  override def mkBot: ZhegalkinExpr[T] = zero

  override def mkTop: ZhegalkinExpr[T] = one

  override def mkCst(id: Int): ZhegalkinExpr[T] = ZhegalkinExpr.mkVar(ZhegalkinVar(id, flexible = false))(lat)

  override def mkVar(id: Int): ZhegalkinExpr[T] = ZhegalkinExpr.mkVar(ZhegalkinVar(id, flexible = true))(lat)

  override def mkNot(f: ZhegalkinExpr[T]): ZhegalkinExpr[T] = ZhegalkinExpr.mkCompl(f)(this, lat)

  override def mkOr(f1: ZhegalkinExpr[T], f2: ZhegalkinExpr[T]): ZhegalkinExpr[T] = ZhegalkinExpr.mkUnion(f1, f2)(this, lat)

  override def mkAnd(f1: ZhegalkinExpr[T], f2: ZhegalkinExpr[T]): ZhegalkinExpr[T] = ZhegalkinExpr.mkInter(f1, f2)(this, lat)

  // Performance: We must override the default implementation of `mkXor` to increase performance.
  override def mkXor(f1: ZhegalkinExpr[T], f2: ZhegalkinExpr[T]): ZhegalkinExpr[T] = ZhegalkinExpr.mkXor(f1, f2)(this, lat)

  override def freeVars(f: ZhegalkinExpr[T]): SortedSet[Int] = f.freeVars.map(_.id)

  override def map(f: ZhegalkinExpr[T])(fn: Int => ZhegalkinExpr[T]): ZhegalkinExpr[T] = f.map(fn)(this, lat)

  override def lookupOrComputeSVE(q: ZhegalkinExpr[T], sve: ZhegalkinExpr[T] => BoolSubstitution[ZhegalkinExpr[T]]): BoolSubstitution[ZhegalkinExpr[T]] = {
    Cache.lookupOrComputeSVE(q, sve)
  }

}
