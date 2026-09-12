/*
 * Copyright 2022 Matthew Lutze
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.language.ast.shared.RegionScope

import scala.collection.immutable.SortedSet


object RigidityEnv {
  /**
    * The empty rigidity environment.
    */
  val empty: RigidityEnv = RigidityEnv(SortedSet.empty)

  /**
    * Returns the rigidity environment where only the given variables are marked rigid.
    */
  def ofRigidVars(tvars: Iterable[Symbol.KindedTypeVarSym]): RigidityEnv = RigidityEnv(tvars.to(SortedSet))
}

/**
  * Environment tracking the rigidity of type variables.
  *
  * `s` holds the set of rigid variable symbols.
  * All variables not in `s` are considered flexible.
  */
case class RigidityEnv(s: SortedSet[Symbol.KindedTypeVarSym]) {

  /**
    * Returns the rigidity of the given `sym` according to this environment.
    */
  def get(sym: Symbol.KindedTypeVarSym)(implicit scope: RegionScope): Rigidity = {
    if (s.contains(sym) || sym.scope.isOutside(scope)) {
      Rigidity.Rigid
    } else {
      Rigidity.Flexible
    }
  }

  /**
    * Returns true iff the given `sym` is rigid according to this environment.
    */
  def isRigid(sym: Symbol.KindedTypeVarSym)(implicit scope: RegionScope): Boolean = get(sym) == Rigidity.Rigid

  /**
    * Returns true iff the given `sym` is flexible according to this environment.
    */
  def isFlexible(sym: Symbol.KindedTypeVarSym)(implicit scope: RegionScope): Boolean = get(sym) == Rigidity.Flexible

  /**
    * Returns the flexible vars from the given list.
    */
  def getFlexibleVarsOf(tvars: List[Type.Var])(implicit scope: RegionScope): List[Type.Var] = tvars.filter(tvar => isFlexible(tvar.sym))

  /**
    * Marks the given `sym` as rigid in this environment.
    */
  def markRigid(sym: Symbol.KindedTypeVarSym): RigidityEnv = RigidityEnv(s + sym)

  /**
    * Merges the two rigidity environments, favoring Rigid in case of conflict.
    */
  def ++(that: RigidityEnv): RigidityEnv = RigidityEnv(this.s ++ that.s)
}
