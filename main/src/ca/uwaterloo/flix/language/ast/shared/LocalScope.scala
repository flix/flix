/*
 * Copyright 2024 Chenhao Gao
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.ast.shared

import ca.uwaterloo.flix.language.ast.UnkindedType
import ca.uwaterloo.flix.util.collection.ListMap

import java.lang.constant.ClassDesc

/**
  * Companion object for the [[LocalScope]] class.
  */
object LocalScope {
  /**
    * Returns the empty local scope.
    */
  def empty: LocalScope = LocalScope(ListMap.empty[String, Resolution])

  /**
    * Returns a singleton local scope with a mapping from `name` to `res`.
    */
  def singleton(name: String, res: Resolution): LocalScope = LocalScope(ListMap.singleton(name, res))
}

/**
  * Represents a local scope with a mapping from variable names to their resolutions.
  *
  * @param scp        the environment map containing variable names and their corresponding resolutions.
  * @param superClass `None` means super calls are illegal here; `Some(clazz)` means super calls resolve to `clazz`.
  * @param superTargs the type arguments of the enclosing NewObject expression, if any.
  */
case class LocalScope(scp: ListMap[String, Resolution], superClass: Option[ClassDesc] = None, superTargs: List[UnkindedType] = Nil) {
  /**
    * Returns the map of variable names to their resolutions.
    */
  def m: Map[String, List[Resolution]] = scp.m

  /**
    * Returns the local scope extended with another local scope.
    */
  def ++(that: LocalScope): LocalScope = LocalScope(this.scp ++ that.scp, this.superClass, this.superTargs)

  /**
    * Returns an option of the list of resolutions corresponding to the variable `name`.
    */
  def get(name: String): List[Resolution] = scp.get(name)

  /**
    * Returns the list of resolutions corresponding to the variable `name`.
    */
  def apply(name: String): List[Resolution] = scp(name)

  /**
    * Returns the local scope extended with the additional mapping from `name` to `res`.
    */
  def +(kv: (String, Resolution)): LocalScope = LocalScope(scp + kv, this.superClass, this.superTargs)

  /**
    * Returns the local scope extended with the additional mapping from `name` to `res`.
    *
    * Currently, we just take the first resolution in the list of resolutions.
    */
  def resolve(name: String): Option[Resolution] =
    scp.get(name).headOption

  def withSuperClass(clazz: Option[ClassDesc]): LocalScope = copy(superClass = clazz)

  def withSuperTargs(targs: List[UnkindedType]): LocalScope = copy(superTargs = targs)
}
