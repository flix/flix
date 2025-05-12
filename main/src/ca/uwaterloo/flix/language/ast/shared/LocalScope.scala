/*
 * Copyright 2024 Chenhao Gao
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
package ca.uwaterloo.flix.language.ast.shared

import ca.uwaterloo.flix.util.collection.ListMap

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
  * @param scp the environment map containing variable names and their corresponding resolutions.
  */
case class LocalScope(scp: ListMap[String, Resolution]) {
  /**
    * Returns the map of variable names to their resolutions.
    */
  def m: Map[String, List[Resolution]] = scp.m

  /**
    * Returns the local scope extended with another local scope.
    */
  def ++(that: LocalScope): LocalScope = LocalScope(this.scp ++ that.scp)

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
  def +(kv: (String, Resolution)): LocalScope = LocalScope(scp + kv)

  /**
    * Returns the local scope extended with the additional mapping from `name` to `res`.
    *
    * Currently, we just take the first resolution in the list of resolutions.
    */
  def resolve(name: String): Option[Resolution] =
    scp.get(name).headOption
}
