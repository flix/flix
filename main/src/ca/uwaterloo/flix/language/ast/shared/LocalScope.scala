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
  * @param env the environment map containing variable names and their corresponding resolutions.
  */
case class LocalScope(env: ListMap[String, Resolution]){
  /**
    * Returns the map of variable names to their resolutions.
    */
  def m: Map[String, List[Resolution]] = env.m

  /**
    * Returns the local scope extended with another local scope.
    */
  def ++(that: LocalScope): LocalScope = LocalScope(this.env ++ that.env)

  /**
    * Returns an option of the list of resolutions corresponding to the variable `name`.
    */
  def get(name: String): Option[List[Resolution]] = env.get(name)

  /**
    * Returns the list of resolutions corresponding to the variable `name`.
    */
  def apply(name: String): List[Resolution] = env(name)

  /**
    * Returns the local scope extended with the additional mapping from `name` to `res`.
    */
  def +(kv: (String, Resolution)): LocalScope = LocalScope(env + kv)
}
