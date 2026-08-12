/*
 * Copyright 2025 Matthew Lutze
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
package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.language.ast.shared.AssocTypeDef
import ca.uwaterloo.flix.language.ast.{Symbol, Type, TypeHead}

/**
  * Maintains information about associated type definitions.
  */
object EqualityEnv {
  val empty: EqualityEnv = EqualityEnv(Map.empty)
}

case class EqualityEnv(private val m: Map[(Symbol.AssocTypeSym, TypeHead), List[AssocTypeDef]]) {

  /**
    * Returns the definitions of the associated type for the given selector type.
    */
  def getAssocDefs(sym: Symbol.AssocTypeSym, sel: Type): List[AssocTypeDef] = {
    TypeHead.fromType(sel) match {
      case None => Nil
      case Some(head) => m.getOrElse((sym, head), Nil)
    }
  }

  /**
    * Adds the given associate type to the environment.
    */
  // MATT docs
  def addAssocTypeDef(sym: Symbol.AssocTypeSym, sel: Type, args: List[Type], ret: Type): EqualityEnv = {
    TypeHead.fromType(sel) match {
      // Resiliency: Ignore this instance if it's not well-formed
      case None => this

      case Some(head) =>
        // tparams are Nil because we are adding instances directly, but not schemas of instances
        val tparams = Nil
        val defn = AssocTypeDef(tparams, sel, args, ret)

        val key = (sym, head)
        EqualityEnv(m + (key -> (defn :: m.getOrElse(key, Nil))))
    }
  }
}
