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

case class EqualityEnv(private val m: Map[(Symbol.AssocTypeSym, TypeHead), AssocTypeDef]) {

  /**
    * Returns the value of the associated type, if it is defined.
    */
  def getAssocDef(sym: Symbol.AssocTypeSym, tpe: Type): Option[AssocTypeDef] = {
    for {
      head <- TypeHead.fromType(tpe)
      assoc <- m.get((sym, head))
    } yield assoc
  }

  /**
    * Adds the given associate type to the environment.
    */
  def addAssocTypeDef(sym: Symbol.AssocTypeSym, arg: Type, ret: Type): EqualityEnv = {
    TypeHead.fromType(arg) match {
      // Resiliency: Ignore this instance if it's not well-formed
      case None => this

      case Some(head) =>
        // tparams are Nil because we are adding instances directly, but not schemas of instances
        val tparams = Nil
        val defn = AssocTypeDef(tparams, arg, ret)

        EqualityEnv(m + ((sym, head) -> defn))
    }
  }
}
