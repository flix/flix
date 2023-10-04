/*
 * Copyright 2023 Matthew Lutze
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
package ca.uwaterloo.flix.language.ast

/**
  * Tracks levels of regions, allowing them to be purified out of their scope.
  */
sealed trait LevelEnv {

  /**
    * Purifies regions in the type if they are out of scope.
    */
  def purify(tpe: Type): Type = tpe
//    this match {
//    case LevelEnv.Unleveled => tpe
//    case LevelEnv.Leveled(scopes) =>
//      tpe.map {
//        case Type.Var(sym, _) if (sym.isRegion && !scopes.contains(sym)) => Type.Pure
//        case t => t
//      }
//  }

  /**
    * Enters the scope of the given region.
    */
  def enterScope(sym: Symbol.KindedTypeVarSym): LevelEnv = this match {
    case LevelEnv.Unleveled => this
    case LevelEnv.Leveled(scopes) => LevelEnv.Leveled(scopes + sym)
  }

  /**
    * Exits the scope of the given region.
    */
  def exitScope(sym: Symbol.KindedTypeVarSym): LevelEnv = this match {
    case LevelEnv.Unleveled => this
    case LevelEnv.Leveled(scopes) => LevelEnv.Leveled(scopes - sym)
  }

}

object LevelEnv {

  /**
    * The top level environment: all regions are purified.
    */
  val Top: LevelEnv = Leveled(Set.empty)

  /**
    * A level environment where levels are not relevant.
    */
  case object Unleveled extends LevelEnv

  /**
    * A level environment where levels are relevant.
    */
  case class Leveled(scopes: Set[Symbol.KindedTypeVarSym]) extends LevelEnv
}

