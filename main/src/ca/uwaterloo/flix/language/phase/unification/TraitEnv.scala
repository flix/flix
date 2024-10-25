/*
 * Copyright 2024 Matthew Lutze
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

import ca.uwaterloo.flix.language.ast.{Ast, Symbol, Type}
import ca.uwaterloo.flix.util.InternalCompilerException

/**
  * The trait environment stores information about traits.
  */
case class TraitEnv(private val m: Map[Symbol.TraitSym, Ast.TraitContext]) {

  /**
    * Returns the instances of the given trait.
    */
  def getInstances(sym: Symbol.TraitSym): List[Ast.Instance] = {
    m(sym).instances
  }

  /**
    * Returns the instances of the given trait.
    *
    * Returns None if the symbol is not in the TraitEnv.
    */
  def getInstancesOpt(sym: Symbol.TraitSym): Option[List[Ast.Instance]] = {
    m.get(sym).map(_.instances)
  }

  /**
    * Returns the supertraits of the given trait.
    */
  def getSuperTraits(sym: Symbol.TraitSym): List[Symbol.TraitSym] = {
    m(sym).superTraits
  }

  /**
    * Returns the supertraits of the given trait.
    */
  def getSuperTraitsOpt(sym: Symbol.TraitSym): Option[List[Symbol.TraitSym]] = {
    m.get(sym).map(_.superTraits)
  }

  /**
    * Adds the given instance to the trait environment
    *
    * Transitively adds the supertraits of the given trait.
    * For example, given the trait environment:
    *
    * {{{
    *   trait Order[a] with Eq[a]
    *   instance Eq[String]
    *   instance Order[String]
    * }}}
    *
    * If we add
    * {{{
    *   instance Order[b]
    * }}}
    *
    * then we get
    * {{{
    *   trait Order[a] with Eq[a]
    *   instance Eq[String]
    *   instance Order[String]
    *
    *   instance Eq[b]
    *   instance Order[b]
    * }}}
    */
  def addInstance(sym: Symbol.TraitSym, tpe: Type): TraitEnv = {
    val syms = getTransitiveSuperTraits(sym) + sym
    val newM = syms.foldLeft(m) {
      case (acc, s) =>
        val inst = Ast.Instance(tpe, Nil)
        val context = m.get(s) match {
          case Some(Ast.TraitContext(supers, insts)) => Ast.TraitContext(supers, inst :: insts)
          case None => throw InternalCompilerException(s"unexpected unknown trait sym: $sym", sym.loc)
        }
        acc + (s -> context)
    }
    TraitEnv(newM)
  }

  /**
    * Returns a map from trait symbols to trait context.
    */
  def toMap: Map[Symbol.TraitSym, Ast.TraitContext] = m

  /**
    * Returns the super traits of the symbol, as well as the super traits' super traits, etc.
    */
  private def getTransitiveSuperTraits(sym: Symbol.TraitSym): Set[Symbol.TraitSym] = {
    val directSupers = getSuperTraits(sym)
    val indirectSupers = directSupers.toSet.flatMap(getTransitiveSuperTraits)
    indirectSupers ++ directSupers
  }
}
