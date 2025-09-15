/*
 * Copyright 2025 Asher Frost
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

import ca.uwaterloo.flix.language.ast.{Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.ast.shared.QualifiedSym
object SymbolSet {

  /**
    * Returns an empty symbol set.
    */
  val empty: SymbolSet = SymbolSet(
    Set.empty[Symbol.EnumSym],
    Set.empty[Symbol.StructSym],
    Set.empty[Symbol.TraitSym],
    Set.empty[Symbol.EffSym],
  )

  /**
    * Returns all the symbols in a given type
    */
  def symbolsOf(tpe: Type): SymbolSet = {
    /**
      * Returns all the symbols in a given type constructor
      */
    def getSymbolsWithin(t: TypeConstructor): SymbolSet = {
      t match {
        case TypeConstructor.Enum(sym, _) => SymbolSet(Set(sym), Set.empty, Set.empty, Set.empty)
        case TypeConstructor.Struct(sym, _) => SymbolSet(Set.empty, Set(sym), Set.empty, Set.empty)
        case TypeConstructor.Effect(sym, _) => SymbolSet(Set.empty, Set.empty, Set.empty, Set(sym))
        case _ => SymbolSet.empty
      }
    }
    (tpe.typeConstructors map getSymbolsWithin).foldLeft(empty) { _ ++ _ }
  }

  /**
    * Return a symbol set containing all the symbols in `s1` ambiguous w.r.t `s2`.
    * @param s1 The first symbol set
    * @param s2 The second symbol set
    * @return All the symbols that are in `s1` such that a symbol in `s2` has the same base name but a different namespace
    */
  def ambiguous(s1: SymbolSet, s2: SymbolSet): SymbolSet = {
    val s3 = s1 ++ s2
    SymbolSet(
      s3.enums.filter(sym1 => s3.enums.count(sym2 => isAmbiguous(sym1, sym2)) > 1),
      s3.structs.filter(sym1 => s3.enums.count(sym2 => isAmbiguous(sym1, sym2)) > 1),
      s3.traits.filter(sym1 => s3.enums.count(sym2 => isAmbiguous(sym1, sym2)) > 1),
      s3.effects.filter(sym1 => s3.enums.count(sym2 => isAmbiguous(sym1, sym2)) > 1),
    )
  }

  /**
    * Checks if two symbols are ambiguous with respect to each other.
    * Two symbols are ambiguous if they have the same name but different namespaces.
    *
    * @param sym1 The first symbol
    * @param sym2 The second symbol
    * @return true if the symbols are ambiguous, false otherwise
    */
  private def isAmbiguous(sym1: QualifiedSym, sym2: QualifiedSym): Boolean = {
    sym1.name == sym2.name && sym1.namespace != sym2.namespace
  }
}

case class SymbolSet(
                    enums: Set[Symbol.EnumSym],
                    structs: Set[Symbol.StructSym],
                    traits: Set[Symbol.TraitSym],
                    effects: Set[Symbol.EffSym],
                    ) {

  /**
    * Returns the union of `this` and `that`
    */
  def ++(that : SymbolSet): SymbolSet = {
    SymbolSet(
      enums ++ that.enums,
      structs ++ that.structs,
      traits ++ that.traits,
      effects ++ that.effects,
    )
  }
}
