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
  def symbolsOf(wrt: Type): SymbolSet = {
    /**
      * Returns all the symbols in a given type constructor
      */
    def getSymbolsWithin(wrt: TypeConstructor): SymbolSet = {
      wrt match {
        case TypeConstructor.Enum(sym, _) => SymbolSet(Set(sym), Set.empty, Set.empty, Set.empty)
        case TypeConstructor.Struct(sym, _) => SymbolSet(Set.empty, Set(sym), Set.empty, Set.empty)
        case TypeConstructor.Effect(sym, _) => SymbolSet(Set.empty, Set.empty, Set.empty, Set(sym))
        case _ => SymbolSet.empty
      }
    }

    (wrt.typeConstructors map getSymbolsWithin).foldLeft(empty) {
      _ ++ _
    }
  }

  private def isAmbiguous(sym1: QualifiedSym, sym2: QualifiedSym): Boolean = {
    sym1.namespace != sym2.namespace && sym1.name == sym2.name
  }

  /**
    * Return a symbol set containing all the symbols in `s1` ambiguous w.r.t `s2`.
    * @param s1 The primary symbol set
    * @param s2 The set to be checked against
    */
  def ambiguous(s1: SymbolSet, s2: SymbolSet): SymbolSet = SymbolSet(
    s1.enums.filter(sym1 => s2.enums.exists(sym2 => isAmbiguous(sym1, sym2))),
    s1.structs.filter(sym1 => s2.structs.exists(sym2 => isAmbiguous(sym1, sym2))),
    s1.traits.filter(sym1 => s2.traits.exists(sym2 => isAmbiguous(sym1, sym2))),
    s1.effects.filter(sym1 => s2.effects.exists(sym2 => isAmbiguous(sym1, sym2)))
  )
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

  /**
    * Returns the intersection of `this` and `that`
    */
  def &(that: SymbolSet): SymbolSet = {
    SymbolSet(
      enums & that.enums,
      structs & that.structs,
      traits & that.traits,
      effects & that.effects,
    )
  }

  /**
    * Checks to see if `this` is ambiguous with respect to the given `SymbolSet`.
    */
  private def isAmbiguous(sym : QualifiedSym): Boolean = {
    enums.exists(x => x.qname == sym.qname && x.qnamespace != sym.qnamespace) ||
      structs.exists(x => x.qname == sym.qname && x.qnamespace != sym.qnamespace) ||
      traits.exists(x => x.qname == sym.qname && x.qnamespace != sym.qnamespace)||
      effects.exists(x => x.qname == sym.qname && x.qnamespace != sym.qnamespace)
  }

  /**
    * Finds all symbols that are ambiguous in `this`
    */
  def getAmbiguous: Set[_] = {
    (enums ++ structs ++ traits ++ effects) filter isAmbiguous
  }

  /**
    * Formats the qualified name of a symbol, with respect to `this`
    *
    * @param sym The symbol to compute
    */
  def formatDistinct(sym : QualifiedSym): String = {
    if (isAmbiguous(sym)) (sym.qnamespace.mkString(".") + "." + sym.qname)
    else sym.qname
  }
}
