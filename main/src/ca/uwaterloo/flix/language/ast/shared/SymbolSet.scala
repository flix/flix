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

import ca.uwaterloo.flix.language.ast.Symbol

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
}
case class SymbolSet(
                    enums: Set[Symbol.EnumSym],
                    structs: Set[Symbol.StructSym],
                    // restrictableEnum: Set[Symbol.RestrictableEnumSym],
                    /* cases: Set[Symbol.CaseSym],
                    restrictableCases: Set[Symbol.RestrictableCaseSym],
                    structField: Set[Symbol.StructFieldSym],
                    */ traits: Set[Symbol.TraitSym],
                    /* sigs: Set[Symbol.SigSym],
                    labels: Set[Symbol.LabelSym],
                    typeAliases: Set[Symbol.TypeAliasSym],
                    assocTypes: Set[Symbol.AssocTypeSym], */
                    effects: Set[Symbol.EffSym],
                    /* ops: Set[Symbol.OpSym],
                    regionSym: Set[Symbol.RegionSym],
                    modules: Set[Symbol.ModuleSym], */
                    ) {

  /**
    * Returns the union of the symbol sets
    */
  def ++(that : SymbolSet): SymbolSet = {
    SymbolSet(
      enums ++ that.enums,
      structs ++ that.structs,
      /* restrictableEnum ++ that.restrictableEnum,
      cases ++ that.cases,
      restrictableCases ++ that.restrictableCases,
      structField ++ that.structField, */
      traits ++ that.traits,
      /* sigs ++ that.sigs,
      labels ++ that.labels,
      typeAliases ++ that.typeAliases,
      assocTypes ++ that.assocTypes, */
      effects ++ that.effects,
      /* ops ++ that.ops,
      regionSym ++ that.regionSym,
      modules ++ that.modules */
    )
  }

  /**
    * Returns the intersection of the symbol sets
    */
  def &(that: SymbolSet): SymbolSet = {
    SymbolSet(
      enums & that.enums,
      structs & that.structs,
      /* restrictableEnum & that.restrictableEnum,
      cases & that.cases,
      restrictableCases & that.restrictableCases,
      structField & that.structField, */
      traits & that.traits,
      /* sigs & that.sigs,
      labels & that.labels,
      typeAliases & that.typeAliases,
      assocTypes & that.assocTypes, */
      effects & that.effects,
      /* ops & that.ops,
      regionSym & that.regionSym,
      modules & that.modules */
    )
  }
}
