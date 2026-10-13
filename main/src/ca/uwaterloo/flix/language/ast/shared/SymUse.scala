/*
 * Copyright 2024 Holger Dal Mogensen
 * Copyright 2024 Alexander Dybdahl Troelsen
 * Copyright 2025 Chenhao Gao
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.ast.shared

import ca.uwaterloo.flix.language.ast.Name.{Ident, QName}
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol}

sealed trait SymUse

object SymUse {
  /**
    * Represents a use of an associated type sym.
    */
  case class AssocTypeSymUse(sym: Symbol.AssocTypeSym, loc: SourceLocation) extends SymUse

  /**
    * Represents a use of an enum case sym.
    */
  case class CaseSymUse(sym: Symbol.CaseSym, loc: SourceLocation) extends SymUse

  /**
    * Represents a use of a defn sym.
    */
  case class DefSymUse(sym: Symbol.DefnSym, loc: SourceLocation) extends SymUse

  /**
    * Represents a use of an effect sym.
    *
    * For an occurrence of the form `Xxx.Yyy.Zzz`, `sym` is the [[Symbol]] of the accessed element `Zzz`
    * and `qname` represents the qualified name `Xxx.Yyy.Zzz` in its entirety, where [[QName.namespace]]
    * contains an [[Ident]] for `Xxx` and `Yyy` and [[QName.ident]] contains a [[Ident]] for `Zzz`.
    *
    * @param sym    The [[Symbol]] being used.
    * @param qname  The qualified name ([[QName]]) of the use of `sym`:
    */
  case class EffSymUse(sym: Symbol.EffSym, qname: QName) extends SymUse

  /**
    * Represents a use of a LocalDef sym.
    */
  case class LocalDefSymUse(sym: Symbol.VarSym, loc: SourceLocation) extends SymUse

  /**
    * Represents a use of an effect operation sym.
    */
  case class OpSymUse(sym: Symbol.OpSym, loc: SourceLocation) extends SymUse

  /**
    * Represents a use of a restrictable enum case sym.
    */
  case class RestrictableCaseSymUse(sym: Symbol.RestrictableCaseSym, loc: SourceLocation) extends SymUse

  /**
    * Represents a use of a restrictable enum sym.
    */
  case class RestrictableEnumSymUse(sym: Symbol.RestrictableEnumSym, loc: SourceLocation) extends SymUse

  /**
    * Represents a use of a sig sym.
    */
  case class SigSymUse(sym: Symbol.SigSym, loc: SourceLocation) extends SymUse

  /**
    * Represents a use of a struct field sym.
    */
  case class StructFieldSymUse(sym: Symbol.StructFieldSym, loc: SourceLocation) extends SymUse

  /**
    * Represents a use of a class sym.
    */
  case class TraitSymUse(sym: Symbol.TraitSym, loc: SourceLocation) extends SymUse

  /**
    * Represents a use of a type alias sym.
    */
  case class TypeAliasSymUse(sym: Symbol.TypeAliasSym, loc: SourceLocation) extends SymUse

}
