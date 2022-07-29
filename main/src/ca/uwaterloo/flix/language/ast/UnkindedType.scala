package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.api.Flix

object UnkindedType {
  case class Var(sym: Symbol.UnkindedTypeVarSym, loc: SourceLocation) extends UnkindedType
  case class Cst(tc: TypeConstructor, loc: SourceLocation) extends UnkindedType
  case class Enum(sym: Symbol.EnumSym, loc: SourceLocation) extends UnkindedType
  case class UnappliedAlias(sym: Symbol.TypeAliasSym, loc: SourceLocation) extends UnkindedType
  case class Apply(tpe1: UnkindedType, tpe2: UnkindedType, loc: SourceLocation) extends UnkindedType
  case class UnkindedArrow(purAndEff: PurityAndEffect, arity: Int, loc: SourceLocation) extends UnkindedType
  case class ReadWrite(tpe: UnkindedType, loc: SourceLocation) extends UnkindedType
  case class Ascribe(tpe: UnkindedType, kind: Kind, loc: SourceLocation) extends UnkindedType
  case class Alias(cst: Ast.AliasConstructor, args: List[Type], tpe: Type, loc: SourceLocation) extends UnkindedType

  case class PurityAndEffect(pur: Option[UnkindedType], eff: Option[List[UnkindedType]])

  /**
    * Returns a fresh type variable of the given kind `k` and rigidity `r`.
    */
  def freshVar(loc: SourceLocation, isRegion: Boolean = false, text: Ast.VarText = Ast.VarText.Absent)(implicit flix: Flix): UnkindedType.Var = {
    val sym = Symbol.freshUnkindedTypeVarSym(text, isRegion, loc)
    UnkindedType.Var(sym, loc)
  }
}

sealed trait UnkindedType {
  def loc: SourceLocation
}
