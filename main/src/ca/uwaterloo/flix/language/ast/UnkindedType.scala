package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.util.InternalCompilerException

object UnkindedType {
  case class Var(sym: Symbol.UnkindedTypeVarSym, loc: SourceLocation) extends UnkindedType
  case class Cst(tc: TypeConstructor, loc: SourceLocation) extends UnkindedType
  case class Enum(sym: Symbol.EnumSym, loc: SourceLocation) extends UnkindedType
  case class UnappliedAlias(sym: Symbol.TypeAliasSym, loc: SourceLocation) extends UnkindedType
  case class Apply(tpe1: UnkindedType, tpe2: UnkindedType, loc: SourceLocation) extends UnkindedType
  case class Arrow(purAndEff: PurityAndEffect, arity: Int, loc: SourceLocation) extends UnkindedType
  case class ReadWrite(tpe: UnkindedType, loc: SourceLocation) extends UnkindedType
  case class Ascribe(tpe: UnkindedType, kind: Kind, loc: SourceLocation) extends UnkindedType
  case class Alias(cst: Ast.AliasConstructor, args: List[UnkindedType], tpe: UnkindedType, loc: SourceLocation) extends UnkindedType

  case class PurityAndEffect(pur: Option[UnkindedType], eff: Option[List[UnkindedType]]) {
    /**
      * Maps the function `f` over the contents of this PurityAndEffect
      */
    def map(f: UnkindedType => UnkindedType): PurityAndEffect = PurityAndEffect(pur.map(f), eff.map(_.map(f)))
  }

  /**
    * Returns a fresh type variable of the given kind `k` and rigidity `r`.
    */
  def freshVar(loc: SourceLocation, isRegion: Boolean = false, text: Ast.VarText = Ast.VarText.Absent)(implicit flix: Flix): UnkindedType.Var = {
    val sym = Symbol.freshUnkindedTypeVarSym(text, isRegion, loc)
    UnkindedType.Var(sym, loc)
  }

  /**
    * Constructs the apply type base[t_1, ,..., t_n].
    */
  def mkApply(base: UnkindedType, ts: List[UnkindedType], loc: SourceLocation): UnkindedType = ts.foldLeft(base) {
    case (acc, t) => Apply(acc, t, loc)
  }

  def eraseAliases(tpe0: UnkindedType): UnkindedType = tpe0 match {
    case tpe: Var => tpe
    case tpe: Cst => tpe
    case tpe: Enum => tpe
    case Apply(tpe1, tpe2, loc) => Apply(eraseAliases(tpe1), eraseAliases(tpe2), loc)
    case Arrow(purAndEff, arity, loc) => Arrow(purAndEff.map(eraseAliases), arity, loc)
    case ReadWrite(tpe, loc) => ReadWrite(eraseAliases(tpe), loc)
    case Ascribe(tpe, kind, loc) => Ascribe(eraseAliases(tpe), kind, loc)
    case Alias(_, _, tpe, _) => eraseAliases(tpe)
    case UnappliedAlias(_, _) => throw InternalCompilerException("unexpected unapplied alias")
  }
}

sealed trait UnkindedType {
  def loc: SourceLocation
}
