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

  /**
    * Constructs the tuple type (A, B, ...) where the types are drawn from the list `ts`.
    */
  def mkTuple(ts: List[UnkindedType], loc: SourceLocation): UnkindedType = {
    val init = UnkindedType.Cst(TypeConstructor.Tuple(ts.length), loc)
    ts.foldLeft(init: UnkindedType) {
      case (acc, x) => Apply(acc, x, loc)
    }
  }

  /**
    * Constructs a RecordExtend type.
    */
  def mkRecordRowExtend(field: Name.Field, tpe: UnkindedType, rest: UnkindedType, loc: SourceLocation): UnkindedType = {
    mkApply(UnkindedType.Cst(TypeConstructor.RecordRowExtend(field), loc), List(tpe, rest), loc)
  }

  /**
    * Constructs a SchemaExtend type.
    */
  def mkSchemaRowExtend(pred: Name.Pred, tpe: UnkindedType, rest: UnkindedType, loc: SourceLocation): UnkindedType = {
    mkApply(UnkindedType.Cst(TypeConstructor.SchemaRowExtend(pred), loc), List(tpe, rest), loc)
  }

  /**
    * Constructs a Record type.
    */
  def mkRecord(tpe: UnkindedType, loc: SourceLocation): UnkindedType = {
    Apply(UnkindedType.Cst(TypeConstructor.Record, loc), tpe, loc)
  }

  /**
    * Constructs a Schema type.
    */
  def mkSchema(tpe: UnkindedType, loc: SourceLocation): UnkindedType = {
    Apply(UnkindedType.Cst(TypeConstructor.Schema, loc), tpe, loc)
  }


  /**
    * Construct a relation type with the given list of type arguments `ts0`.
    */
  def mkRelation(ts0: List[UnkindedType], loc: SourceLocation): UnkindedType = {
    val ts = ts0 match {
      case Nil => UnkindedType.Cst(TypeConstructor.Unit, loc)
      case x :: Nil => x
      case xs => mkTuple(xs, loc)
    }

    Apply(UnkindedType.Cst(TypeConstructor.Relation, loc), ts, loc)
  }

  /**
    * Construct a lattice type with the given list of type arguments `ts0`.
    */
  def mkLattice(ts0: List[UnkindedType], loc: SourceLocation): UnkindedType = {
    val ts = ts0 match {
      case Nil => UnkindedType.Cst(TypeConstructor.Unit, loc)
      case x :: Nil => x
      case xs => mkTuple(xs, loc)
    }

    Apply(UnkindedType.Cst(TypeConstructor.Lattice, loc), ts, loc)
  }

  /**
    * Erases all the aliases from the type.
    */
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
