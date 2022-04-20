package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.util.InternalCompilerException

object BoolTable {

  sealed trait Term

  object Term {
    case object True extends Term

    case object False extends Term

    case class Var(sym: Symbol.KindedTypeVarSym) extends Term

    case class Neg(t: Term) extends Term

    case class Conj(t1: Term, t2: Term) extends Term

    case class Disj(t1: Term, t2: Term) extends Term
  }

  def minimize(tpe: Type): Type = {
    val t = fromType(tpe)

    val freeVars = tpe.typeVars.toList


    toType(t)
  }

  def fromType(tpe: Type): Term = tpe match {
    case Type.KindedVar(sym, _) => Term.Var(sym)
    case Type.True => Term.True
    case Type.False => Term.False
    case Type.Apply(Type.Cst(TypeConstructor.Not, _), x, _) => Term.Neg(fromType(x))
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.And, _), x, _), y, _) => Term.Conj(fromType(x), fromType(y))
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Or, _), x, _), y, _) => Term.Disj(fromType(x), fromType(y))
    case _ => throw InternalCompilerException(s"Unexpected type: '$tpe'.")
  }

  def toType(t0: Term): Type = t0 match {
    case Term.True => Type.True
    case Term.False => Type.False
    case Term.Var(sym) => Type.KindedVar(sym, SourceLocation.Unknown)
    case Term.Neg(t) => Type.mkNot(toType(t), SourceLocation.Unknown)
    case Term.Conj(t1, t2) => Type.mkAnd(toType(t1), toType(t2), SourceLocation.Unknown)
    case Term.Disj(t1, t2) => Type.mkOr(toType(t1), toType(t2), SourceLocation.Unknown)
  }

}
