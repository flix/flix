package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.language.ast.{Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.util.InternalCompilerException

object BoolTable {

  sealed trait Term

  object Term {
    case class Var(sym: Symbol.TypeVarSym) extends Term

    case class Neg(t: Term) extends Term

    case class Conj(x: Term, y: Term) extends Term

    case class Disj(x: Term, y: Term) extends Term
  }

  def minimize(tpe: Type): Type = {
    val t = fromType(tpe)

    tpe
  }

  def fromType(tpe: Type): Term = tpe match {
    case Type.KindedVar(sym, _) => Term.Var(sym)
    case Type.Apply(Type.Cst(TypeConstructor.Not, _), x, _) => Term.Neg(fromType(x))
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.And, _), x, _), y, _) => Term.Conj(fromType(x), fromType(y))
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Or, _), x, _), y, _) => Term.Disj(fromType(x), fromType(y))
    case _ => throw InternalCompilerException(s"Unexpected type: '$tpe'.")
  }

}
