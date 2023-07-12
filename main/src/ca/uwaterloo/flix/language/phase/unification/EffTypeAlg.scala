package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.util.InternalCompilerException
import ca.uwaterloo.flix.util.collection.Bimap

class EffTypeAlg extends BoolAlg2[Type, EffTypeAlg.VarOrEff] {
  /**
    * Returns `true` if `f` represents TRUE.
    */
  override def isTrue(f: Type): Boolean = f == Type.True

  /**
    * Returns `true` if `f` represents FALSE.
    */
  override def isFalse(f: Type): Boolean = f == Type.False

  /**
    * Returns `true` if `f` represents a variable.
    */
  override def isVar(f: Type): Boolean = f match {
    case Type.Var(_, _) => true
    case _ => false
  }

  /**
    * Returns a representation of TRUE.
    */
  override def mkTrue: Type = Type.True

  /**
    * Returns a representation of FALSE.
    */
  override def mkFalse: Type = Type.False

  /**
    * Returns a representation of the variable with the given `id`.
    */
  override def mkVar(id: EffTypeAlg.VarOrEff): Type = id match {
    case EffTypeAlg.VarOrEff.Eff(id) => Type.Cst(TypeConstructor.Effect(id), SourceLocation.Unknown)
    case EffTypeAlg.VarOrEff.Var(id) => Type.Var(id, SourceLocation.Unknown)
  }

  /**
    * Returns a representation of the complement of `f`.
    */
  override def mkNot(f: Type): Type = Type.mkNot(f, SourceLocation.Unknown)

  /**
    * Returns a representation of the disjunction of `f1` and `f2`.
    */
  override def mkOr(f1: Type, f2: Type): Type = Type.mkOr(f1, f2, SourceLocation.Unknown)

  /**
    * Returns a representation of the conjunction of `f1` and `f2`.
    */
  override def mkAnd(f1: Type, f2: Type): Type = Type.mkAnd(f1, f2, SourceLocation.Unknown)

  /**
    * Converts the given formula f into another Boolean formula.
    */
  override def convert[G, W](f: Type, env: Bimap[EffTypeAlg.VarOrEff, W])(implicit otherAlg: BoolAlg2[G, W]): G = f match {
    case Type.True => otherAlg.mkTrue
    case Type.False => otherAlg.mkFalse
    case Type.Apply(Type.Not, tpe1, _) => otherAlg.mkNot(convert(tpe1, env))
    case Type.Apply(Type.Apply(Type.And, tpe1, _), tpe2, _) => otherAlg.mkAnd(convert(tpe1, env), convert(tpe2, env))
    case Type.Apply(Type.Apply(Type.Or, tpe1, _), tpe2, _) => otherAlg.mkOr(convert(tpe1, env), convert(tpe2, env))
    case _ => throw InternalCompilerException(s"Unexpected type: ${f}", SourceLocation.Unknown)
  }
}

object EffTypeAlg {
  trait VarOrEff extends Ordered[VarOrEff] {
    override def compare(that: VarOrEff): Int = (this, that) match {
      case (VarOrEff.Var(_), VarOrEff.Eff(_)) => -1
      case (VarOrEff.Eff(_), VarOrEff.Var(_)) => 1
      case (VarOrEff.Eff(eff1), VarOrEff.Eff(eff2)) => eff1.compare(eff2)
      case (VarOrEff.Var(var1), VarOrEff.Var(var2)) => var1.compare(var2)
    }
  }

  object VarOrEff {
    case class Var(sym: Symbol.KindedTypeVarSym) extends VarOrEff

    case class Eff(sym: Symbol.EffectSym) extends VarOrEff

  }
}
