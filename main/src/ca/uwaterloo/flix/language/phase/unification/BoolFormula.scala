/*
 * Copyright 2022 Magnus Madsen
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
package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.language.ast.{Kind, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.util.InternalCompilerException
import ca.uwaterloo.flix.util.collection.Bimap

import scala.collection.immutable.SortedSet

/**
  * A common super-type for Boolean formulas.
  */
sealed trait BoolFormula {

  /**
    * Returns the free variables in `this` formula.
    */
  final def freeVars: SortedSet[Int] = this match {
    case BoolFormula.True => SortedSet.empty
    case BoolFormula.False => SortedSet.empty
    case BoolFormula.Var(x) => SortedSet(x)
    case BoolFormula.Neg(f) => f.freeVars
    case BoolFormula.Conj(f1, f2) => f1.freeVars ++ f2.freeVars
    case BoolFormula.Disj(f1, f2) => f1.freeVars ++ f2.freeVars
  }

  /**
    * Returns the size of `this` formulas.
    *
    * The size is the number of conjunctions and disjunctions.
    */
  final def size: Int = this match {
    case BoolFormula.True => 0
    case BoolFormula.False => 0
    case BoolFormula.Var(_) => 0
    case BoolFormula.Neg(t) => t.size
    case BoolFormula.Conj(t1, t2) => t1.size + t2.size + 1
    case BoolFormula.Disj(t1, t2) => t1.size + t2.size + 1
  }

  /**
    * Returns a human-readable string representation of `this` formula.
    */
  override def toString: String = this match {
    case BoolFormula.True => "true"
    case BoolFormula.False => "false"
    case BoolFormula.Var(x) => s"x$x"
    case BoolFormula.Neg(f) => f match {
      case BoolFormula.Var(x) => s"!x$x"
      case _ => s"!($f)"
    }
    case BoolFormula.Conj(f1, f2) => s"(and $f1 $f2)"
    case BoolFormula.Disj(f1, f2) => s"(or $f1 $f2)"
  }

}

object BoolFormula {

  /**
    * Represents the constant True.
    */
  case object True extends BoolFormula

  /**
    * Represents the constant False.
    */
  case object False extends BoolFormula

  /**
    * Represents a variable. Variables are numbered by integers.
    */
  case class Var(x: Int) extends BoolFormula

  /**
    * Represents the negation of the formula `f`.
    */
  case class Neg(f: BoolFormula) extends BoolFormula

  /**
    * Represents the conjunction (logical and) of `f1` and `f2`.
    */
  case class Conj(f1: BoolFormula, f2: BoolFormula) extends BoolFormula

  /**
    * Represents the disjunction (logical or) of `f1` and `f2`.
    */
  case class Disj(f1: BoolFormula, f2: BoolFormula) extends BoolFormula

  /**
    * Substitutes all variables in `f` using the substitution map `m`.
    *
    * The map `m` must bind each free variable in `f` to a (new) variable.
    */
  def substitute(f: BoolFormula, m: Bimap[Int, Int]): BoolFormula = f match {
    case True => True
    case False => False
    case Var(x) => m.getForward(x) match {
      case None => throw InternalCompilerException(s"Unexpected unbound variable: 'x$x'.")
      case Some(y) => Var(y)
    }
    case Neg(f1) => Neg(substitute(f1, m))
    case Conj(f1, f2) => Conj(substitute(f1, m), substitute(f2, m))
    case Disj(f1, f2) => Disj(substitute(f1, m), substitute(f2, m))
  }

  /**
    * Converts the given type `tpe` to a Boolean formula under the given variable substitution map `m`.
    *
    * The map `m` must bind each free type variable in `tpe` to a Boolean variable.
    */
  def fromBoolType(tpe: Type, m: Bimap[VarOrEff, Int]): BoolFormula = tpe match {
    case Type.KindedVar(sym, _) => m.getForward(VarOrEff.Var(sym)) match {
      case None => throw InternalCompilerException(s"Unexpected unbound variable: '$sym'.")
      case Some(x) => Var(x)
    }
    case Type.True => True
    case Type.False => False
    case Type.Apply(Type.Cst(TypeConstructor.Not, _), tpe1, _) => Neg(fromBoolType(tpe1, m))
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.And, _), tpe1, _), tpe2, _) => Conj(fromBoolType(tpe1, m), fromBoolType(tpe2, m))
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Or, _), tpe1, _), tpe2, _) => Disj(fromBoolType(tpe1, m), fromBoolType(tpe2, m))
    case _ => throw InternalCompilerException(s"Unexpected type: '$tpe'.")
  }

  /**
    * Converts the given type `tpe` to a Boolean formula under the given variable substitution map `m`.
    *
    * The map `m` must bind each free type variable in `tpe` to a Boolean variable.
    */
  def fromEffType(tpe: Type, m: Bimap[VarOrEff, Int]): BoolFormula = tpe match {
    case Type.KindedVar(sym, _) => m.getForward(VarOrEff.Var(sym)) match {
      case None => throw InternalCompilerException(s"Unexpected unbound variable: '$sym'.")
      case Some(x) => Var(x)
    }
    case Type.Cst(TypeConstructor.Effect(sym), _) => m.getForward(VarOrEff.Eff(sym)) match {
      case None => throw InternalCompilerException(s"Unexpected unbound effect: '$sym'.")
      case Some(x) => Var(x)
    }
    case Type.All => True
    case Type.Empty => False
    case Type.Apply(Type.Cst(TypeConstructor.Complement, _), tpe1, _) => Neg(fromEffType(tpe1, m))
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Intersection, _), tpe1, _), tpe2, _) => Conj(fromEffType(tpe1, m), fromEffType(tpe2, m))
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Union, _), tpe1, _), tpe2, _) => Disj(fromEffType(tpe1, m), fromEffType(tpe2, m))
    case _ => throw InternalCompilerException(s"Unexpected type: '$tpe'.")
  }

  /**
    * Converts the given formula `f` back to a type under the given variable substitution map `m`.
    *
    * The map `m` must bind each free variable in `f` to a type variable.
    */
  def toType(f: BoolFormula, m: Bimap[VarOrEff, Int], kind: Kind, loc: SourceLocation): Type = kind match {
    case Kind.Bool => toBoolType(f, m, loc)
    case Kind.Effect => toEffType(f, m, loc)
    case _ => throw InternalCompilerException(s"Unexpected kind: '$kind'.")
  }

  /**
    * Converts the given formula `f` back to a type under the given variable substitution map `m`.
    *
    * The map `m` must bind each free variable in `f` to a type variable.
    */
  private def toBoolType(f: BoolFormula, m: Bimap[VarOrEff, Int], loc: SourceLocation): Type = f match {
    case True => Type.True
    case False => Type.False
    case Var(x) => m.getBackward(x) match {
      case None => throw InternalCompilerException(s"Unexpected unbound variable: '$x'.")
      case Some(VarOrEff.Var(sym)) => Type.KindedVar(sym, loc)
      case Some(VarOrEff.Eff(sym)) => throw InternalCompilerException(s"Unexpected effect: '$sym'.")
    }
    case Neg(f1) => Type.mkNot(toBoolType(f1, m, loc), loc)
    case Conj(t1, t2) => Type.mkAnd(toBoolType(t1, m, loc), toBoolType(t2, m, loc), loc)
    case Disj(t1, t2) => Type.mkOr(toBoolType(t1, m, loc), toBoolType(t2, m, loc), loc)
  }

  /**
    * Converts the given formula `f` back to a type under the given variable substitution map `m`.
    *
    * The map `m` must bind each free variable in `f` to a type variable.
    */
  private def toEffType(f: BoolFormula, m: Bimap[VarOrEff, Int], loc: SourceLocation): Type = f match {
    case True => Type.All
    case False => Type.Empty
    case Var(x) => m.getBackward(x) match {
      case None => throw InternalCompilerException(s"Unexpected unbound variable: '$x'.")
      case Some(VarOrEff.Var(sym)) => Type.KindedVar(sym, loc)
      case Some(VarOrEff.Eff(sym)) => Type.Cst(TypeConstructor.Effect(sym), loc)
    }
    case Neg(f1) => Type.mkComplement(toEffType(f1, m, loc), loc)
    case Conj(t1, t2) => Type.mkIntersection(toEffType(t1, m, loc), toEffType(t2, m, loc), loc)
    case Disj(t1, t2) => Type.mkUnion(toEffType(t1, m, loc), toEffType(t2, m, loc), loc)
  }

  /**
    * Union of variable and effect types.
    */
  sealed trait VarOrEff

  object VarOrEff {
    /**
      * A type variable.
      */
    case class Var(sym: Symbol.KindedTypeVarSym) extends VarOrEff

    /**
      * An effect constant.
      */
    case class Eff(sym: Symbol.EffectSym) extends VarOrEff
  }
}
