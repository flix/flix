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
  * A common super-type for Boolean algebras.
  */
sealed trait BoolFormula {

  /**
    * Returns the free variables in `this` expression.
    */
  final def freeVars: SortedSet[Int] = this match {
    case BoolFormula.True => SortedSet.empty
    case BoolFormula.False => SortedSet.empty
    case BoolFormula.Var(x) => SortedSet(x)
    case BoolFormula.Not(f) => f.freeVars
    case BoolFormula.And(f1, f2) => f1.freeVars ++ f2.freeVars
    case BoolFormula.Or(f1, f2) => f1.freeVars ++ f2.freeVars
  }

  /**
    * Returns the size of `this` expression.
    *
    * The size is the number of joins and meets
    */
  final def size: Int = this match {
    case BoolFormula.True => 0
    case BoolFormula.False => 0
    case BoolFormula.Var(_) => 0
    case BoolFormula.Not(t) => t.size
    case BoolFormula.And(t1, t2) => t1.size + t2.size + 1
    case BoolFormula.Or(t1, t2) => t1.size + t2.size + 1
  }

  /**
    * Returns a human-readable string representation of `this` expression.
    */
  override def toString: String = this match {
    case BoolFormula.True => "true"
    case BoolFormula.False => "false"
    case BoolFormula.Var(x) => s"x$x"
    case BoolFormula.Not(f) => f match {
      case BoolFormula.Var(x) => s"!x$x"
      case _ => s"!($f)"
    }
    case BoolFormula.And(f1, f2) => s"(and $f1 $f2)"
    case BoolFormula.Or(f1, f2) => s"(or $f1 $f2)"
  }

}

object BoolFormula {

  /**
    * Represents the constant ⊤.
    */
  case object True extends BoolFormula

  /**
    * Represents the constant ⊥.
    */
  case object False extends BoolFormula

  /**
    * Represents a variable. Variables are numbered by integers.
    */
  case class Var(x: Int) extends BoolFormula

  /**
    * Represents ¬f
    */
  case class Not(f: BoolFormula) extends BoolFormula

  /**
    * Represents f1 ⊓ f2
    */
  case class And(f1: BoolFormula, f2: BoolFormula) extends BoolFormula

  /**
    * Represents f1 ⊔ f2
    */
  case class Or(f1: BoolFormula, f2: BoolFormula) extends BoolFormula

  /**
    * Substitutes all variables in `f` using the substitution map `m`.
    *
    * The map `m` must bind each free variable in `f` to a (new) variable.
    */
  def substitute(f: BoolFormula, m: Bimap[Int, Int]): BoolFormula = f match {
    case True => True
    case False => False
    case Var(x) => m.getForward(x) match {
      case None => throw InternalCompilerException(s"Unexpected unbound variable: 'x$x'.", SourceLocation.Unknown)
      case Some(y) => Var(y)
    }
    case Not(f1) => Not(substitute(f1, m))
    case And(f1, f2) => And(substitute(f1, m), substitute(f2, m))
    case Or(f1, f2) => Or(substitute(f1, m), substitute(f2, m))
  }

  /**
    * Converts the given type `tpe` to a Boolean algebra expression under the given variable substitution map `m`.
    *
    * The map `m` must bind each free type variable in `tpe` to a Boolean variable.
    */
  def fromBoolType(tpe: Type, m: Bimap[VarOrEff, Int]): BoolFormula = tpe match {
    case Type.Var(sym, _) => m.getForward(VarOrEff.Var(sym)) match {
      case None => throw InternalCompilerException(s"Unexpected unbound variable: '$sym'.", sym.loc)
      case Some(x) => Var(x)
    }
    case Type.Empty => True
    case Type.All => False
    case Type.Apply(Type.Cst(TypeConstructor.Complement, _), tpe1, _) => Not(fromBoolType(tpe1, m))
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Union, _), tpe1, _), tpe2, _) => And(fromBoolType(tpe1, m), fromBoolType(tpe2, m))
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Intersection, _), tpe1, _), tpe2, _) => Or(fromBoolType(tpe1, m), fromBoolType(tpe2, m))
    case _ => throw InternalCompilerException(s"Unexpected type: '$tpe'.", tpe.loc)
  }

  /**
    * Converts the given algebraic expression `f` back to a type under the given variable substitution map `m`.
    *
    * The map `m` must bind each free variable in `f` to a type variable.
    */
  def toType(f: BoolFormula, m: Bimap[VarOrEff, Int], kind: Kind, loc: SourceLocation): Type = kind match {
    case Kind.Eff => toBoolType(f, m, loc)
    case _ => throw InternalCompilerException(s"Unexpected kind: '$kind'.", loc)
  }

  /**
    * Converts the given algebraic expression `f` back to a type under the given variable substitution map `m`.
    *
    * The map `m` must bind each free variable in `f` to a type variable.
    */
  private def toBoolType(f: BoolFormula, m: Bimap[VarOrEff, Int], loc: SourceLocation): Type = f match {
    case True => Type.Empty
    case False => Type.All
    case Var(x) => m.getBackward(x) match {
      case None => throw InternalCompilerException(s"Unexpected unbound variable: '$x'.", loc)
      case Some(VarOrEff.Var(sym)) => Type.Var(sym, loc)
      case Some(VarOrEff.Eff(sym)) => throw InternalCompilerException(s"Unexpected effect: '$sym'.", sym.loc)
    }
    case Not(f1) => Type.mkComplement(toBoolType(f1, m, loc), loc)
    case And(t1, t2) => Type.mkUnion(toBoolType(t1, m, loc), toBoolType(t2, m, loc), loc)
    case Or(t1, t2) => Type.mkIntersection(toBoolType(t1, m, loc), toBoolType(t2, m, loc), loc)
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
