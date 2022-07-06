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

import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.util.InternalCompilerException
import ca.uwaterloo.flix.util.collection.Bimap

import scala.collection.immutable.SortedSet

/**
  * A common super-type for Boolean formulas.
  */
sealed trait SetFormula {

  /**
    * Returns the free variables in `this` formula.
    */
  final def freeVars: SortedSet[Int] = this match {
    case SetFormula.All => SortedSet.empty
    case SetFormula.Empty => SortedSet.empty
    case SetFormula.Var(x) => SortedSet(x)
    case SetFormula.Complement(f) => f.freeVars
    case SetFormula.Intersection(f1, f2) => f1.freeVars ++ f2.freeVars
    case SetFormula.Union(f1, f2) => f1.freeVars ++ f2.freeVars
  }

  /**
    * Returns the size of `this` formulas.
    *
    * The size is the number of conjunctions and disjunctions.
    */
  final def size: Int = this match {
    case SetFormula.All => 0
    case SetFormula.Empty => 0
    case SetFormula.Var(_) => 0
    case SetFormula.Complement(t) => t.size
    case SetFormula.Intersection(t1, t2) => t1.size + t2.size + 1
    case SetFormula.Union(t1, t2) => t1.size + t2.size + 1
  }

  /**
    * Returns a human-readable string representation of `this` formula.
    */
  override def toString: String = this match {
    case SetFormula.All => "true"
    case SetFormula.Empty => "false"
    case SetFormula.Var(x) => s"x$x"
    case SetFormula.Complement(f) => f match {
      case SetFormula.Var(x) => s"!x$x"
      case _ => s"!($f)"
    }
    case SetFormula.Intersection(f1, f2) => s"(and $f1 $f2)"
    case SetFormula.Union(f1, f2) => s"(or $f1 $f2)"
  }

}

object SetFormula {

  /**
    * Represents the constant All.
    */
  case object All extends SetFormula

  /**
    * Represents the constant Empty.
    */
  case object Empty extends SetFormula

  /**
    * Represents a variable. Variables are numbered by integers.
    */
  case class Var(x: Int) extends SetFormula

  /**
    * Represents the negation of the formula `f`.
    */
  case class Complement(f: SetFormula) extends SetFormula

  /**
    * Represents the conjunction (logical and) of `f1` and `f2`.
    */
  case class Intersection(f1: SetFormula, f2: SetFormula) extends SetFormula

  /**
    * Represents the disjunction (logical or) of `f1` and `f2`.
    */
  case class Union(f1: SetFormula, f2: SetFormula) extends SetFormula

  /**
    * Substitutes all variables in `f` using the substitution map `m`.
    *
    * The map `m` must bind each free variable in `f` to a (new) variable.
    */
  def substitute(f: SetFormula, m: Bimap[Int, Int]): SetFormula = f match {
    case All => All
    case Empty => Empty
    case Var(x) => m.getForward(x) match {
      case None => throw InternalCompilerException(s"Unexpected unbound variable: 'x$x'.")
      case Some(y) => Var(y)
    }
    case Complement(f1) => Complement(substitute(f1, m))
    case Intersection(f1, f2) => Intersection(substitute(f1, m), substitute(f2, m))
    case Union(f1, f2) => Union(substitute(f1, m), substitute(f2, m))
  }

  /**
    * Converts the given type `tpe` to a Boolean formula under the given variable substitution map `m`.
    *
    * The map `m` must bind each free type variable in `tpe` to a Boolean variable.
    */
  def fromType(tpe: Type, m: Bimap[Constant, Int]): SetFormula = tpe match {
    case Type.KindedVar(sym, _) => m.getForward(Constant.Var(sym)) match {
      case None => throw InternalCompilerException(s"Unexpected unbound variable: '$sym'.")
      case Some(x) => Var(x)
    }
    case Type.Cst(TypeConstructor.Effect(sym), _) => m.getForward(Constant.Eff(sym)) match {
      case None => throw InternalCompilerException(s"Unexpected unbound variable: '$sym'.")
      case Some(x) => Var(x)
    }
    case Type.All => All
    case Type.Empty => Empty
    case Type.Apply(Type.Cst(TypeConstructor.Complement, _), tpe1, _) => Complement(fromType(tpe1, m))
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Intersection, _), tpe1, _), tpe2, _) => Intersection(fromType(tpe1, m), fromType(tpe2, m))
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Union, _), tpe1, _), tpe2, _) => Union(fromType(tpe1, m), fromType(tpe2, m))
    case _ => throw InternalCompilerException(s"Unexpected type: '$tpe'.")
  }

  /**
    * Converts the given formula `f` back to a type under the given variable substitution map `m`.
    *
    * The map `m` must bind each free variable in `f` to a type variable.
    */
  def toType(f: SetFormula, m: Bimap[Constant, Int], loc: SourceLocation): Type = f match {
    case All => Type.All
    case Empty => Type.Empty
    case Var(x) => m.getBackward(x) match {
      case None => throw InternalCompilerException(s"Unexpected unbound variable: '$x'.")
      case Some(Constant.Var(sym)) => Type.KindedVar(sym, loc)
      case Some(Constant.Eff(sym)) => Type.Cst(TypeConstructor.Effect(sym), loc)
    }
    case Complement(f1) => Type.mkComplement(toType(f1, m, loc), loc)
    case Intersection(t1, t2) => Type.mkIntersection(toType(t1, m, loc), toType(t2, m, loc), loc)
    case Union(t1, t2) => Type.mkUnion(toType(t1, m, loc), toType(t2, m, loc), loc)
  }

  // MATT docs
  // MATT name
  sealed trait Constant
  object Constant {
    case class Var(sym: Symbol.KindedTypeVarSym) extends Constant
    case class Eff(sym: Symbol.EffectSym) extends Constant
  }
}

