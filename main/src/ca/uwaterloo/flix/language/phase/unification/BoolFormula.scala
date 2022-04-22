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
    * Converts the given type `tpe` to a Boolean formula under the given variable substitution map `m`.
    *
    * The map `m` must bind each free type variable in `tpe` to a Boolean variable.
    */
  def fromType(tpe: Type, m: Bimap[Symbol.KindedTypeVarSym, Int]): BoolFormula = tpe match {
    case Type.KindedVar(sym, _) => m.getForward(sym) match {
      case None => throw InternalCompilerException(s"Unexpected unbound variable: '$sym'.")
      case Some(x) => Var(x)
    }
    case Type.True => True
    case Type.False => False
    case Type.Apply(Type.Cst(TypeConstructor.Not, _), tpe1, _) => Neg(fromType(tpe1, m))
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.And, _), tpe1, _), tpe2, _) => Conj(fromType(tpe1, m), fromType(tpe2, m))
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Or, _), tpe1, _), tpe2, _) => Disj(fromType(tpe1, m), fromType(tpe2, m))
    case _ => throw InternalCompilerException(s"Unexpected type: '$tpe'.")
  }

  /**
    * Converts the given formula `f` back to a type under the given variable substitution map `m`.
    *
    * The map `m` must bind each free variable in `f` to a type variable.
    */
  def toType(f: BoolFormula, m: Bimap[Symbol.KindedTypeVarSym, Int], loc: SourceLocation): Type = f match {
    case True => Type.True
    case False => Type.False
    case Var(x) => m.getBackward(x) match {
      case None => throw InternalCompilerException(s"Unexpected unbound variable: '$x'.")
      case Some(sym) => Type.KindedVar(sym, loc)
    }
    case Neg(f1) => Type.mkNot(toType(f1, m, loc), loc)
    case Conj(t1, t2) => Type.mkAnd(toType(t1, m, loc), toType(t2, m, loc), loc)
    case Disj(t1, t2) => Type.mkOr(toType(t1, m, loc), toType(t2, m, loc), loc)
  }

}
