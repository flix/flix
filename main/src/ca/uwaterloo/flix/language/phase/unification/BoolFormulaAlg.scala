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

import ca.uwaterloo.flix.language.ast.{Ast, Kind, SourceLocation, Type, TypeConstructor}
import ca.uwaterloo.flix.language.phase.unification.BoolFormula.{And, False, Not, Or, True, Var}
import ca.uwaterloo.flix.util.InternalCompilerException
import ca.uwaterloo.flix.util.collection.Bimap

import scala.collection.immutable.SortedSet

/**
  * An implementation of the [[BoolAlg]] interface for [[BoolFormula]].
  */
class BoolFormulaAlg extends BoolAlg[BoolFormula] {

  override def isTrue(f: BoolFormula): Boolean = f == BoolFormula.True

  override def isFalse(f: BoolFormula): Boolean = f == BoolFormula.False

  override def isVar(f: BoolFormula): Boolean = f match {
    case Var(_) => true
    case _ => false
  }

  override def satisfiable(f: BoolFormula): Boolean = f match {
    case BoolFormula.True => true
    case BoolFormula.False => false
    case BoolFormula.Var(_) => true
    case _ => evaluateAll(f, freeVars(f).toList, List.empty)
  }

  override def mkTrue: BoolFormula = True

  override def mkFalse: BoolFormula = False

  override def mkVar(id: Int): BoolFormula = Var(id)

  override def mkNot(f: BoolFormula): BoolFormula = f match {
    case BoolFormula.True =>
      BoolFormula.False

    case BoolFormula.False =>
      BoolFormula.True

    case BoolFormula.Not(x) =>
      x

    // ¬(¬x ∨ y) => x ∧ ¬y
    case BoolFormula.Or(BoolFormula.Not(x), y) =>
      mkAnd(x, mkNot(y))

    // ¬(x ∨ ¬y) => ¬x ∧ y
    case BoolFormula.Or(x, BoolFormula.Not(y)) =>
      mkAnd(mkNot(x), y)

    case _ => BoolFormula.Not(f)
  }

  override def mkAnd(f1: BoolFormula, f2: BoolFormula): BoolFormula = (f1, f2) match {
    // T ∧ x => x
    case (BoolFormula.True, _) =>
      f2

    // x ∧ T => x
    case (_, BoolFormula.True) =>
      f1

    // F ∧ x => F
    case (BoolFormula.False, _) =>
      BoolFormula.False

    // x ∧ F => F
    case (_, BoolFormula.False) =>
      BoolFormula.False

    // ¬x ∧ (x ∨ y) => ¬x ∧ y
    case (BoolFormula.Not(x1), BoolFormula.Or(x2, y)) if x1 == x2 =>
      mkAnd(mkNot(x1), y)

    // x ∧ ¬x => F
    case (x1, BoolFormula.Not(x2)) if x1 == x2 =>
      BoolFormula.False

    // ¬x ∧ x => F
    case (BoolFormula.Not(x1), x2) if x1 == x2 =>
      BoolFormula.False

    // (x ∧ y) ∧ x) => (x ∧ y)
    case (BoolFormula.And(x1, y), x2) if x1 == x2 =>
      mkAnd(x1, y)

    // (x ∧ y) ∧ y) => (x ∧ y)
    case (BoolFormula.And(x, y1), y2) if y1 == y2 =>
      mkAnd(x, y1)

    // x ∧ x => x
    case _ if f1 == f2 => f1

    case _ => BoolFormula.And(f1, f2)
  }

  override def mkOr(f1: BoolFormula, f2: BoolFormula): BoolFormula = (f1, f2) match {
    // T ∨ x => T
    case (BoolFormula.True, _) =>
      BoolFormula.True

    // F ∨ y => y
    case (BoolFormula.False, _) =>
      f2

    // x ∨ T => T
    case (_, BoolFormula.True) =>
      BoolFormula.True

    // x ∨ F => x
    case (_, BoolFormula.False) =>
      f1

    // ¬x ∨ x => T
    case (BoolFormula.Not(x), y) if x == y =>
      BoolFormula.True

    // x ∨ ¬x => T
    case (x, BoolFormula.Not(y)) if x == y =>
      BoolFormula.True

    case _ => BoolFormula.Or(f1, f2)
  }

  override def map(f: BoolFormula)(fn: Int => BoolFormula): BoolFormula = f match {
    case True => True
    case False => False
    case And(f1, f2) => mkAnd(map(f1)(fn), map(f2)(fn))
    case Or(f1, f2) => mkOr(map(f1)(fn), map(f2)(fn))
    case Not(f1) => mkNot(map(f1)(fn))
    case Var(sym) => fn(sym)
  }

  override def toType(f: BoolFormula, env: Bimap[BoolFormula.IrreducibleEff, Int]): Type = f match {
    case BoolFormula.True => Type.True
    case BoolFormula.False => Type.False
    case BoolFormula.And(f1, f2) => Type.mkAnd(toType(f1, env), toType(f2, env), SourceLocation.Unknown)
    case BoolFormula.Or(f1, f2) => Type.mkOr(toType(f1, env), toType(f2, env), SourceLocation.Unknown)
    case BoolFormula.Not(f1) => Type.mkNot(toType(f1, env), SourceLocation.Unknown)
    case BoolFormula.Var(id) => env.getBackward(id) match {
      case Some(BoolFormula.IrreducibleEff.Var(sym)) => Type.Var(sym, SourceLocation.Unknown)
      case Some(BoolFormula.IrreducibleEff.Eff(sym)) => Type.Cst(TypeConstructor.Effect(sym), SourceLocation.Unknown)
      case Some(BoolFormula.IrreducibleEff.Assoc(sym, arg)) => Type.AssocType(Ast.AssocTypeConstructor(sym, SourceLocation.Unknown), arg, Kind.Eff, SourceLocation.Unknown)
      case Some(BoolFormula.IrreducibleEff.JvmToEff(t)) => t
      case None => throw InternalCompilerException(s"unexpected unknown ID: $id", SourceLocation.Unknown)
    }
  }

  override def freeVars(f: BoolFormula): SortedSet[Int] = f.freeVars

  override def minimize(f: BoolFormula): BoolFormula = f

  /**
    * Enumerates all assignments to `f` and checks if one of them is satisfiable.
    */
  private def evaluateAll(f: BoolFormula, l: List[Int], env: List[Int]): Boolean = l match {
    case Nil =>
      // All variables are bound. Compute the truth value.
      evaluate(f, env)
    case x :: xs =>
      // Recurse on two cases: x = false and x = true.
      evaluateAll(f, xs, env) || evaluateAll(f, xs, x :: env)
  }

  /**
    * Computes the truth value of the formula `f` assuming the variables in `trueVars`
    * are true and the rest are false.
    */
  private def evaluate(f: BoolFormula, trueVars: List[Int]): Boolean = f match {
    case True => true
    case False => false
    case Var(x) => trueVars.contains(x)
    case Not(f1) => !evaluate(f1, trueVars)
    case Or(f1, f2) => evaluate(f1, trueVars) || evaluate(f2, trueVars)
    case And(f1, f2) => evaluate(f1, trueVars) && evaluate(f2, trueVars)
  }

}
