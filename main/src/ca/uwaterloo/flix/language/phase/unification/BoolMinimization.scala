/*
 *  Copyright 2022 Jonathan Lindegaard Starup
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.language.ast.Type

object BoolMinimization {

  sealed trait Formula

  object Formula {

    case object True extends Formula

    case object False extends Formula

    case class Var(v: Type.Var) extends Formula

    case class Not(term: Formula) extends Formula

    case class And(terms: List[Formula]) extends Formula

    case class Or(terms: List[Formula]) extends Formula

  }

  // TODO: The `mkX` functions could also simplify cases like
  //       `mkAnd(True, x)` to `x` etc.

  /**
    * Cancels the double negation if `f` is a direct negation.
    */
  def mkNot(f: Formula): Formula = {
    import Formula._
    f match {
      case Not(term) => term
      case True | False | _: Var | _: And | _: Or => Not(f)
    }
  }

  /**
    * Merges the conjunctions if `f1` or `f2` are conjunctions themselves.
    */
  def mkAnd(f1: Formula, f2: Formula): Formula = {
    import Formula._
    (f1, f2) match {
      case (And(ts1), And(ts2)) => And(ts1 ++ ts2)
      case (And(ts), _) => And(f2 :: ts)
      case (_, And(ts)) => And(f1 :: ts)
      case (_, _) => And(List(f1, f2))
    }
  }

  /**
    * Merges the conjunctions in `f` into the new conjunction.
    */
  def mkAnd(f: List[Formula]): Formula = {
    import Formula._
    val hoistedTerms = f.flatMap(f0 => f0 match {
      case And(terms) => terms
      case True | False | Var(_) | Not(_) | Or(_) => List(f0)
    })
    And(hoistedTerms)
  }

  /**
    * Merges the disjunctions if `f1` or `f2` are disjunctions themselves.
    */
  def mkOr(f1: Formula, f2: Formula): Formula = {
    import Formula._
    (f1, f2) match {
      case (Or(ts1), Or(ts2)) => Or(ts1 ++ ts2)
      case (Or(ts), _) => Or(f2 :: ts)
      case (_, Or(ts)) => Or(f1 :: ts)
      case (_, _) => Or(List(f1, f2))
    }
  }

  /**
    * Merges the disjunctions in `f` into the new disjunction.
    */
  def mkOr(f: List[Formula]): Formula = {
    import Formula._
    val hoistedTerms = f.flatMap(f0 => f0 match {
      case Or(terms) => terms
      case True | False | Var(_) | Not(_) | And(_) => List(f0)
    })
    Or(hoistedTerms)
  }

  /**
    * Transform `f` to an equivalent formula where negation only appears
    * on variables directly.
    */
  def toNNF(f: Formula): Formula = {
    import Formula._
    f match {
      case True => True

      case False => False

      case Var(v) => Var(v)

      case not: Not => not.term match {
        case True => False
        case False => True
        case Var(v) => Not(Var(v))
        case Not(term) => toNNF(term)
        case And(terms) => mkOr(terms.map(t => toNNF(mkNot(t))))
        case Or(terms) => mkAnd(terms.map(t => toNNF(mkNot(t))))
      }

      case And(terms) => mkAnd(terms.map(toNNF))

      case Or(terms) => mkOr(terms.map(toNNF))

    }
  }

  /**
    * Transform `f` to an equivalent formula that is a conjunction of
    * disjunctions of either variables or negated variables.
    * Ex. `(x ∨ y) ∧ (z ∨ ¬y ∨ ¬z) ∧ (x)`
    */
  def toCNF(f: Formula): Formula = ???

  /**
    * Transform `f` to an equivalent formula that is a disjunction of
    * conjunctions of either variables or negated variables.
    * Ex. `(x ∧ y) ∨ (z ∧ ¬y ∧ ¬z) ∨ (x)`
    */
  def toDNF(f: Formula): Formula = ???

}
