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

import ca.uwaterloo.flix.language.ast.{Kind, Rigidity, SourceLocation, Type}

object BooleanMinimization {

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
    * Merges the conjunctions if arguments are conjunctions themselves.
    */
  def mkAnd(f: Formula*): Formula = {
    mkAnd(f.toList)
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
    * Merges the disjunctions if arguments are disjunctions themselves.
    */
  def mkOr(f: Formula*): Formula = {
    mkOr(f.toList)
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

  def main(args: Array[String]): Unit = {
    import Formula._
    def mkVar(s: String): Var = Var(Type.KindedVar(s.hashCode, Kind.Bool, SourceLocation.Unknown, Rigidity.Flexible, Some(s)))

    val formulas = List(
      mkAnd(mkVar("x"), mkVar("y")),
      mkOr(mkVar("y"), mkVar("z")),
      mkAnd(mkOr(mkVar("x"), mkVar("y")), mkOr(mkVar("x"), mkVar("z"))),
      mkNot(mkAnd(mkOr(mkVar("x"), mkNot(mkVar("y"))), mkNot(mkOr(mkVar("x"), mkVar("z"))))),
    )

    def run(msg: String, t: Formula => Formula): Unit = {
      println(msg)
      formulas.foreach(f => {
        println("----")
        println(show(f))
        println(show(t(f)))
      })
    }

    run("NNF", toNNF)
  }

  def show(f: Formula): String = {
    def showAux(f: Formula): (String, Boolean) = {
      def showTerm(term: Formula): String = {
        val (termStr, paran) = showAux(term)
        if (paran) s"($termStr)" else termStr
      }

      f match {
        case Formula.True => ("T", false)
        case Formula.False => ("F", false)
        case Formula.Var(v) => (v.text.getOrElse("???"), false)
        case Formula.Not(term) =>
          val (termStr, paran) = showAux(term)
          val rep = if (paran) s"($termStr)" else termStr
          (s"¬$rep", false)
        case Formula.And(terms) =>
          val rep = terms.map(showTerm).mkString(" ∧ ")
          (rep, true)
        case Formula.Or(terms) =>
          val rep = terms.map(showTerm).mkString(" ∨ ")
          (rep, true)
      }
    }

    showAux(f)._1
  }

}
