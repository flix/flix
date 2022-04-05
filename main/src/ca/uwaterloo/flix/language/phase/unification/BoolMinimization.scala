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

import ca.uwaterloo.flix.language.ast.SourceLocation.Unknown
import ca.uwaterloo.flix.language.ast.{Kind, Rigidity, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.util.InternalCompilerException

import scala.annotation.tailrec

object BoolMinimization {

  /**
    * Minimizes the Boolean formula given by `t`.
    */
  def minimize(t: Type): Type = {
    val res = toType(toCNF(fromType(t)))
    val prints = false
    if (prints) {
      val before = t.size
      val after = res.size
      if (before > 9 || after > 9) {
        val goodBad = if (after <= before) "+" else " "
        println(s"$goodBad $before cnf'ed to $after")
      }
    }
    res
  }

  private sealed trait Formula

  private object Formula {

    case object True extends Formula

    case object False extends Formula

    case class Var(v: Type.KindedVar) extends Formula

    case class Not(term: Formula) extends Formula

    /**
      * A conjunction of terms. And empty list is `True`.
      */
    case class And(terms: List[Formula]) extends Formula

    /**
      * A disjunction of terms. And empty list is `True`.
      */
    case class Or(terms: List[Formula]) extends Formula

  }

  /**
    * Sort terms `True` -> `False` -> `Var`s (by name then negation) ->
    * complex negation -> conjunctions -> disjunctions.
    */
  private def sortTerms(terms: List[Formula]): List[Formula] = {
    import Formula._
    terms.sortWith {
      case (True, True) => false
      case (True, False | Var(_) | Not(_) | And(_) | Or(_)) => true

      case (False, True | False) => false
      case (False, Var(_) | Not(_) | And(_) | Or(_)) => true

      case (Var(_), True | False) => false

      case (Var(v1), Var(v2)) => v1 < v2
      case (Var(v1), Not(Var(v2))) if v1 == v2 => true
      case (Var(v1), Not(Var(v2))) => v1 < v2
      case (Not(Var(v1)), Var(v2)) if v1 == v2 => false
      case (Not(Var(v1)), Var(v2)) => v1 < v2
      case (Not(Var(v1)), Not(Var(v2))) => v1 < v2
      case (Var(_), Not(_) | And(_) | Or(_)) => true

      case (Not(_), True | False | Var(_)) => false
      case (Not(_), Not(_)) => false
      case (Not(_), And(_) | Or(_)) => true

      case (And(_), True | False | Var(_) | Not(_)) => false
      case (And(_), And(_)) => false
      case (And(_), Or(_)) => true

      case (Or(_), True | False | Var(_) | Not(_) | And(_) | Or(_)) => false
    }
  }

  /**
    * Reduces things like `y ∨ y` to `y` or `¬x ∧ ¬x` to `x`. Returns `None` if
    * the list contains both `x` and `¬x` for some `x`.
    * Assumes the list is sorted.
    */
  private def removeSimpleDuplicates(terms: List[Formula]): Option[List[Formula]] = {
    @tailrec
    def aux(terms0: List[Formula], acc: List[Formula]): Option[List[Formula]] = {
      import Formula._
      terms0 match {
        case Nil =>
          Some(acc.reverse)
        case last :: Nil =>
          Some((last :: acc).reverse)
        case Var(v1) :: Var(v2) :: rest if v1 == v2 =>
          aux(Var(v2) :: rest, acc)
        case Not(Var(v1)) :: Not(Var(v2)) :: rest if v1 == v2 =>
          aux(Not(Var(v2)) :: rest, acc)
        case Var(v1) :: Not(Var(v2)) :: _ if v1 == v2 =>
          None
        case other :: rest =>
          aux(rest, other :: acc)
      }
    }

    aux(terms, Nil)
  }

  /**
    * Performs unit propagation on `terms` of a conjunction. If `x` or `¬y` is
    * present, then they must be `True` and `False` respectively. This is
    * substituted in the remaining conjunction.
    */
  private def unitPropagation(terms: List[Formula]): Option[List[Formula]] = {
    import Formula._
    /**
      * Substitutes the `bindings` given on the formula `f`
      */
    def subst(f: Formula, bindings: Map[Var, Formula]): Formula = f match {
      case True => True
      case False => False
      case v: Var => bindings.getOrElse(v, v)
      case Not(term) => mkNot(subst(term, bindings))
      case And(terms) => mkAnd(terms.map(t => subst(t, bindings)))
      case Or(terms) => mkOr(terms.map(t => subst(t, bindings)))
    }

    @tailrec
    def aux(terms0: List[Formula], bindings: Map[Formula.Var, Formula], acc: List[Formula]): List[Formula] = terms0 match {
      case Nil =>
        acc.reverse
      case (v: Var) :: rest if bindings.contains(v) =>
        aux(subst(v, bindings) :: rest, bindings, acc)
      case (v: Var) :: rest =>
        aux(rest, bindings + (v -> True), v :: acc)
      case Not(v: Var) :: rest if bindings.contains(v) =>
        aux(subst(Not(v), bindings) :: rest, bindings, acc)
      case Not(v: Var) :: rest =>
        aux(rest, bindings + (v -> False), Not(v) :: acc)
      case f :: rest if bindings.nonEmpty =>
        val replacedF = subst(f, bindings)
        replacedF match {
          case True =>
            aux(rest, bindings, acc)
          case False =>
            // false shouldn't occur here so shortcutting doesn't matter
            aux(rest, bindings, False :: acc)
          case v: Var if bindings.contains(v) =>
            aux(subst(v, bindings) :: rest, bindings, acc)
          case v: Var =>
            aux(rest, bindings + (v -> True), v :: acc)
          case Not(v: Var) =>
            aux(rest, bindings + (v -> False), Not(v) :: acc)
          case Not(_) | And(_) | Or(_) =>
            aux(rest, bindings, replacedF :: acc)
        }
      case f :: rest => aux(rest, bindings, f :: acc)
    }

    val propagatedTerms = aux(terms, Map.empty, Nil)
    // This last normalization is for sorting
    normalizeTerms(propagatedTerms)
  }

  /**
    * Build `Not(f)` and performs shallow minimization.
    */
  private def mkNot(f: Formula): Formula = {
    import Formula._
    f match {
      case True => False
      case False => True
      case Var(v) => Not(Var(v))
      case Not(term) => term
      case And(terms) => Not(And(terms))
      case Or(terms) => Not(Or(terms))
    }
  }

  /**
    * Merges the conjunctions if arguments are conjunctions themselves and
    * performs shallow minimization along with unit propagation.
    */
  private def mkAnd(f: Formula*): Formula = {
    mkAnd(f.toList)
  }

  /**
    * Merges the conjunctions in `f` into the new conjunction and performs
    * shallow minimization.
    */
  private def mkAnd(f: List[Formula]): Formula = {
    import Formula._
    val baseValue: Option[List[Formula]] = Some(Nil)
    val hoistedTerms = f.foldRight(baseValue) {
      case (term, opt) => opt.flatMap(
        acc => term match {
          case True => Some(acc)
          case False => None
          case v: Var => Some(v :: acc)
          case n: Not => Some(n :: acc)
          case And(terms) => Some(terms ++ acc)
          case or: Or => Some(or :: acc)
        }
      )
    }
    val finalTerms = hoistedTerms
      .flatMap(normalizeTerms)
      .flatMap(unitPropagation)
    finalTerms match {
      case None => False
      case Some(term :: Nil) => term
      case Some(terms) => And(terms)
    }
  }

  /**
    * Sorts terms, reduces simple duplicates and returns `None` if the terms
    * contains a simple contradiction.
    */
  private def normalizeTerms(terms: List[Formula]): Option[List[Formula]] = {
    removeSimpleDuplicates(sortTerms(terms))
  }

  /**
    * Merges the disjunctions if arguments are disjunctions themselves and
    * performs shallow minimization.
    */
  private def mkOr(f: Formula*): Formula = {
    mkOr(f.toList)
  }

  /**
    * Merges the disjunctions in `f` into the new disjunction and performs
    * shallow minimization.
    */
  private def mkOr(f: List[Formula]): Formula = {
    import Formula._
    val baseValue: Option[List[Formula]] = Some(Nil)
    val hoistedTerms = f.foldRight(baseValue) {
      case (term, opt) => opt.flatMap(
        acc => term match {
          case True => None
          case False => Some(acc)
          case v: Var => Some(v :: acc)
          case n: Not => Some(n :: acc)
          case and: And => Some(and :: acc)
          case Or(terms) => Some(terms ++ acc)
        }
      )
    }
    hoistedTerms match {
      case None => True
      case Some(term :: Nil) => term
      case Some(terms) =>
        normalizeTerms(terms) match {
          case None => True
          case Some(normalizedTerms) => Or(normalizedTerms)
        }
    }
  }

  /**
    * Transform `f` to an equivalent formula where negation only appears
    * on variables directly.
    */
  private def toNNF(f: Formula): Formula = {
    import Formula._
    f match {
      case True => True

      case False => False

      case Var(v) => Var(v)

      case not: Not => not.term match {
        case True => False
        case False => True
        case Var(v) => mkNot(Var(v))
        case Not(term) => term
        case And(terms) => toNNF(mkOr(terms.map(t => mkNot(t))))
        case Or(terms) => toNNF(mkAnd(terms.map(t => mkNot(t))))
      }

      case And(terms) => mkAnd(terms.map(toNNF))

      case Or(terms) => mkOr(terms.map(toNNF))

    }
  }

  /**
    * Transform `f` to an equivalent formula that is a conjunction of
    * disjunctions of either variables or negated variables.
    * Ex. `x ∧ (x ∨ y) ∧ (z ∨ ¬y ∨ ¬z)`
    */
  private def toCNF(f: Formula): Formula = {
    def aux(f0: Formula): Formula = {
      import Formula._
      f0 match {
        case True => True
        case False => False
        case Var(v) => Var(v)
        case Not(Var(v)) => Not(Var(v))
        case Not(_) => throw InternalCompilerException(s"Found complex negation after NNF ${show(f0)}")
        case And(terms) => mkAnd(terms.map(toCNF))
        case Or(terms) =>
          // Note: this code relies on the ordering of simple terms before conjunctions
          terms.foldLeft(False: Formula)(
            (cnf, term) => term match {
              case True => mkOr(cnf, True)
              case False => mkOr(cnf, False)
              case Var(v) => mkOr(cnf, Var(v))
              case Not(Var(v)) => mkOr(cnf, Not(Var(v)))
              case Not(_) => throw InternalCompilerException(s"Found complex negation after NNF ${show(term)}")
              case And(terms0) => toCNF(mkAnd(terms0.map(t => mkOr(cnf, t))))
              case Or(_) => throw InternalCompilerException(s"Found directly nested Or: ${show(f0)}")
            })
      }
    }

    aux(toNNF(f))
  }

  /**
    * Transform `f` to an equivalent formula that is a disjunction of
    * conjunctions of either variables or negated variables.
    * Ex. `x ∨ (x ∧ y) ∨ (z ∧ ¬y ∧ ¬z)`
    */
  private def toDNF(f: Formula): Formula = ???

  private def fromType(t: Type): Formula = {
    import Formula._
    t match {
      case Type.True => True
      case Type.False => False
      case v: Type.KindedVar => Var(v)
      case Type.Apply(Type.Cst(TypeConstructor.Not, _), t0, _) =>
        mkNot(fromType(t0))
      case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.And, _), t1, _), t2, _) =>
        mkAnd(fromType(t1), fromType(t2))
      case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Or, _), t1, _), t2, _) =>
        mkOr(fromType(t1), fromType(t2))
      case other =>
        throw InternalCompilerException(s"Unexpected non-boolean type ${other.toString}")
    }
  }

  private def toType(f: Formula): Type = {
    import Formula._
    f match {
      case True => Type.True
      case False => Type.False
      case Var(v) => v
      case Not(term) => Type.mkNot(toType(term), Unknown)
      case And(terms) => terms match {
        case Nil => Type.True
        case fst :: rest => rest.foldLeft(toType(fst)) {
          (acc, f) => Type.mkAnd(acc, toType(f), Unknown)
        }
      }
      case Or(terms) => terms match {
        case Nil => Type.True
        case fst :: rest => rest.foldLeft(toType(fst)) {
          (acc, f) => Type.mkOr(acc, toType(f), Unknown)
        }
      }
    }
  }

  /**
    * A couple tests for NNF and CNF
    */
  def main(args: Array[String]): Unit = {
    import Formula._
    def mkVar(s: String): Var = Var(
      Type.KindedVar(
        new Symbol.KindedTypeVarSym(
          s.hashCode,
          Some(s),
          Kind.Bool,
          Rigidity.Flexible,
          Unknown
        ),
        Unknown
      )
    )

    val formulas = List(
      (mkAnd(mkVar("x"), mkVar("y")),
        "x ∧ y"),
      (mkOr(mkVar("y"), mkVar("z")),
        "y ∨ z"),
      (mkAnd(mkOr(mkVar("q"), mkAnd(mkVar("q"), mkVar("h"))), mkNot(mkVar("x")), mkVar("y"), mkVar("x")),
        "(q ∨ (q ∧ h)) ∧ ¬x ∧ y ∧ x"),
      (mkAnd(mkOr(mkVar("x"), mkVar("y")), mkOr(mkVar("x"), mkVar("z"))),
        "(x ∨ y) ∧ (x ∨ z)"),
      (mkNot(mkAnd(mkOr(mkVar("x"), mkNot(mkVar("y"))), mkNot(mkOr(mkVar("x"), mkVar("z"))))),
        "¬((x ∨ ¬y) ∧ ¬(x ∨ z))"),
      (mkAnd(mkOr(mkVar("z"), mkNot(mkAnd(mkVar("z"), mkVar("Q")))), mkNot(mkAnd(mkOr(mkVar("x"), mkNot(mkVar("y"))), mkNot(mkOr(mkVar("x"), mkVar("z")))))),
        "(z ∨ ¬(z ∧ Q)) ∧ ¬((x ∨ ¬y) ∧ ¬(x ∨ z))"),
      (mkAnd(mkVar("x"), mkOr(mkVar("y"), mkVar("x"))),
        "x ∧ (y ∨ x)")
    )

    def run(msg: String, t: Formula => Formula): Unit = {
      println(msg)
      val delim = "----"
      formulas.foreach(f => {
        println(delim)
        println(f._2)
        println(show(f._1))
        println(show(t(f._1)))
      })
      println(delim)
    }

    run("NNF", toNNF)
    run("CNF", toCNF)
  }

  /**
    * Creates a string representation of the formula.
    */
  private def show(f: Formula): String = {
    def showAux(f: Formula): (String, Boolean) = {
      def showTermBraces(term: Formula): String = {
        val (termStr, paran) = showAux(term)
        if (paran) s"($termStr)" else termStr
      }

      import Formula._
      f match {
        case True => ("T", false)
        case False => ("F", false)
        case Var(v) => (v.sym.text.getOrElse("???"), false)
        case Not(term) =>
          val (termStr, paran) = showAux(term)
          val rep = if (paran) s"($termStr)" else termStr
          (s"¬$rep", false)
        case And(terms) =>
          val rep = terms.map(showTermBraces).mkString(" ∧ ")
          (rep, true)
        case Or(terms) =>
          val rep = terms.map(showTermBraces).mkString(" ∨ ")
          (rep, true)
      }
    }

    showAux(f)._1
  }

}
