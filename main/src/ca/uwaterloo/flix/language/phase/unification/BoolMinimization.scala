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

import ca.uwaterloo.flix.language.ast.Ast.VarText
import ca.uwaterloo.flix.language.ast.SourceLocation.Unknown
import ca.uwaterloo.flix.language.ast.{Kind, Rigidity, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.util.InternalCompilerException

object BoolMinimization {

  /**
    * Minimizes the Boolean formula given by `t`.
    */
  def minimize(t: Type): Type = {
    val formula = Formula.fromType(t)
    val nnf = Formula.toNNF(formula)
    val cnf = toCNF(nnf)
    val res = FormulaNNF.toType(cnf)
    // TODO remove this
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

    /**
      * Build `Not(f)` and performs shallow minimization.
      */
    def mkNot(f: Formula): Formula = {
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
    def mkAnd(f: Formula*): Formula = {
      mkAnd(f.toList)
    }

    /**
      * Merges the conjunctions in `f` into the new conjunction and performs
      * shallow minimization.
      */
    def mkAnd(f: List[Formula]): Formula = {
      val baseValue: Option[List[Formula]] = Some(Nil)
      // TODO use something with constant time concat
      val hoistedTerms = f.foldRight(baseValue) {
        case (term, opt) => opt.flatMap(
          acc => term match {
            case True => Some(acc)
            case False => None
            case And(terms) => Some(terms ++ acc)
            case Var(_) | Not(_) | Or(_) => Some(term :: acc)
          }
        )
      }
      hoistedTerms match {
        case None => False
        case Some(term :: Nil) => term
        case Some(terms) => And(terms)
      }
    }

    /**
      * Merges the disjunctions if arguments are disjunctions themselves and
      * performs shallow minimization.
      */
    def mkOr(f: Formula*): Formula = {
      mkOr(f.toList)
    }

    /**
      * Merges the disjunctions in `f` into the new disjunction and performs
      * shallow minimization.
      */
    def mkOr(f: List[Formula]): Formula = {
      val baseValue: Option[List[Formula]] = Some(Nil)
      val hoistedTerms = f.foldRight(baseValue) {
        case (term, opt) => opt.flatMap(
          acc => term match {
            case True => None
            case False => Some(acc)
            case Or(terms) => Some(terms ++ acc)
            case Var(_) | Not(_) | And(_) => Some(term :: acc)
          }
        )
      }
      hoistedTerms match {
        case None => True
        case Some(term :: Nil) => term
        case Some(terms) => Or(terms)
      }
    }

    /**
      * Transform `f` to an equivalent formula where negation only appears
      * on variables directly.
      */
    def toNNF(f: Formula): FormulaNNF = {
      f match {
        case True => FormulaNNF.True

        case False => FormulaNNF.False

        case Var(v) => FormulaNNF.Var(v)

        case not: Not => not.term match {
          case True => FormulaNNF.False
          case False => FormulaNNF.True
          case Var(v) => FormulaNNF.NotVar(v)
          case Not(term) => toNNF(term)
          case And(terms) => toNNF(mkOr(terms.map(t => mkNot(t))))
          case Or(terms) => toNNF(mkAnd(terms.map(t => mkNot(t))))
        }

        case And(terms) => FormulaNNF.mkAnd(terms.map(toNNF))

        case Or(terms) => FormulaNNF.mkOr(terms.map(toNNF))

      }
    }

    def fromType(t: Type): Formula = {
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

  }

  private sealed trait FormulaNNF

  private object FormulaNNF {

    case object True extends FormulaNNF

    case object False extends FormulaNNF

    case class Var(v: Type.KindedVar) extends FormulaNNF

    case class NotVar(v: Type.KindedVar) extends FormulaNNF

    /**
      * A conjunction of terms. And empty list is `True`.
      */
    case class And(terms: List[FormulaNNF]) extends FormulaNNF

    /**
      * A disjunction of terms. And empty list is `True`.
      */
    case class Or(terms: List[FormulaNNF]) extends FormulaNNF


    /**
      * Merges the conjunctions if arguments are conjunctions themselves and
      * performs shallow minimization along with unit propagation.
      */
    def mkAnd(f: FormulaNNF*): FormulaNNF = {
      mkAnd(f.toList)
    }

    /**
      * Merges the conjunctions in `f` into the new conjunction and performs
      * shallow minimization.
      */
    def mkAnd(f: List[FormulaNNF]): FormulaNNF = {
      val baseValue: Option[List[FormulaNNF]] = Some(Nil)
      // TODO use something with constant time concat
      val hoistedTerms = f.foldRight(baseValue) {
        case (term, opt) => opt.flatMap(
          acc => term match {
            case True => Some(acc)
            case False => None
            case And(terms) => Some(terms ++ acc)
            case Var(_) | NotVar(_) | Or(_) => Some(term :: acc)
          }
        )
      }
      hoistedTerms match {
        case None => False
        case Some(term :: Nil) => term
        case Some(terms) => And(terms)
      }
    }

    /**
      * Merges the disjunctions if arguments are disjunctions themselves and
      * performs shallow minimization.
      */
    def mkOr(f: FormulaNNF*): FormulaNNF = {
      mkOr(f.toList)
    }

    /**
      * Merges the disjunctions in `f` into the new disjunction and performs
      * shallow minimization.
      */
    def mkOr(f: List[FormulaNNF]): FormulaNNF = {
      val baseValue: Option[List[FormulaNNF]] = Some(Nil)
      val hoistedTerms = f.foldRight(baseValue) {
        case (term, opt) => opt.flatMap(
          acc => term match {
            case True => None
            case False => Some(acc)
            case Or(terms) => Some(terms ++ acc)
            case Var(_) | NotVar(_) | And(_) => Some(term :: acc)
          }
        )
      }
      hoistedTerms match {
        case None => True
        case Some(term :: Nil) => term
        case Some(terms) => Or(terms)
      }
    }

    def toType(f: FormulaNNF): Type = {
      f match {
        case True => Type.True
        case False => Type.False
        case Var(v) => v
        case NotVar(v) => Type.mkNot(v, Unknown)
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

  }

  /**
    * Transform `f` to an equivalent formula that is a conjunction of
    * disjunctions of either variables or negated variables.
    * Ex. `x ∧ (x ∨ y) ∧ (z ∨ ¬y ∨ ¬z)`
    */
  private def toCNF(f: FormulaNNF): FormulaNNF = {
    import FormulaNNF._
    f match {
      case True => True
      case False => False
      case Var(v) => Var(v)
      case NotVar(v) => NotVar(v)
      case And(terms) => mkAnd(terms.map(toCNF))
      case Or(terms) =>
        // Note: this code relies on the ordering of simple terms before conjunctions
        terms.foldLeft(False: FormulaNNF)(
          (cnf, term) => term match {
            case True => mkOr(cnf, True)
            case False => mkOr(cnf, False)
            case Var(v) => mkOr(cnf, Var(v))
            case NotVar(v) => mkOr(cnf, NotVar(v))
            case And(terms0) => toCNF(mkAnd(terms0.map(t => mkOr(cnf, t))))
            case Or(_) => throw InternalCompilerException(s"Found directly nested disjunction")
          })
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
          VarText.SourceText(s),
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

    val delim = "----"

    println("NNF")
    formulas.foreach(f => {
      println(delim)
      println(f._2)
      println(show(f._1))
      println(show(toNNF(f._1)))
    })
    println(delim)

    println("CNF")
    formulas.foreach(f => {
      println(delim)
      println(f._2)
      println(show(f._1))
      println(show(toCNF(toNNF(f._1))))
    })
    println(delim)
  }

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
        case Var(v) => (show(v), false)
        case Not(term) =>
          val rep = showTermBraces(term)
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

  private def show(f: FormulaNNF): String = {
    def showAux(f: FormulaNNF): (String, Boolean) = {
      def showTermBraces(term: FormulaNNF): String = {
        val (termStr, paran) = showAux(term)
        if (paran) s"($termStr)" else termStr
      }

      import FormulaNNF._
      f match {
        case True => ("T", false)
        case False => ("F", false)
        case Var(v) => (show(v), false)
        case NotVar(v) => (s"¬${show(v)}", false)
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

  private def show(v: Type.KindedVar): String = v.sym.text match {
    case VarText.Absent => "???"
    case VarText.SourceText(s) => s
    case VarText.FallbackText(s) => s
  }

}
