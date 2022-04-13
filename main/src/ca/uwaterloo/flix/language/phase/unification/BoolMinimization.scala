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
import ca.uwaterloo.flix.language.ast.{Kind, Rigidity, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.util.InternalCompilerException

object BoolMinimization {

  /**
    * Minimizes the Boolean formula given by `t`.
    */
  def minimize(t: Type): Type = {
    // TODO remove this
    val useMinimizer = false
    val outputSizeChange = false
    if (useMinimizer) {
      val formula = Formula.fromType(t)
      val nnf = Formula.toNNF(formula)
      val cnf = FormulaNNF.toCNF(nnf)
      val res = FormulaCNF.toType(cnf, t.loc)
      if (outputSizeChange) {
        val before = t.size
        val after = res.size
        if (before > 9 || after > 9) {
          val goodBad = if (after <= before) "+" else " "
          println(s"$goodBad $before cnf'ed to $after")
        }
      }
      res
    } else t
  }

  private sealed trait Formula

  private object Formula {

    sealed trait Conjunction extends NonNegation with NonDisjunction

    sealed trait Disjunction extends NonNegation with NonConjunction

    sealed trait Negation extends NonConjunction with NonDisjunction

    sealed trait NonConjunction extends Formula

    sealed trait NonDisjunction extends Formula

    sealed trait NonNegation extends Formula

    sealed trait SimpleTerm extends Conjunction with Disjunction with Negation

    case object True extends SimpleTerm

    case object False extends SimpleTerm

    case class Var(v: Type.KindedVar) extends SimpleTerm

    case class Not(term: NonNegation) extends Negation

    /**
      * A conjunction of terms. And empty list is `True`.
      */
    case class And(terms: Vector[NonConjunction]) extends Conjunction

    /**
      * A disjunction of terms. And empty list is `True`.
      */
    case class Or(terms: Vector[NonDisjunction]) extends Disjunction

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
      mkAnd(f.toVector)
    }

    /**
      * Merges the conjunctions in `f` into the new conjunction and performs
      * shallow minimization.
      */
    def mkAnd(f: Vector[Formula]): Formula = {
      if (f.isEmpty) throw InternalCompilerException("Cannot be empty")
      val baseValue: Option[Vector[NonConjunction]] = Some(Vector.empty)
      val hoistedTerms = f.foldLeft(baseValue) {
        case (opt, term) => opt.flatMap(
          acc => term match {
            case True => Some(acc)
            case False => None
            case And(terms) => Some(acc ++ terms)
            case t@Var(_) => Some(acc :+ t)
            case t@Not(_) => Some(acc :+ t)
            case t@Or(_) => Some(acc :+ t)
          }
        )
      }
      hoistedTerms match {
        case None => False
        case Some(Vector()) => True // happens for input like True ∧ True
        case Some(Vector(term)) => term
        case Some(terms) => And(terms)
      }
    }

    /**
      * Merges the disjunctions if arguments are disjunctions themselves and
      * performs shallow minimization.
      */
    def mkOr(f: Formula*): Formula = {
      mkOr(f.toVector)
    }

    /**
      * Merges the disjunctions in `f` into the new disjunction and performs
      * shallow minimization.
      */
    def mkOr(f: Vector[Formula]): Formula = {
      if (f.isEmpty) throw InternalCompilerException("Cannot be empty")
      val baseValue: Option[Vector[NonDisjunction]] = Some(Vector.empty)
      val hoistedTerms = f.foldLeft(baseValue) {
        case (opt, term) => opt.flatMap(
          acc => term match {
            case True => None
            case False => Some(acc)
            case Or(terms) => Some(acc ++ terms)
            case t@Var(_) => Some(acc :+ t)
            case t@Not(_) => Some(acc :+ t)
            case t@And(_) => Some(acc :+ t)
          }
        )
      }
      hoistedTerms match {
        case None => True
        case Some(Vector()) => False // happens for input like False ∨ False
        case Some(Vector(term)) => term
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
          case And(terms) => toNNF(mkOr(terms.map(t => mkNot(t))))
          case Or(terms) => toNNF(mkAnd(terms.map(t => mkNot(t))))
        }

        case And(terms) => FormulaNNF.mkAnd(terms.map(toNNF))

        case Or(terms) => FormulaNNF.mkOr(terms.map(toNNF))

      }
    }

    /**
      * Converts a type boolean formula `t` to a `Formula` formula.
      * Throws exceptions for non-boolean or malformed types.
      */
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

    sealed trait Disjunction extends FormulaNNF

    sealed trait Conjunction extends FormulaNNF

    sealed trait SimpleTerm extends Conjunction with Disjunction with FormulaCNF.Disjunction

    case object True extends SimpleTerm

    case object False extends SimpleTerm

    case class Var(v: Type.KindedVar) extends SimpleTerm

    case class NotVar(v: Type.KindedVar) extends SimpleTerm

    /**
      * A conjunction of terms. And empty list is `True`.
      */
    case class And(terms: Vector[Disjunction]) extends Conjunction

    /**
      * A disjunction of terms. And empty list is `True`.
      */
    case class Or(terms: Vector[Conjunction]) extends Disjunction

    /**
      * Merges the conjunctions if arguments are conjunctions themselves and
      * performs shallow minimization along with unit propagation.
      */
    def mkAnd(f: FormulaNNF*): FormulaNNF = {
      mkAnd(f.toVector)
    }

    /**
      * Merges the conjunctions in `f` into the new conjunction and performs
      * shallow minimization.
      */
    def mkAnd(f: Vector[FormulaNNF]): FormulaNNF = {
      if (f.isEmpty) throw InternalCompilerException("Cannot be empty")
      val baseValue: Option[Vector[Disjunction]] = Some(Vector.empty)
      val hoistedTerms = f.foldLeft(baseValue) {
        case (opt, term) => opt.flatMap(
          acc => term match {
            case True => Some(acc)
            case False => None
            case And(terms) => Some(acc ++ terms)
            case t@Var(_) => Some(acc :+ t)
            case t@NotVar(_) => Some(acc :+ t)
            case t@Or(_) => Some(acc :+ t)
          }
        )
      }
      hoistedTerms match {
        case None => False
        case Some(Vector()) => True // happens for input like True ∧ True
        case Some(Vector(term)) => term
        case Some(terms) => And(terms)
      }
    }

    /**
      * Merges the disjunctions if arguments are disjunctions themselves and
      * performs shallow minimization.
      */
    def mkOr(f: FormulaNNF*): FormulaNNF = {
      mkOr(f.toVector)
    }

    /**
      * Merges the disjunctions in `f` into the new disjunction and performs
      * shallow minimization.
      */
    def mkOr(f: Vector[FormulaNNF]): FormulaNNF = {
      if (f.isEmpty) throw InternalCompilerException("Cannot be empty")
      val baseValue: Option[Vector[Conjunction]] = Some(Vector.empty)
      val hoistedTerms = f.foldLeft(baseValue) {
        case (opt, term) => opt.flatMap(
          acc => term match {
            case True => None
            case False => Some(acc)
            case Or(terms) => Some(acc ++ terms)
            case t@Var(_) => Some(acc :+ t)
            case t@NotVar(_) => Some(acc :+ t)
            case t@And(_) => Some(acc :+ t)
          }
        )
      }
      hoistedTerms match {
        case None => True
        case Some(Vector()) => False // happens for input like False ∨ False
        case Some(Vector(term)) => term
        case Some(terms) => Or(terms)
      }
    }

    /**
      * Transform `f` to an equivalent formula that is a conjunction of
      * disjunctions of either variables or negated variables.
      * Ex. `x ∧ (x ∨ y) ∧ (z ∨ ¬y ∨ ¬z)`
      */
    def toCNF(f: FormulaNNF): FormulaCNF = {
      f match {
        case True => True
        case False => False
        case Var(v) => Var(v)
        case NotVar(v) => NotVar(v)
        case And(terms) => FormulaCNF.mkAnd(terms.map(toCNF))
        case Or(terms) =>
          val (simpleTerms, conjunctions) = split(terms)
          if (conjunctions.isEmpty) {
            FormulaCNF.mkOr(simpleTerms)
          } else {
            val base: FormulaNNF = if (simpleTerms.isEmpty) False else mkOr(simpleTerms)
            val newTerms: FormulaNNF = conjunctions.foldLeft(base) {
              case (acc, and) =>
                // Transform the formula `acc ∨ ( a_1 ∧ a_2 ∧ ... ∧ a_n)`
                // to `(a_1 ∨ acc) ∧ (a_2 ∨ acc) ∧ ... ∧ (a_n ∨ acc)`
                mkAnd(and.terms.map(d => mkOr(d, acc)))
            }
            toCNF(newTerms)
          }
      }
    }

    /**
      * Split terms into simple terms and conjunctions.
      */
    def split(terms: Vector[Conjunction]): (Vector[FormulaNNF.SimpleTerm], Vector[FormulaNNF.And]) = {
      val base: (Vector[FormulaNNF.SimpleTerm], Vector[FormulaNNF.And]) = (Vector.empty, Vector.empty)
      terms.foldLeft(base) {
        case ((simples, conjunctions), conjunction) => conjunction match {
          case True => (simples :+ FormulaNNF.True, conjunctions)
          case False => (simples :+ FormulaNNF.False, conjunctions)
          case Var(v) => (simples :+ FormulaNNF.Var(v), conjunctions)
          case NotVar(v) => (simples :+ FormulaNNF.NotVar(v), conjunctions)
          case and@And(_) => (simples, conjunctions :+ and)
        }
      }
    }

  }

  private sealed trait FormulaCNF

  private object FormulaCNF {

    sealed trait Disjunction extends FormulaCNF

    /**
      * A conjunction of disjunctions. And empty list is `True`.
      */
    case class And(terms: Vector[Disjunction]) extends FormulaCNF

    /**
      * A disjunction of simple terms. And empty list is `True`.
      */
    case class Or(terms: Set[FormulaNNF.SimpleTerm]) extends Disjunction

    /**
      * Merges the conjunctions if arguments are conjunctions themselves and
      * performs shallow minimization along with unit propagation.
      */
    def mkAnd(f: FormulaCNF*): FormulaCNF = {
      mkAnd(f.toVector)
    }

    /**
      * Merges the conjunctions in `f` into the new conjunction and performs
      * shallow minimization.
      */
    def mkAnd(f: Vector[FormulaCNF]): FormulaCNF = {
      if (f.isEmpty) throw InternalCompilerException("Cannot be empty")
      val baseValue: Option[Vector[Disjunction]] = Some(Vector.empty)
      val hoistedTerms = f.foldLeft(baseValue) {
        case (opt, term) => opt.flatMap(
          acc => term match {
            case FormulaNNF.True => Some(acc)
            case FormulaNNF.False => None
            case And(terms) => Some(acc ++ terms)
            case t@FormulaNNF.Var(_) => Some(acc :+ t)
            case t@FormulaNNF.NotVar(_) => Some(acc :+ t)
            case t@Or(_) => Some(acc :+ t)
          }
        )
      }
      hoistedTerms match {
        case None => FormulaNNF.False
        case Some(Vector()) => FormulaNNF.True // happens for input like True ∧ True
        case Some(Vector(term)) => term
        case Some(terms) => And(terms)
      }
    }

    /**
      * Merges the disjunctions if arguments are disjunctions themselves and
      * performs shallow minimization.
      */
    def mkOr(f: Disjunction*): Disjunction = {
      mkOr(f.toVector)
    }

    /**
      * Merges the disjunctions in `f` into the new disjunction and performs
      * shallow minimization.
      */
    def mkOr(f: Vector[Disjunction]): Disjunction = {
      if (f.isEmpty) throw InternalCompilerException("Cannot be empty")
      val baseValue: Option[Set[FormulaNNF.SimpleTerm]] = Some(Set.empty)
      val hoistedTerms = f.foldLeft(baseValue) {
        case (opt, term) => opt.flatMap(
          acc => term match {
            case FormulaNNF.True => None
            case FormulaNNF.False => Some(acc)
            case Or(terms) => Some(acc ++ terms)
            case t@FormulaNNF.Var(_) => Some(acc + t)
            case t@FormulaNNF.NotVar(_) => Some(acc + t)
          }
        )
      }
      hoistedTerms match {
        case None => FormulaNNF.True
        case Some(set) if set.isEmpty => FormulaNNF.False // happens for input like False ∨ False
        case Some(set) if set.size == 1 => set.head
        case Some(terms) => Or(terms)
      }
    }

    def toType(f: FormulaCNF, loc: SourceLocation): Type = {
      f match {
        case FormulaNNF.True => Type.mkTrue(loc)
        case FormulaNNF.False => Type.mkFalse(loc)
        case FormulaNNF.Var(v) => v
        case FormulaNNF.NotVar(v) => Type.mkNot(v, loc)
        case And(terms) =>
          if (terms.isEmpty) throw InternalCompilerException("Cannot be empty")
          val (base, remaining) = (terms.head, terms.tail)
          remaining.foldLeft(toType(base, loc)) {
            (acc, f) => Type.mkAnd(acc, toType(f, loc), loc)
          }
        case Or(terms) =>
          if (terms.isEmpty) throw InternalCompilerException("Cannot be empty")
          val (base, remaining) = (terms.head, terms.tail)
          remaining.foldLeft(toType(base, loc)) {
            (acc, f) => Type.mkOr(acc, toType(f, loc), loc)
          }
      }
    }

  }

  def main(args: Array[String]): Unit = {
    import Formula._
    def mkVar(s: String): Var = Var(
      Type.KindedVar(
        new Symbol.KindedTypeVarSym(
          s.hashCode,
          VarText.SourceText(s),
          Kind.Bool,
          Rigidity.Flexible,
          SourceLocation.Unknown
        ),
        SourceLocation.Unknown
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
      println(show(FormulaNNF.toCNF(toNNF(f._1))))
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

  private def show(f: FormulaCNF): String = {
    def showAux(f: FormulaCNF): (String, Boolean) = {
      def showTermBraces(term: FormulaCNF): String = {
        val (termStr, paran) = showAux(term)
        if (paran) s"($termStr)" else termStr
      }

      import FormulaCNF._
      f match {
        case FormulaNNF.True => ("T", false)
        case FormulaNNF.False => ("F", false)
        case FormulaNNF.Var(v) => (show(v), false)
        case FormulaNNF.NotVar(v) => (s"¬${show(v)}", false)
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
