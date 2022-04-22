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

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.Ast.VarText
import ca.uwaterloo.flix.language.ast.{Kind, Rigidity, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.util.InternalCompilerException

import scala.collection.mutable

object BoolMinimization {

  /**
    * Minimizes the Boolean formula given by `t`.
    */
  def minimize(t: Type)(implicit flix: Flix): Type = {
    // TODO remove this
    val outputSizeChange = false
    if (flix.options.xminimize) {
      val formula = Formula.fromType(t)
      val nnf = Formula.toNNF(formula)
      val cnf = FormulaNNF.toCNF(nnf)
      val unitProp = FormulaCNF.unitPropagation(cnf)
      val res = FormulaCNF.toType(unitProp, t.loc)
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

    case object True extends Formula

    case object False extends Formula

    case class Var(v: Type.KindedVar) extends Conjunction with Disjunction with Negation

    case class Not(term: NonNegation) extends Negation

    /**
      * A conjunction of terms. `terms` should not be empty.
      */
    case class And(terms: Vector[NonConjunction]) extends Conjunction

    /**
      * A disjunction of terms. `terms` should not be empty.
      */
    case class Or(terms: Vector[NonDisjunction]) extends Disjunction

    /**
      * A negation of `f`.
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
      * Equivalent to `mkAnd(f.toVector)`.
      */
    def mkAnd(f: Formula*): Formula = {
      mkAnd(f.toVector)
    }

    /**
      * Merges the conjunctions in `f` into the new conjunction and performs
      * shallow minimization regarding `True` and `False`.
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
      * Equivalent to `mkOr(f.toVector)`.
      */
    def mkOr(f: Formula*): Formula = {
      mkOr(f.toVector)
    }

    /**
      * Merges the disjunctions in `f` into the new disjunction and performs
      * shallow minimization regarding `True` and `False`.
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
      * Returns a formula in negative normal form (NNF) equivalent to `f`.
      */
    def toNNF(f: Formula): FormulaNNF = {
      f match {
        case True => FormulaNNF.True

        case False => FormulaNNF.False

        case Var(v) => FormulaNNF.Var(v)

        case not: Not => not.term match {
          case Var(v) => FormulaNNF.NotVar(v)
          case And(terms) => toNNF(mkOr(terms.map(t => mkNot(t))))
          case Or(terms) => toNNF(mkAnd(terms.map(t => mkNot(t))))
        }

        case And(terms) => FormulaNNF.mkAnd(terms.map(toNNF))

        case Or(terms) => FormulaNNF.mkOr(terms.map(toNNF))

      }
    }

    /**
      * Converts a type boolean formula `t` to a boolean formula of type `Formula`.
      * Source locations are not maintained.
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

    sealed trait StrictDisjunction extends Disjunction

    sealed trait Conjunction extends FormulaNNF

    sealed trait StrictConjunction extends Conjunction

    sealed trait Variable extends StrictConjunction with StrictDisjunction

    case object True extends Disjunction with Conjunction

    case object False extends Disjunction with Conjunction

    case class Var(v: Type.KindedVar) extends Variable

    case class NotVar(v: Type.KindedVar) extends Variable

    /**
      * A conjunction of terms. `terms` should not be empty.
      */
    case class And(terms: Vector[StrictDisjunction]) extends StrictConjunction

    /**
      * A disjunction of terms. `terms` should not be empty.
      */
    case class Or(terms: Vector[StrictConjunction]) extends StrictDisjunction

    /**
      * Equivalent to `mkAnd(f.toVector)`.
      */
    def mkAnd(f: FormulaNNF*): FormulaNNF = {
      mkAnd(f.toVector)
    }

    /**
      * Merges the conjunctions in `f` into the new conjunction and performs
      * shallow minimization regarding `True` and `False`.
      */
    def mkAnd(f: Vector[FormulaNNF]): FormulaNNF = {
      if (f.isEmpty) throw InternalCompilerException("Cannot be empty")
      val baseValue: Option[Vector[StrictDisjunction]] = Some(Vector.empty)
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
      * Equivalent to `mkOr(f.toVector)`.
      */
    def mkOr(f: FormulaNNF*): FormulaNNF = {
      mkOr(f.toVector)
    }

    /**
      * Merges the disjunctions in `f` into the new disjunction and performs
      * shallow minimization regarding `True` and `False`.
      */
    def mkOr(f: Vector[FormulaNNF]): FormulaNNF = {
      if (f.isEmpty) throw InternalCompilerException("Cannot be empty")
      val baseValue: Option[Vector[StrictConjunction]] = Some(Vector.empty)
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
      * Returns a formula in conjunctive normal form (CNF) equivalent to `f`.
      *
      * Example: `x ∧ (x ∨ y) ∧ (z ∨ ¬y ∨ ¬z)`
      */
    def toCNF(f: FormulaNNF): FormulaCNF = {
      f match {
        case True => FormulaCNF.True
        case False => FormulaCNF.False
        case Var(v) => FormulaCNF.Var(v)
        case NotVar(v) => FormulaCNF.NotVar(v)
        case And(terms) => FormulaCNF.mkAnd(terms.map(toCNF))
        case Or(terms) =>
          val (simpleTerms, conjunctions) = split(terms)
          if (conjunctions.isEmpty) {
            val cnfSimpleTerms = simpleTerms.map {
              case Var(v) => FormulaCNF.Var(v)
              case NotVar(v) => FormulaCNF.NotVar(v)
            }
            FormulaCNF.mkOr(cnfSimpleTerms)
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
    def split(terms: Vector[StrictConjunction]): (Vector[Variable], Vector[And]) = {
      val base: (Vector[Variable], Vector[And]) = (Vector.empty, Vector.empty)
      terms.foldLeft(base) {
        case ((simples, conjunctions), conjunction) => conjunction match {
          case Var(v) => (simples :+ Var(v), conjunctions)
          case NotVar(v) => (simples :+ NotVar(v), conjunctions)
          case and@And(_) => (simples, conjunctions :+ and)
        }
      }
    }

  }

  private sealed trait FormulaCNF

  private object FormulaCNF {

    sealed trait Disjunction extends FormulaCNF

    sealed trait StrictDisjunction extends Disjunction

    sealed trait Variable extends StrictDisjunction {
      def v: Type.KindedVar
    }

    case object True extends Disjunction

    case object False extends Disjunction

    case class Var(v: Type.KindedVar) extends Variable

    case class NotVar(v: Type.KindedVar) extends Variable

    /**
      * A conjunction of disjunctions. `terms` should not be empty.
      */
    case class And(terms: Vector[StrictDisjunction]) extends FormulaCNF

    /**
      * A disjunction of simple terms. `terms` should not be empty.
      */
    case class Or(terms: Set[Variable]) extends StrictDisjunction

    /**
      * Equivalent to `mkAnd(f.toVector)`.
      */
    def mkAnd(f: FormulaCNF*): FormulaCNF = {
      mkAnd(f.toVector)
    }

    /**
      * Merges the conjunctions in `f` into the new conjunction and performs
      * shallow minimization regarding `True` and `False`.
      */
    def mkAnd(f: Vector[FormulaCNF]): FormulaCNF = {
      if (f.isEmpty) throw InternalCompilerException("Cannot be empty")
      val baseValue: Option[Vector[StrictDisjunction]] = Some(Vector.empty)
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
      * Equivalent to `mkOr(f.toVector)`.
      */
    def mkOr(f: Disjunction*): FormulaCNF = {
      mkOr(f.toVector)
    }

    /**
      * Merges the disjunctions in `f` into the new disjunction and performs
      * shallow minimization regarding `True` and `False`.
      */
    def mkOr(f: Vector[Disjunction]): FormulaCNF = {
      if (f.isEmpty) throw InternalCompilerException("Cannot be empty")
      val baseValue: Option[Set[Variable]] = Some(Set.empty)
      val hoistedTerms = f.foldLeft(baseValue) {
        case (opt, term) => opt.flatMap(
          acc => term match {
            case True => None
            case False => Some(acc)
            case Or(terms) => Some(acc ++ terms)
            case t@Var(_) => Some(acc + t)
            case t@NotVar(_) => Some(acc + t)
          }
        )
      }
      hoistedTerms match {
        case None => True
        case Some(set) if set.isEmpty => False // happens for input like False ∨ False
        case Some(set) if set.size == 1 => set.head
        case Some(terms) => Or(terms)
      }
    }

    /**
      * Performs unit propagation, simplifying `x ∧ (a ∨ y)` into `a ∧ y`.
      * Based on Zhang and Stickel 1996 "An Efficient Algorithm for Unit Propagation"
      */
    def unitPropagationOptimized(f: FormulaCNF): FormulaCNF = {
      type Queue[E] = mutable.ArrayDeque[E]

      def empty[E](): Queue[E] = mutable.ArrayDeque.empty

      def stackPush[E](e: E, q: Queue[E]): Unit = q.append(e)

      def stackPop[E](q: Queue[E]): E = q.removeHead()

      def aux(clauses: Vector[StrictDisjunction]): FormulaCNF = {
        type Clause = StrictDisjunction
        var ok = true
        type clauseList = mutable.ArrayBuffer[Clause]
        val clausesOfPosHead = mutable.Map[Variable, clauseList]()
        val clausesOfNegHead = mutable.Map[Variable, clauseList]()
        val clausesOfPosTail = mutable.Map[Variable, clauseList]()
        val clausesOfNegTail = mutable.Map[Variable, clauseList]()

        def safeGet(m: mutable.Map[Variable, clauseList], v: Variable): clauseList =
          m.getOrElseUpdate(v, mutable.ArrayBuffer())

        for (c <- clauses) c match {
          case v@Var(_) =>
            safeGet(clausesOfPosHead, v) :+ c
            safeGet(clausesOfPosTail, v) :+ c
          case v@NotVar(_) =>
            safeGet(clausesOfNegHead, v) :+ c
            safeGet(clausesOfNegTail, v) :+ c
          case Or(terms) =>
            terms.head match {
              case v@Var(_) => safeGet(clausesOfPosHead, v) :+ c
              case v@NotVar(_) => safeGet(clausesOfNegHead, v) :+ c
            }
            terms.last match {
              case v@Var(_) => safeGet(clausesOfPosTail, v) :+ c
              case v@NotVar(_) => safeGet(clausesOfNegTail, v) :+ c
            }
        }

        def propagateTrueValue(v: FormulaCNF.Var): Unit = {
          for (c <- clausesOfNegHead(v) if ok) {
            shortenClauseFromHead(c)
          }
          for (c <- clausesOfNegTail(v) if ok) {
            ??? //shortenClauseFromTail(c)
          }
        }

        def shortenClauseFromHead(c: Clause): Unit = {
          ???
        }

        val units = empty[Variable]()
        val truthValue = mutable.Map[Variable, Boolean]()
        for (c <- clauses) c match {
          case l@Var(_) => stackPush(l, units)
          case l@NotVar(_) => stackPush(l, units)
          // case Or(terms) if terms.lengthIs == 1 => stackPush(terms.head, units)
          case Or(_) => ()
        }
        while (units.nonEmpty) {
          val l = stackPop(units)
          truthValue.get(l) match {
            case Some(true) => () // continue
            case Some(false) => ok = false // impossible unit clause
            case None => // variable value is unknown
              l match {
                case v@Var(_) =>
                  truthValue(l) = true
                  propagateTrueValue(v)
                case v@NotVar(_) =>
                  truthValue(l) = false
                  ??? //propagateFalseValue(v)
              }
          }
        }
        if (!ok) False else
          ???
      }

      f match {
        case True | False | Var(_) | NotVar(_) | Or(_) => f
        case And(terms) => aux(terms)
      }
    }

    def unitPropagation(f: FormulaCNF): FormulaCNF = {
      def aux(terms: Vector[StrictDisjunction]): FormulaCNF = {
        val occurIndex = mutable.Map[Variable, mutable.ArrayBuffer[Int]]().withDefault(_ => mutable.ArrayBuffer())
        val truthValue = mutable.Map[Type.KindedVar, Boolean]()
        val count = mutable.ArrayBuffer[Int]()
        val todo = mutable.ArrayDeque[Int]()
        terms.zipWithIndex.foreach {
          case (v@Var(_), i) =>
            count.append(1)
            todo.append(i)
          // skip this
          // occurIndex(v) :+= i
          case (v@NotVar(_), i) =>
            count.append(1)
            todo.append(i)
          // skip this
          // occurIndex(v) :+= i
          case (Or(terms), i) =>
            count.append(terms.size)
            if (terms.sizeIs == 1) todo.append(i)
            for (t <- terms) t match {
              case v@Var(_) => occurIndex(v) :+= i
              case v@NotVar(_) => occurIndex(v) :+= i
            }
        }
        while (todo.nonEmpty) {
          val vi = todo.removeHead()
          count(vi) = -1
          val (v, negatedV) = terms(vi) match {
            case Var(v) =>
              truthValue(v) = true
              (Var(v), NotVar(v))
            case NotVar(v) =>
              truthValue(v) = false
              (NotVar(v), Var(v))
            case Or(terms) => terms.find(v => !truthValue.contains(v.v)) match {
              case Some(Var(v)) =>
                truthValue(v) = true
                (Var(v), NotVar(v))
              case Some(NotVar(v)) =>
                truthValue(v) = false
                (NotVar(v), Var(v))
              case None => throw InternalCompilerException("Impossible")
            }
            case _ => throw InternalCompilerException("Impossible")
          }
          for (i <- occurIndex(v)) {
            count(i) = -1 // clause is true
          }
          for (i <- occurIndex(negatedV)) {
            count(i) -= 1
            if (count(i) == 1) todo.append(i) // unit clause
            else if (count(i) == 0) return False // clause cannot be true
          }
        }
        val unitClauses = truthValue.map {
          case (v, true) => Var(v)
          case (v, false) => NotVar(v)
        }.toVector
        val remaining = terms.zipWithIndex.foldLeft(Vector.empty[StrictDisjunction]) {
          case (acc, (_, i)) if count(i) == -1 => acc
          case (acc, (Var(v), _)) => acc :+ Var(v)
          case (acc, (NotVar(v), _)) => acc :+ NotVar(v)
          case (acc, (Or(terms), _)) => acc :+ Or(terms.filter(v => !truthValue.contains(v.v)))
        }
        mkAnd(unitClauses ++ remaining)
      }

      f match {
        case True | False | Var(_) | NotVar(_) | Or(_) => f
        case And(terms) => aux(terms)
      }
    }


    /**
      * Converts a CNF boolean formula `f` to a type formula.
      * N-ary operations are combined left-to-right.
      *
      * Example: `x ∧ y ∧ z ∧ q` becomes `((x ∧ y) ∧ z) ∧ q`.
      */
    def toType(f: FormulaCNF, loc: SourceLocation): Type = {
      f match {
        case True => Type.mkTrue(loc)
        case False => Type.mkFalse(loc)
        case Var(v) => v
        case NotVar(v) => Type.mkNot(v, loc)
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
        "x ∧ (y ∨ x)"),
      (mkAnd(mkVar("x"), mkOr(mkVar("y"), mkVar("x")), mkOr(mkNot(mkVar("x")), mkVar("z")), mkOr(mkVar("z"), mkVar("q"))),
        "x ∧ (y ∨ x) ∧ (¬x ∨ z) ∧ (z ∨ q)")
    )

    val delim = "----"

    println("raw input")
    println("input after smart constructors")
    println("NNF")
    println("CNF")
    println("Unit Propagation")

    formulas.foreach(f => {
      println(delim)
      println(f._2)
      println(show(f._1))
      val nnf = toNNF(f._1)
      println(show(nnf))
      val cnf = FormulaNNF.toCNF(nnf)
      println(show(cnf))
      val unitProp = FormulaCNF.unitPropagation(cnf)
      println(show(unitProp))
    })
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
