package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.language.phase.unification.SetFormula._

class SetFormulaAlg {

  def mkAll(): SetFormula = All

  def mkCst(s: Set[Int])(implicit universe: Set[Int]): SetFormula = {
    if (s == universe) All else Cst(s)
  }

  def mkEmpty(): SetFormula = Empty

  def mkVar(id: Int): SetFormula = Var(id)

  def mkNot(f: SetFormula)(implicit universe: Set[Int]): SetFormula = f match {
    case All => Empty
    case Empty => All
    case Cst(s) => mkCst(universe diff s)
    case Not(f) => f
    case _ => Not(f)
  }

  def mkComplement(f: SetFormula)(implicit universe: Set[Int]): SetFormula =
    mkNot(f)

  def mkAnd(f1: SetFormula, f2: SetFormula)(implicit universe: Set[Int]): SetFormula = (f1, f2) match {
    case (All, other) => other
    case (other, All) => other
    case (Empty, _) => Empty
    case (_, Empty) => Empty
    case (Cst(c1), Cst(c2)) => mkCst(c1 intersect c2)
    case _ => And(f1, f2)
  }

  def mkIntersection(f1: SetFormula, f2: SetFormula)(implicit universe: Set[Int]): SetFormula =
    mkAnd(f1, f2)

  def mkOr(f1: SetFormula, f2: SetFormula)(implicit universe: Set[Int]): SetFormula = (f1, f2) match {
    case (All, _) => All
    case (_, All) => All
    case (Empty, other) => other
    case (other, Empty) => other
    case (Cst(c1), Cst(c2)) => mkCst(c1 union c2)
    case _ => Or(f1, f2)
  }

  def mkUnion(f1: SetFormula, f2: SetFormula)(implicit universe: Set[Int]): SetFormula =
    mkOr(f1, f2)

  def mkDifference(f1: SetFormula, f2: SetFormula)(implicit universe: Set[Int]): SetFormula = (f1, f2) match {
    case (All, other) => mkComplement(other)
    case (_, All) => Empty
    case (Empty, _) => Empty
    case (other, Empty) => other
    case (Cst(c1), Cst(c2)) => mkCst(c1 diff c2)
    case _ => mkIntersection(f1, mkComplement(f2))
  }

}
