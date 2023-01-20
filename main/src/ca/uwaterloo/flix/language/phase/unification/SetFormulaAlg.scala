package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.language.phase.unification.SetFormula._

object SetFormulaAlg {

  def mkCst(s: Set[Int])(implicit universe: Set[Int]): SetFormula = {
    Cst(s)
  }

  def mkEmpty(): SetFormula = Empty

  def mkVar(id: Int): SetFormula = Var(id)

  def mkNot(f: SetFormula)(implicit universe: Set[Int]): SetFormula = f match {
    case Cst(s) => mkCst(universe diff s)
    case Not(f) => f
    case _ => Not(f)
  }

  def mkComplement(f: SetFormula)(implicit universe: Set[Int]): SetFormula =
    mkNot(f)

  def mkAnd(f1: SetFormula, f2: SetFormula)(implicit universe: Set[Int]): SetFormula = (f1, f2) match {
    case (Empty, _) => Empty
    case (_, Empty) => Empty
    case (Cst(c1), Cst(c2)) => mkCst(c1 intersect c2)
    case _ => And(f1, f2)
  }

  def mkIntersection(f1: SetFormula, f2: SetFormula)(implicit universe: Set[Int]): SetFormula =
    mkAnd(f1, f2)

  /**
    * Returns the non-balanced conjuntion of `terms`.
    * If `terms` is empty, Empty is returned.
    */
  def mkAnd(terms: List[SetFormula])(implicit universe: Set[Int]): SetFormula =
    terms.reduceOption(mkAnd).getOrElse(mkEmpty())

  /**
    * Returns the non-balanced intersection of `terms`.
    * If `terms` is empty, Empty is returned.
    */
  def mkIntersection(terms: List[SetFormula])(implicit universe: Set[Int]): SetFormula =
    mkAnd(terms)

  def mkOr(f1: SetFormula, f2: SetFormula)(implicit universe: Set[Int]): SetFormula = (f1, f2) match {
    case (Empty, other) => other
    case (other, Empty) => other
    case (Cst(c1), Cst(c2)) => mkCst(c1 union c2)
    case _ => Or(f1, f2)
  }

  def mkUnion(f1: SetFormula, f2: SetFormula)(implicit universe: Set[Int]): SetFormula =
    mkOr(f1, f2)

  /**
    * Returns the non-balanced disjunction of `terms`.
    * If `terms` is empty, Empty is returned.
    */
  def mkOr(terms: List[SetFormula])(implicit universe: Set[Int]): SetFormula =
    terms.reduceOption(mkOr).getOrElse(mkEmpty())

  /**
    * Returns the non-balanced union of `terms`.
    * If `terms` is empty, Empty is returned.
    */
  def mkUnion(terms: List[SetFormula])(implicit universe: Set[Int]): SetFormula =
    mkOr(terms)

  def mkDifference(f1: SetFormula, f2: SetFormula)(implicit universe: Set[Int]): SetFormula = (f1, f2) match {
    case (Empty, _) => Empty
    case (other, Empty) => other
    case (Cst(c1), Cst(c2)) => mkCst(c1 diff c2)
    case _ => mkIntersection(f1, mkComplement(f2))
  }

  /**
    * Computes the truth value of the formula `f` assuming the variables in `trueVars`
    * are true and the rest are false.
    */
  private def evaluate(f: SetFormula, trueVars: List[Int])(implicit universe: Set[Int]): Set[Int] = f match {
    case Cst(s) => s
    case Var(x) => if (trueVars.contains(x)) universe else Set.empty
    case Not(f1) => universe diff evaluate(f1, trueVars)
    case Or(f1, f2) => evaluate(f1, trueVars) union evaluate(f2, trueVars)
    case And(f1, f2) => evaluate(f1, trueVars) intersect evaluate(f2, trueVars)
  }

  /**
    * Returns `f` rewritten by exhaustive evaluation. This is exponential in the
    * number of variables. The output is in conceptual dnf
    * (disjunction of conjunctions or union of intersections).
    *
    * The lists `trueVars`, `falseVars`, and `unassignedVars` are assumed to be
    * disjoint and cover all variables in the formula
    */
  private def exhaustiveEvaluation(f: SetFormula, trueVars: List[Int], falseVars: List[Int], unassignedVars: List[Int])(implicit universe: Set[Int]): List[(SetFormula, Set[Int])] = unassignedVars match {
    case Nil => // we can evaluate the formula with all variables assigned
      val res = evaluate(f, trueVars)
      if (res.isEmpty) List((mkEmpty(), Set.empty))
      else {
        // res ∩ pos1 ∩ ... ∩ posn ∩ !neg1 ∩ ... ∩ !negn
        val terms = trueVars.map(mkVar) ++ falseVars.map(i => mkNot(mkVar(i)))
        List((mkIntersection(terms), res))
      }
    case x :: xs => // compute `f` recursively with `x` being true or false
      val xt = exhaustiveEvaluation(f, x :: trueVars, falseVars, xs)
      val xf = exhaustiveEvaluation(f, trueVars, x :: falseVars, xs)
      xt ++ xf
  }

  def simplifyByExhaustiveEvaluation(f: SetFormula)(implicit universe: Set[Int]): SetFormula = {
    val terms = exhaustiveEvaluation(f, Nil, Nil, f.freeVars.toList)
    terms match {
      case Nil => mkEmpty()
      case head :: _ =>
        if (terms.forall(_._2 == head._2)) {
          // constant function
          mkCst(head._2)
        } else {
          mkUnion(terms.map { case (f, s) => mkIntersection(mkCst(s), f) })
        }
    }
  }

}
