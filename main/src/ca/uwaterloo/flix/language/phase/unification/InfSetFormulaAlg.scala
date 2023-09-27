package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.language.ast.Type
import ca.uwaterloo.flix.util.collection.Bimap

import scala.collection.immutable.SortedSet

class InfSetFormulaAlg extends BoolAlg[InfSetFormula] {
  /**
    * Returns `true` if `f` represents TRUE.
    */
  override def isTrue(f: InfSetFormula): Boolean = f match {
    case InfSetFormula.Top => true
    case _ => false
  }

  /**
    * Returns `true` if `f` represents FALSE.
    */
  override def isFalse(f: InfSetFormula): Boolean = f match {
    case InfSetFormula.Bot => true
    case _ => false
  }

  /**
    * Returns `true` if `f` represents a variable.
    */
  override def isVar(f: InfSetFormula): Boolean = f match {
    case InfSetFormula.Var(_) => true
    case _ => false
  }

  /**
    * Returns a representation of TRUE.
    */
  override def mkTrue: InfSetFormula = InfSetFormula.Top

  /**
    * Returns a representation of FALSE.
    */
  override def mkFalse: InfSetFormula = InfSetFormula.Bot

  /**
    * Returns a representation of the variable with the given `id`.
    */
  override def mkVar(id: Int): InfSetFormula = InfSetFormula.Var(id)

  /**
    * Returns a representation of the complement of `f`.
    */
  override def mkNot(f: InfSetFormula): InfSetFormula = InfSetFormula.mkCompl(f)

  /**
    * Returns a representation of the disjunction of `f1` and `f2`.
    */
  override def mkOr(f1: InfSetFormula, f2: InfSetFormula): InfSetFormula = InfSetFormula.mkUnion(f1, f2)

  /**
    * Returns a representation of the conjunction of `f1` and `f2`.
    */
  override def mkAnd(f1: InfSetFormula, f2: InfSetFormula): InfSetFormula = InfSetFormula.mkInter(f1, f2)

  /**
    * Returns the set of free variables in `f`.
    */
  override def freeVars(f: InfSetFormula): SortedSet[Int] = f.freeVars

  /**
    * Applies the function `fn` to every variable in `f`.
    */
  override def map(f: InfSetFormula)(fn: Int => InfSetFormula): InfSetFormula = f match {
    case InfSetFormula.Top => f
    case InfSetFormula.Cst(_) => f
    case InfSetFormula.Var(x) => fn(x)
    case InfSetFormula.Compl(ff) => InfSetFormula.mkCompl(map(ff)(fn))
    case InfSetFormula.Inter(f1, f2) => InfSetFormula.mkInter(map(f1)(fn), map(f2)(fn))
    case InfSetFormula.Union(f1, f2) => InfSetFormula.mkUnion(map(f1)(fn), map(f2)(fn))
  }

  /**
    * Returns `true` if formula is satisfiable and `false` otherwise.
    */
  override def satisfiable(f: InfSetFormula): Boolean = f match {
    case InfSetFormula.Top => true
    case InfSetFormula.Cst(_) => false
    case InfSetFormula.Var(_) => true
    case _ => evaluateAll(f, freeVars(f).toList, List.empty, List.empty)
  }

  def evaluateAll(f: InfSetFormula, l: List[Int], topVars: List[Int], botVars: List[Int]): Boolean = l match {
    case Nil =>
      // All variables are bound. Compute the truth value.
      InfSetFormula.evaluateGround(f, topVars, botVars) == InfSetFormula.Top
    case x :: xs =>
      // Recurse on two cases: x = false and x = true.
      evaluateAll(f, xs, x :: topVars, botVars) || evaluateAll(f, xs, topVars, x :: botVars)
  }


  /**
    * Returns a representation equivalent to `f` (but potentially smaller).
    */
  override def minimize(f: InfSetFormula): InfSetFormula = InfSetFormula.minimize(f)

  /**
    * Converts the given formula f into a type.
    */
  override def toType(f: InfSetFormula, env: Bimap[BoolFormula.VarOrEff, Int]): Type = ???
}
