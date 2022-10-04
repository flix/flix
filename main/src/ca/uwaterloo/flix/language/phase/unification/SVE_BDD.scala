package ca.uwaterloo.flix.language.phase.unification

/*package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.Type
import ca.uwaterloo.flix.util.collection.Bimap
import org.sosy_lab.pjbdd.api.{Builders, DD}
import org.sosy_lab.pjbdd.util.parser.DotExporter

object SVE_BDD {
  /**
    * Performs success variable elimination on the given boolean expression `f`.
    *
    * `flexvs` is the list of remaining flexible variables in the expression.
    */
  def successiveVariableElimination(tpe: Type, flexvs: flexvs: List[Int])(implicit flix: Flix): Substitution = {
    // Compute the variables in `tpe`.
    val tvars = tpe.typeVars.toList.map(tvar => BoolFormula.VarOrEff.Var(tvar.sym))
    val effs = tpe.effects.toList.map(BoolFormula.VarOrEff.Eff)

    // Construct a bi-directional map from type variables to indices.
    // The idea is that the first variable becomes x0, the next x1, and so forth.
    val m = (tvars ++ effs).zipWithIndex.foldLeft(Bimap.empty[BoolFormula.VarOrEff, BoolFormulaTable.Variable]) {
      case (macc, (sym, x)) => macc + (sym -> x)
    }

    // Construct the formula from the type and then the BDD from the formula
    val f = BoolFormula.fromBoolType(tpe, m)
    val dd = BoolFormulaBDD.createBDDFromFormula(f)

    print("Original formula:")
    println(f)

    //Do SVE and convert back to a normal substitution
    val BDD_sub = successiveVariableElimination(dd, flexvs)
    BDD_sub.toSubstitution(m,tpe.loc)
  }

  private def successiveVariableElimination(dd: DD, flexvs: List[Type.KindedVar])(implicit flix: Flix): BDD_Substitution = flexvs match {
    case Nil =>
      // Determine if f is unsatisfiable when all (rigid) variables are made flexible.
      if (!satisfiable(dd))
        BDD_Substitution.empty
      else
        throw BDDBooleanUnificationException

    case x :: xs =>
      val t0 = BDD_Substitution.singleton(x.sym, BoolFormulaBDD.creator.makeFalse())(dd)
      val t1 = BDD_Substitution.singleton(x.sym, BoolFormulaBDD.creator.makeTrue())(dd)

      val se = successiveVariableElimination(BoolFormulaBDD.creator.makeAnd(t0, t1), xs)

      val f1 = BoolFormulaBDD.creator.makeOr(se(t0), BoolFormulaBDD.creator.makeAnd(BoolFormulaBDD.creator.makeIthVar(x.sym.id), BoolFormulaBDD.creator.makeNot(se(t1))))
      val st = BDD_Substitution.singleton(x.sym, f1)
      st ++ se
  }

  /**
    * An exception thrown to indicate that boolean unification using BDDs failed.
    */
  private case object BDDBooleanUnificationException extends RuntimeException

  /**
    * Returns `true` if the given boolean formula `f` is satisfiable
    * when ALL variables in the formula are flexible.
    */
  private def satisfiable(dd: DD)(implicit flix: Flix): Boolean = {
    !dd.isFalse()
  }

}*/
