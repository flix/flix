/*package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.language.ast.{Ast, Kind, Scheme, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.phase.unification.BoolFormula.VarOrEff
import ca.uwaterloo.flix.util.collection.Bimap
import org.sosy_lab.pjbdd.api.{Builders, DD}

object BDD_Substitution {
  /**
    * Returns the empty substitution.
    */
  val empty: BDD_Substitution = BDD_Substitution(Map.empty)

  /**
    * Returns the singleton substitution mapping the type variable `x` to `dd`.
    */
  def singleton(x: Symbol.KindedTypeVarSym, dd: DD): BDD_Substitution = {
    BDD_Substitution(Map(x -> dd))
  }

}

/**
  * A BDD substitution is a map from type variables to BDDs.
  */
case class BDD_Substitution(m: Map[Symbol.KindedTypeVarSym, DD]) {

  /**
    * Returns `true` if `this` is the empty substitution.
    */
  val isEmpty: Boolean = m.isEmpty

  /**
    * Applies `this` substitution to the given BDD `dd0`.
    */
  def apply(dd0: DD): DD = {
    // Return the BDD if the substitution is empty. Otherwise go through the BDD and apply all substitutions.
    if (isEmpty) dd0 else {
      var dd1 = dd0
      for ((x,dd) <- m) {
        dd1 = BoolFormulaBDD.creator.makeCompose(dd1, x.id, dd)
      }
      dd1
    }
  }

  /**
    * Returns the left-biased composition of `this` substitution with `that` substitution.
    */
  def ++(that: BDD_Substitution): BDD_Substitution = {
    if (this.isEmpty) {
      that
    } else if (that.isEmpty) {
      this
    } else {
      BDD_Substitution(
        this.m ++ that.m.filter(kv => !this.m.contains(kv._1))
      )
    }
  }

  def toSubstitution(var_map: Bimap[VarOrEff, Int], loc: SourceLocation): Substitution = {
    if(isEmpty) Substitution.empty else {
      var sub = Substitution.empty
      for ((x, dd) <- this.m) {
        val f = BoolFormulaBDD.createFormulaFromBDD(dd)
        val tpe = BoolFormula.toType(f, var_map, Kind.Bool, loc)
        val x_sub = Substitution.singleton(x, tpe)
        sub = sub ++ x_sub
      }
      sub
    }
  }

}
*/
