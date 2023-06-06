/*
 *  Copyright 2020 Magnus Madsen
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
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.util.Result
import ca.uwaterloo.flix.util.Result.{Ok, ToErr, ToOk}
import org.sosy_lab.pjbdd.api.DD

// TODO EFF-MIGRATION rename to EffUnification
object BoolUnification {

  /**
    * The number of variables required before we switch to using BDDs for SVE.
    */
  private val DefaultThreshold: Int = 5

  /**
    * Returns the most general unifier of the two given Boolean formulas `tpe1` and `tpe2`.
    */
  def unify(tpe1: Type, tpe2: Type, renv0: RigidityEnv)(implicit flix: Flix): Result[(Substitution, List[Ast.BroadEqualityConstraint]), UnificationError] = {

    //
    // NOTE: ALWAYS UNSOUND. USE ONLY FOR EXPERIMENTS.
    //
    if (flix.options.xnoboolunif) {
      return (Substitution.empty, Nil).toOk
    }

    //
    // Optimize common unification queries.
    //
    if (flix.options.xprintboolunif) {
      val loc = if (tpe1.loc != SourceLocation.Unknown) tpe1.loc else tpe2.loc

      // Alpha rename variables.
      val alpha = ((tpe1.typeVars ++ tpe2.typeVars).toList.zipWithIndex).foldLeft(Map.empty[Symbol.KindedTypeVarSym, Type.Var]) {
        case (macc, (tvar, idx)) =>
          val sym = new Symbol.KindedTypeVarSym(idx, Ast.VarText.Absent, tvar.kind, tvar.sym.isRegion, tvar.loc)
          val newTvar = Type.Var(sym, tvar.loc)
          macc + (tvar.sym -> newTvar)
      }
      val subst = Substitution(alpha)
      println(s"$loc: ${subst(tpe1)} =?= ${subst(tpe2)}")
    }

    if (!flix.options.xnoboolspecialcases) {

      // Case 1: Unification of identical formulas.
      if (tpe1 eq tpe2) {
        return Ok((Substitution.empty, Nil))
      }

      // Case 2: Common unification instances.
      // Note: Order determined by code coverage.
      (tpe1, tpe2) match {
        case (Type.Var(x, _), Type.Var(y, _)) =>
          if (renv0.isFlexible(x)) {
            return Ok((Substitution.singleton(x, tpe2), Nil)) // 9000 hits
          }
          if (renv0.isFlexible(y)) {
            return Ok((Substitution.singleton(y, tpe1), Nil)) // 1000 hits
          }
          if (x == y) {
            return Ok((Substitution.empty, Nil)) // 1000 hits
          }

        case (Type.Cst(TypeConstructor.Pure, _), Type.Cst(TypeConstructor.Pure, _)) =>
          return Ok((Substitution.empty, Nil)) // 6000 hits

        case (Type.Var(x, _), Type.Cst(tc, _)) if renv0.isFlexible(x) => tc match {
          case TypeConstructor.Pure =>
            return Ok((Substitution.singleton(x, Type.Pure), Nil)) // 9000 hits
          case TypeConstructor.EffUniv =>
            return Ok((Substitution.singleton(x, Type.EffUniv), Nil)) // 1000 hits
          case _ => // nop
        }

        case (Type.Cst(tc, _), Type.Var(y, _)) if renv0.isFlexible(y) => tc match {
          case TypeConstructor.Pure =>
            return Ok((Substitution.singleton(y, Type.Pure), Nil)) // 7000 hits
          case TypeConstructor.EffUniv =>
            return Ok((Substitution.singleton(y, Type.EffUniv), Nil)) //  500 hits
          case _ => // nop
        }

        case (Type.Cst(TypeConstructor.EffUniv, _), Type.Cst(TypeConstructor.EffUniv, _)) =>
          return Ok((Substitution.empty, Nil)) //  100 hits

        case _ => // nop
      }

    }

    // Give up early if either type contains an associated type.
    if (Type.hasAssocType(tpe1) || Type.hasAssocType(tpe2)) {
      return Ok((Substitution.empty, List(Ast.BroadEqualityConstraint(tpe1, tpe2))))
    }

    // Choose the SVE implementation based on the number of variables.
    val numberOfVars = (tpe1.typeVars ++ tpe2.typeVars).size
    val threshold = flix.options.xbddthreshold.getOrElse(DefaultThreshold)

    val substRes = if (flix.options.xboolclassic) {
      implicit val alg: BoolAlg[BoolFormula] = new BoolFormulaAlgClassic
      implicit val cache: UnificationCache[BoolFormula] = UnificationCache.GlobalBool
      lookupOrSolve(tpe1, tpe2, renv0)
    } else if (numberOfVars < threshold) {
      implicit val alg: BoolAlg[BoolFormula] = new BoolFormulaAlg
      implicit val cache: UnificationCache[BoolFormula] = UnificationCache.GlobalBool
      lookupOrSolve(tpe1, tpe2, renv0)
    } else {
      implicit val alg: BoolAlg[DD] = new BddFormulaAlg
      implicit val cache: UnificationCache[DD] = UnificationCache.GlobalBdd
      lookupOrSolve(tpe1, tpe2, renv0)
    }

    substRes.map(subst => (subst, Nil))
  }


  /**
    * Lookup the unifier of `tpe1` and `tpe2` or solve them.
    */
  private def lookupOrSolve[F](tpe1: Type, tpe2: Type, renv0: RigidityEnv)
                              (implicit flix: Flix, alg: BoolAlg[F], cache: UnificationCache[F]): Result[Substitution, UnificationError] = {
    //
    // Translate the types into formulas.
    //
    val env = alg.getEnv(List(tpe1, tpe2))
    val f1 = alg.fromType(tpe1, env)
    val f2 = alg.fromType(tpe2, env)

    val renv = alg.liftRigidityEnv(renv0, env)

    //
    // Lookup the query to see if it is already in unification cache.
    //
    if (!flix.options.xnoboolcache) {
      cache.lookup(f1, f2, renv) match {
        case None => // cache miss: must compute the unification.
        case Some(subst) =>
          // cache hit: return the found substitution.
          return subst.toTypeSubstitution(env).toOk
      }
    }

    //
    // Run the expensive Boolean unification algorithm.
    //
    booleanUnification(f1, f2, renv) match {
      case None => UnificationError.MismatchedBools(tpe1, tpe2).toErr
      case Some(subst) =>
        if (!flix.options.xnoboolcache) {
          cache.put(f1, f2, renv, subst)
        }
        subst.toTypeSubstitution(env).toOk
    }
  }

  /**
    * Returns the most general unifier of the two given Boolean formulas `tpe1` and `tpe2`.
    */
  private def booleanUnification[F](tpe1: F, tpe2: F, renv: Set[Int])
                                   (implicit flix: Flix, alg: BoolAlg[F]): Option[BoolSubstitution[F]] = {
    // The boolean expression we want to show is 0.
    val query = alg.mkXor(tpe1, tpe2)

    // Compute the variables in the query.
    val typeVars = alg.freeVars(query).toList

    // Compute the flexible variables.
    val flexibleTypeVars = typeVars.filterNot(renv.contains)

    // Determine the order in which to eliminate the variables.
    val freeVars = computeVariableOrder(flexibleTypeVars)

    // Eliminate all variables.
    try {
      val subst = successiveVariableElimination(query, freeVars)

      //    if (!subst.isEmpty) {
      //      val s = subst.toString
      //      val len = s.length
      //      if (len > 50) {
      //        println(s.substring(0, Math.min(len, 300)))
      //        println()
      //      }
      //    }

      Some(subst)
    } catch {
      case ex: BoolUnificationException => None
    }
  }

  /**
    * A heuristic used to determine the order in which to eliminate variable.
    *
    * Semantically the order of variables is immaterial. Changing the order may
    * yield different unifiers, but they are all equivalent. However, changing
    * the can lead to significant speed-ups / slow-downs.
    *
    * We make the following observation:
    *
    * We want to have synthetic variables (i.e. fresh variables introduced during
    * type inference) expressed in terms of real variables (i.e. variables that
    * actually occur in the source code). We can ensure this by eliminating the
    * synthetic variables first.
    */
  private def computeVariableOrder(l: List[Int]): List[Int] = {
    l.reverse // TODO have to reverse the order for regions to work
  }

  /**
    * Performs success variable elimination on the given boolean expression `f`.
    *
    * `flexvs` is the list of remaining flexible variables in the expression.
    */
  private def successiveVariableElimination[F](f: F, flexvs: List[Int])(implicit flix: Flix, alg: BoolAlg[F]): BoolSubstitution[F] = flexvs match {
    case Nil =>
      // Determine if f is unsatisfiable when all (rigid) variables are made flexible.
      if (!alg.satisfiable(f))
        BoolSubstitution.empty
      else
        throw BoolUnificationException()

    case x :: xs =>
      val t0 = BoolSubstitution.singleton(x, alg.mkFalse)(f)
      val t1 = BoolSubstitution.singleton(x, alg.mkTrue)(f)
      val se = successiveVariableElimination(alg.mkAnd(t0, t1), xs)

      val f1 = alg.minimize(alg.mkOr(se(t0), alg.mkAnd(alg.mkVar(x), alg.mkNot(se(t1)))))
      val st = BoolSubstitution.singleton(x, f1)
      st ++ se
  }

}
