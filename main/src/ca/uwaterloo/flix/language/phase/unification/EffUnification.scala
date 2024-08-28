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
import ca.uwaterloo.flix.language.ast.shared.Scope
import ca.uwaterloo.flix.util.Result.{Ok, ToErr, ToOk}
import ca.uwaterloo.flix.util.{InternalCompilerException, Result}
import org.sosy_lab.pjbdd.api.DD

import scala.collection.mutable

object EffUnification {

  /**
    * The number of variables required before we switch to using BDDs for SVE.
    */
  private val DefaultThreshold: Int = 5

  /**
    * Returns the most general unifier of the two given Boolean formulas `tpe1` and `tpe2`.
    */
  def unify(tpe1: Type, tpe2: Type, renv0: RigidityEnv)(implicit scope: Scope, flix: Flix): Result[(Substitution, List[Ast.BroadEqualityConstraint]), UnificationError] = {

    // TODO: Levels
    // We cannot enforce that all variables should belong to the same level.
    // Consider the equation X^1 and Y^1 ~ Z^0 or W^2 with levels indicated by superscripts.
    // The minimum level is 0, so we could set the level of X, Y, Z, and W to 0, but this is incorrect.
    // The reason is that there is the following unifier:
    //
    // w -> (w ∨ ¬z) ∧ (z ∨ (x ∧ y))
    // x ->	x ∨ z
    // y -> y ∨ z
    // z -> z
    //
    // which satisfies the levels!
    //
    // It is not yet clear how to enforce this, hence we unsoundly do not
    //

    //
    // NOTE: ALWAYS UNSOUND. USE ONLY FOR EXPERIMENTS.
    //
    if (flix.options.xnoboolunif) {
      return (Substitution.empty, Nil).toOk
    }

    //
    // Debug BU queries.
    //
    //    // Alpha rename variables.
    //    val alpha = ((tpe1.typeVars ++ tpe2.typeVars).toList.zipWithIndex).foldLeft(Map.empty[Symbol.KindedTypeVarSym, Type.Var]) {
    //      case (macc, (tvar, idx)) =>
    //        val sym = new Symbol.KindedTypeVarSym(idx, Ast.VarText.Absent, tvar.kind, tvar.sym.isRegion, tvar.sym.level, tvar.loc)
    //        val newTvar = Type.Var(sym, tvar.loc)
    //        macc + (tvar.sym -> newTvar)
    //    }
    //    val subst = Substitution(alpha)
    //    val loc = if (tpe1.loc != SourceLocation.Unknown) tpe1.loc else tpe2.loc
    //    println(s"${loc.formatWithLine}: ${subst(tpe1)} =?= ${subst(tpe2)}")

    //
    // Optimize common unification queries.
    //
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
          case TypeConstructor.Univ =>
            return Ok((Substitution.singleton(x, Type.Univ), Nil)) // 1000 hits
          case _ => // nop
        }

        case (Type.Cst(tc, _), Type.Var(y, _)) if renv0.isFlexible(y) => tc match {
          case TypeConstructor.Pure =>
            return Ok((Substitution.singleton(y, Type.Pure), Nil)) // 7000 hits
          case TypeConstructor.Univ =>
            return Ok((Substitution.singleton(y, Type.Univ), Nil)) //  500 hits
          case _ => // nop
        }

        case (Type.Cst(TypeConstructor.Univ, _), Type.Cst(TypeConstructor.Univ, _)) =>
          return Ok((Substitution.empty, Nil)) //  100 hits

        case (Type.Cst(TypeConstructor.Error(_, _), _), _) =>
          return Ok((Substitution.empty, Nil))

        case (_, Type.Cst(TypeConstructor.Error(_, _), _)) =>
          return Ok((Substitution.empty, Nil))

//        case (t1@Type.Var(x, _), t2) if renv0.isFlexible(x) && !t2.typeVars.contains(t1) =>
//          return Ok(Substitution.singleton(x, t2), Nil)
//
//        case (t1, t2@Type.Var(x, _)) if renv0.isFlexible(x) && !t1.typeVars.contains(t2) =>
//          return Ok(Substitution.singleton(x, t1), Nil)

        case _ => // nop
      }

    }

    //
    // We handle associated types by:
    // 1. replacing each associated type with a fresh rigid variable
    // 2. performing unification
    // 3. replacing the variables with the types they represent
    //    - we do this by composing a substitution back to the associated types
    //      with the substitution from unification
    //
    // First clear any associated types from the types, temporarily replacing simple associated types with variables
    clearAssocs(tpe1, tpe2, renv0) match {
      // Case 1: There were non-reduced associated types. Give up and defer them.
      case None => Ok((Substitution.empty, List(Ast.BroadEqualityConstraint(tpe1, tpe2))))

      // Case 2: All associated types were reduced. Continue with unification.
      case Some((t1, t2, back)) =>

        // add the associated type marker variables to the rigidity environment
        val renv = back.keys.foldLeft(renv0)(_.markRigid(_))

        // build a substitution from the variable mapping
        val backSubst = back.map {
          case (sym, (assoc, tvar)) =>
            val tpe = Type.AssocType(Ast.AssocTypeConstructor(assoc, SourceLocation.Unknown), Type.Var(tvar, SourceLocation.Unknown), sym.kind, SourceLocation.Unknown)
            sym -> tpe
        }

        // Choose the SVE implementation based on the number of variables.
        val numberOfVars = (t1.typeVars ++ t2.typeVars).size
        val threshold = flix.options.xbddthreshold.getOrElse(DefaultThreshold)

        val substRes = if (numberOfVars < threshold) {
          implicit val alg: BoolAlg[BoolFormula] = new BoolFormulaAlg
          implicit val cache: UnificationCache[BoolFormula] = UnificationCache.GlobalBool
          lookupOrSolve(t1, t2, renv)
        } else {
          implicit val alg: BoolAlg[DD] = new BddFormulaAlg
          implicit val cache: UnificationCache[DD] = UnificationCache.GlobalBdd
          lookupOrSolve(t1, t2, renv)
        }

        substRes.map {
          case subst0 =>
            // replace the temporary variables with the assoc types they represent
            val subst1 = Substitution(backSubst) @@ subst0
            // remove the temporary variables from the substitution
            val subst = backSubst.keys.foldLeft(subst1)(_.unbind(_))
            (subst, Nil)
        }.mapErr {
          // If there is an error, apply the back-substitution to get the right types in the message.
          case UnificationError.MismatchedEffects(eff1, eff2) =>
            UnificationError.MismatchedEffects(Substitution(backSubst)(eff1), Substitution(backSubst)(eff2))
          case _ => throw InternalCompilerException("unexpected error", SourceLocation.Unknown)
        }
    }
  }

  /**
    * Replaces all associated types with fresh type variables.
    *
    * Returns the two types and a map from the fresh type variables to the associated types they represent.
    *
    * If any associated type is not fully reduced (i.e. is not of the form T[α] where α is rigid)
    * then returns None.
    */
  private def clearAssocs(tpe1: Type, tpe2: Type, renv: RigidityEnv)(implicit scope: Scope, flix: Flix): Option[(Type, Type, Map[Symbol.KindedTypeVarSym, (Symbol.AssocTypeSym, Symbol.KindedTypeVarSym)])] = {
    val cache = mutable.HashMap.empty[(Symbol.AssocTypeSym, Symbol.KindedTypeVarSym), Symbol.KindedTypeVarSym]

    def visit(t0: Type): Option[Type] = t0 match {
      case t: Type.Var => Some(t)
      case t: Type.Cst => Some(t)
      case Type.Apply(tpe1, tpe2, loc) =>
        for {
          t1 <- visit(tpe1)
          t2 <- visit(tpe2)
        } yield Type.Apply(t1, t2, loc)
      case Type.Alias(_, _, tpe, _) => visit(tpe)
      case Type.AssocType(Ast.AssocTypeConstructor(assoc, _), Type.Var(tvar, _), kind, _) if renv.isRigid(tvar) =>
        // We use top scope as the variables will be marked as rigid anyway
        val sym = cache.getOrElseUpdate((assoc, tvar), Symbol.freshKindedTypeVarSym(Ast.VarText.Absent, kind, isRegion = false, SourceLocation.Unknown)(Scope.Top, flix))
        Some(Type.Var(sym, SourceLocation.Unknown))
      case Type.AssocType(_, _, _, _) => None
    }

    for {
      t1 <- visit(tpe1)
      t2 <- visit(tpe2)
    } yield {
      // build the backward map by reversing the cache
      val map = cache.map {
        case (assoc, tvar) => tvar -> assoc
      }.toMap
      (t1, t2, map)
    }

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
      case None => UnificationError.MismatchedEffects(tpe1, tpe2).toErr
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
