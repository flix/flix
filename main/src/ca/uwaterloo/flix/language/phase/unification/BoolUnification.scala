/*
 *  Copyright 2023 Magnus Madsen
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
import ca.uwaterloo.flix.language.ast.*
import ca.uwaterloo.flix.language.phase.unification.shared.{BoolSubstitution, SveAlgorithm}
import ca.uwaterloo.flix.util.Result.{Ok, ToErr, ToOk}
import ca.uwaterloo.flix.util.collection.Bimap
import ca.uwaterloo.flix.util.{InternalCompilerException, Result}

/**
 * An implementation of Boolean Unification is for the `Bool` kind.
 */
object BoolUnification {

  /**
   * Returns the most general unifier of the two given Boolean formulas `tpe1` and `tpe2`.
   */
  def unify(tpe1: Type, tpe2: Type, renv0: RigidityEnv)(implicit flix: Flix): Result[(Substitution, List[Ast.BroadEqualityConstraint]), UnificationError] = {
    // Give up early if either type contains an associated type.
    if (Type.hasAssocType(tpe1) || Type.hasAssocType(tpe2)) {
      return Ok((Substitution.empty, List(Ast.BroadEqualityConstraint(tpe1, tpe2))))
    }

    // Check for Type.Error
    (tpe1, tpe2) match {
      case (Type.Cst(TypeConstructor.Error(_, _), _), _) => return Ok((Substitution.empty, Nil))
      case (_, Type.Cst(TypeConstructor.Error(_, _), _)) => return Ok((Substitution.empty, Nil))
      case _ => // fallthrough
    }

    implicit val alg: BoolAlg[BoolFormula] = new BoolFormulaAlg

    val result = lookupOrSolve(tpe1, tpe2, renv0)
    result.map(subst => (subst, Nil))
  }


  /**
   * Lookup the unifier of `tpe1` and `tpe2` or solve them.
   */
  private def lookupOrSolve[F](tpe1: Type, tpe2: Type, renv0: RigidityEnv)
                              (implicit flix: Flix, alg: BoolAlg[F]): Result[Substitution, UnificationError] = {
    //
    // Translate the types into formulas.
    //
    val env = alg.getEnv(List(tpe1, tpe2))
    val f1 = alg.fromType(tpe1, env)
    val f2 = alg.fromType(tpe2, env)

    val renv = alg.liftRigidityEnv(renv0, env)

    //
    // Run the expensive Boolean unification algorithm.
    //
    SveAlgorithm.unify(f1, f2, renv) match {
      case None => UnificationError.MismatchedBools(tpe1, tpe2).toErr
      case Some(subst) => toTypeSubstitution(subst, env).toOk
    }
  }

  /**
   * Converts this formula substitution into a type substitution
   */
  def toTypeSubstitution[F](s: BoolSubstitution[F], env: Bimap[BoolFormula.IrreducibleEff, Int])(implicit alg: BoolAlg[F]): Substitution = {
    val map = s.m.map {
      case (k0, v0) =>
        val k = env.getBackward(k0).getOrElse(throw InternalCompilerException(s"missing key $k0", SourceLocation.Unknown))
        val tvar = k match {
          case BoolFormula.IrreducibleEff.Var(sym) => sym
          case BoolFormula.IrreducibleEff.Eff(sym) => throw InternalCompilerException(s"unexpected substituted effect: ${sym}", SourceLocation.Unknown)
          case BoolFormula.IrreducibleEff.Assoc(sym, arg) => throw InternalCompilerException(s"unexpected substituted effect: ${sym}", SourceLocation.Unknown)
          case BoolFormula.IrreducibleEff.JvmToEff(t) => throw InternalCompilerException(s"unexpected substituted effect: ${t}", SourceLocation.Unknown)
        }
        val v = alg.toType(v0, env)
        (tvar, v)
    }
    Substitution(map)
  }

}
