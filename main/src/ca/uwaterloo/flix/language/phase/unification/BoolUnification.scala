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
import ca.uwaterloo.flix.language.ast.shared.BroadEqualityConstraint
import ca.uwaterloo.flix.language.phase.unification.shared.{BoolAlg, BoolSubstitution, SveAlgorithm}
import ca.uwaterloo.flix.util.Result.{Ok, ToErr, ToOk}
import ca.uwaterloo.flix.util.collection.Bimap
import ca.uwaterloo.flix.util.{InternalCompilerException, Result}

import scala.collection.immutable.SortedSet

/**
  * An implementation of Boolean Unification is for the `Bool` kind.
  */
object BoolUnification {

  /**
    * Returns the most general unifier of the two given Boolean formulas `tpe1` and `tpe2`.
    */
  def unify(tpe1: Type, tpe2: Type, renv0: RigidityEnv)(implicit flix: Flix): Result[(Substitution, List[BroadEqualityConstraint]), UnificationError] = {
    // Give up early if either type contains an associated type.
    if (Type.hasAssocType(tpe1) || Type.hasAssocType(tpe2)) {
      return Ok((Substitution.empty, List(BroadEqualityConstraint(tpe1, tpe2))))
    }

    // Check for Type.Error
    (tpe1, tpe2) match {
      case (Type.Cst(TypeConstructor.Error(_, _), _), _) => return Ok((Substitution.empty, Nil))
      case (_, Type.Cst(TypeConstructor.Error(_, _), _)) => return Ok((Substitution.empty, Nil))
      case _ => // fallthrough
    }

    val result = lookupOrSolve(tpe1, tpe2, renv0)
    result.map(subst => (subst, Nil))
  }


  /**
    * Lookup the unifier of `tpe1` and `tpe2` or solve them.
    */
  private def lookupOrSolve(tpe1: Type, tpe2: Type, renv0: RigidityEnv): Result[Substitution, UnificationError] = {
    implicit val alg: BoolAlg[BoolFormula] = BoolFormula.BoolFormulaAlg

    //
    // Translate the types into formulas.
    //
    val env = getEnv(List(tpe1, tpe2))
    val f1 = fromType(tpe1, env)
    val f2 = fromType(tpe2, env)

    val renv = liftRigidityEnv(renv0, env)

    //
    // Run the expensive Boolean unification algorithm.
    //
    SveAlgorithm.unify(f1, f2, renv) match {
      case None => UnificationError.MismatchedBools(tpe1, tpe2).toErr
      case Some(subst) => toTypeSubstitution(subst, env).toOk
    }
  }

  /**
    * Returns an environment built from the given types mapping between type variables and formula variables.
    *
    * This environment should be used in the functions [[toType]] and [[fromType]].
    */
  def getEnv(fs: List[Type]): Bimap[IrreducibleEff, Int] = {
    // Compute the variables in `tpe`.
    val tvars =
      fs.foldLeft(SortedSet.empty[Symbol.KindedTypeVarSym])((acc, tpe) => acc ++ tpe.typeVars.map(_.sym))
        .toList.map(IrreducibleEff.Var.apply)

    val effs =
      fs.foldLeft(SortedSet.empty[Symbol.EffectSym])((acc, tpe) => acc ++ tpe.effects)
        .toList.map(IrreducibleEff.Eff.apply)

    // Construct a bi-directional map from type variables to indices.
    // The idea is that the first variable becomes x0, the next x1, and so forth.
    (tvars ++ effs).zipWithIndex.foldLeft(Bimap.empty[IrreducibleEff, Int]) {
      case (macc, (sym, x)) => macc + (sym -> x)
    }
  }

  /**
    * Returns a rigidity environment on formulas that is equivalent to the given one on types.
    */
  def liftRigidityEnv(renv: RigidityEnv, env: Bimap[IrreducibleEff, Int]): SortedSet[Int] = {
    val rigidVars = renv.s.flatMap {
      case tvar => env.getForward(IrreducibleEff.Var(tvar))
    }
    val effs = env.m1.collect {
      case (IrreducibleEff.Eff(_), i) => i
    }
    rigidVars ++ effs
  }

  /**
    * Converts this formula substitution into a type substitution
    */
  def toTypeSubstitution(s: BoolSubstitution[BoolFormula], env: Bimap[IrreducibleEff, Int]): Substitution = {
    val map = s.m.map {
      case (k0, v0) =>
        val k = env.getBackward(k0).getOrElse(throw InternalCompilerException(s"missing key $k0", SourceLocation.Unknown))
        val tvar = k match {
          case IrreducibleEff.Var(sym) => sym
          case IrreducibleEff.Eff(sym) => throw InternalCompilerException(s"unexpected substituted effect: $sym", SourceLocation.Unknown)
          case IrreducibleEff.Assoc(sym, _) => throw InternalCompilerException(s"unexpected substituted effect: $sym", SourceLocation.Unknown)
          case IrreducibleEff.JvmToEff(t) => throw InternalCompilerException(s"unexpected substituted effect: $t", SourceLocation.Unknown)
        }
        val v = toType(v0, env)
        (tvar, v)
    }
    Substitution(map)
  }

  /**
    * Converts the given type t into a formula.
    */
  private def fromType[F](t: Type, env: Bimap[IrreducibleEff, Int])(implicit alg: BoolAlg[F]): F = Type.eraseTopAliases(t) match {
    case Type.Var(sym, _) => env.getForward(IrreducibleEff.Var(sym)) match {
      case None => throw InternalCompilerException(s"Unexpected unbound variable: '$sym'.", sym.loc)
      case Some(x) => alg.mkVar(x)
    }
    case Type.True => alg.mkTop
    case Type.False => alg.mkBot
    case Type.Apply(Type.Cst(TypeConstructor.Not, _), tpe1, _) => alg.mkNot(fromType(tpe1, env))
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.And, _), tpe1, _), tpe2, _) => alg.mkAnd(fromType(tpe1, env), fromType(tpe2, env))
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Or, _), tpe1, _), tpe2, _) => alg.mkOr(fromType(tpe1, env), fromType(tpe2, env))
    case _ => throw InternalCompilerException(s"Unexpected type: '$t'.", t.loc)
  }

  private def toType(f: BoolFormula, env: Bimap[IrreducibleEff, Int]): Type = f match {
    case BoolFormula.True => Type.True
    case BoolFormula.False => Type.False
    case BoolFormula.And(f1, f2) => Type.mkAnd(toType(f1, env), toType(f2, env), SourceLocation.Unknown)
    case BoolFormula.Or(f1, f2) => Type.mkOr(toType(f1, env), toType(f2, env), SourceLocation.Unknown)
    case BoolFormula.Not(f1) => Type.mkNot(toType(f1, env), SourceLocation.Unknown)
    case BoolFormula.Var(id) => env.getBackward(id) match {
      case Some(IrreducibleEff.Var(sym)) => Type.Var(sym, SourceLocation.Unknown)
      case Some(IrreducibleEff.Eff(sym)) => Type.Cst(TypeConstructor.Effect(sym), SourceLocation.Unknown)
      case Some(IrreducibleEff.Assoc(sym, arg)) => Type.AssocType(Ast.AssocTypeConstructor(sym, SourceLocation.Unknown), arg, Kind.Eff, SourceLocation.Unknown)
      case Some(IrreducibleEff.JvmToEff(t)) => t
      case None => throw InternalCompilerException(s"unexpected unknown ID: $id", SourceLocation.Unknown)
    }
  }

  /**
    * An irreducible effect.
    */
  sealed trait IrreducibleEff

  object IrreducibleEff {

    case class Var(sym: Symbol.KindedTypeVarSym) extends IrreducibleEff

    case class Eff(sym: Symbol.EffectSym) extends IrreducibleEff

    case class Assoc(sym: Symbol.AssocTypeSym, arg: Type) extends IrreducibleEff

    case class JvmToEff(tpe: Type.JvmToEff) extends IrreducibleEff
  }

}
