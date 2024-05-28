/*
 * Copyright 2024 Matthew Lutze
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package ca.uwaterloo.flix.language.phase.typer

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{Ast, Kind, KindedAst, RigidityEnv, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.errors.TypeError
import ca.uwaterloo.flix.language.phase.typer.TypeConstraint.Provenance
import ca.uwaterloo.flix.language.phase.unification.Unification.getUnderOrOverAppliedError
import ca.uwaterloo.flix.language.phase.unification._
import ca.uwaterloo.flix.util.Result.Err
import ca.uwaterloo.flix.util.collection.{ListMap, ListOps}
import ca.uwaterloo.flix.util.{InternalCompilerException, Result, Validation}

import scala.annotation.tailrec

/**
  * Constraint resolution works by iteratively building up a substitution from the constraints.
  *
  * Given a constraint set, we
  * 1. select a constraint from the set,
  * 2. attempt to resolve it, yielding a substitution and new constraints
  * 3. apply the substitution to the accumulated substitution and add the new constraints to our set
  *
  * We repeat this until we cannot make any more progress or we discover an invalid constraint.
  */
object ConstraintSolver {

  /**
    * The maximum number of resolution iterations before we throw an error.
    */
  private val MaxIterations = 1000

  /**
    * Resolves constraints in the given definition using the given inference result.
    */
  def visitDef(defn: KindedAst.Def, infResult: InfResult, renv0: RigidityEnv, tconstrs0: List[Ast.TypeConstraint], tenv0: Map[Symbol.TraitSym, Ast.TraitContext], eqEnv0: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef], root: KindedAst.Root)(implicit flix: Flix): Validation[Substitution, TypeError] = defn match {
    case KindedAst.Def(sym, spec, _) =>
      if (flix.options.xprinttyper.contains(sym.toString)) {
        Debug.startRecording()
      }
      visitSpec(spec, infResult, renv0, tconstrs0, tenv0, eqEnv0, root)
  }

  /**
    * Resolves constraints in the given signature using the given inference result.
    */
  def visitSig(sig: KindedAst.Sig, infResult: InfResult, renv0: RigidityEnv, tconstrs0: List[Ast.TypeConstraint], tenv0: Map[Symbol.TraitSym, Ast.TraitContext], eqEnv0: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef], root: KindedAst.Root)(implicit flix: Flix): Validation[Substitution, TypeError] = sig match {
    case KindedAst.Sig(_, _, None) => Validation.success(Substitution.empty)
    case KindedAst.Sig(sym, spec, Some(_)) =>
      if (flix.options.xprinttyper.contains(sym.toString)) {
        Debug.startRecording()
      }
      visitSpec(spec, infResult, renv0, tconstrs0, tenv0, eqEnv0, root)
  }

  /**
    * Resolves constraints in the given spec using the given inference result.
    */
  def visitSpec(spec: KindedAst.Spec, infResult: InfResult, renv0: RigidityEnv, tconstrs0: List[Ast.TypeConstraint], tenv0: Map[Symbol.TraitSym, Ast.TraitContext], eqEnv0: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef], root: KindedAst.Root)(implicit flix: Flix): Validation[Substitution, TypeError] = spec match {
    case KindedAst.Spec(_, _, _, _, fparams, _, tpe, eff, tconstrs, econstrs, loc) =>

      val InfResult(infConstrs, infTpe, infEff, infRenv) = infResult

      // The initial substitution maps from formal parameters to their types
      val initialSubst = fparams.foldLeft(Substitution.empty) {
        case (acc, KindedAst.FormalParam(sym, mod, tpe, src, loc)) => acc ++ Substitution.singleton(sym.tvar.sym, openOuterSchema(tpe))
      }

      // Wildcard tparams are not counted in the tparams, so we need to traverse the types to get them.
      val allTparams = tpe.typeVars ++ eff.typeVars ++ fparams.flatMap(_.tpe.typeVars) ++ econstrs.flatMap(_.tpe2.typeVars)

      // The rigidity environment is made up of:
      // 1. rigid variables from the context (e.g. from instance type parameters)
      // 2. rigid variables from type inference (e.g. regions)
      // 3. rigid variables from declared function type parameters
      val renv = allTparams.foldLeft(infRenv ++ renv0) {
        case (acc, Type.Var(sym, _)) => acc.markRigid(sym)
      }

      // The trait and equality environments are made up of:
      // 1. constraints from the context (e.g. constraints on instances and traits, plus global constraints)
      // 2. constraints from the function signature
      val cenv = expandTraitEnv(tenv0, tconstrs ++ tconstrs0)
      val eenv = expandEqualityEnv(eqEnv0, econstrs) // TODO ASSOC-TYPES allow econstrs on instances

      // We add extra constraints for the declared type and effect
      val declaredTpeConstr = TypeConstraint.Equality(tpe, infTpe, Provenance.ExpectType(expected = tpe, actual = infTpe, loc))
      val declaredEffConstr = TypeConstraint.Equality(eff, infEff, Provenance.ExpectEffect(expected = eff, actual = infEff, loc))
      val constrs = declaredTpeConstr :: declaredEffConstr :: infConstrs

      ///////////////////////////////////////////////////////////////////
      //             This is where the stuff happens!                  //
      // We resolve the constraints under the environments we created. //
      ///////////////////////////////////////////////////////////////////
      resolve(constrs, initialSubst, renv)(cenv, eenv, flix) match {
        case Result.Ok(ResolutionResult(subst, deferred)) =>
          Debug.stopRecording()
          // If there are any constraints we could not resolve, then we report an error.
          // TODO ASSOC-TYPES here we only consider the first error
          getFirstError(deferred, renv) match {
            // Case 1: Type constraints are resolved.
            case None => Validation.success(subst)
            // Case 2: Type constraints are not resolved.
            case Some(err) => Validation.toSoftFailure(subst, err)
          }

        case Result.Err(err) => Validation.toSoftFailure(Substitution.empty, err)
      }
  }

  /**
    * Adds the given type constraints as assumptions to the trait environment.
    *
    * Transitively adds the supertraits of the constraints.
    * For example, given the trait environment:
    *
    * {{{
    *   trait Order[a] with Eq[a]
    *   instance Eq[String]
    *   instance Order[String]
    * }}}
    *
    * If we add
    * {{{
    *   instance Order[b]
    * }}}
    *
    * then we get
    * {{{
    *   trait Order[a] with Eq[a]
    *   instance Eq[String]
    *   instance Order[String]
    *
    *   instance Eq[b]
    *   instance Order[b]
    * }}}
    */
  def expandTraitEnv(tenv: Map[Symbol.TraitSym, Ast.TraitContext], tconstrs: List[Ast.TypeConstraint]): Map[Symbol.TraitSym, Ast.TraitContext] = {
    tconstrs.flatMap(withSupers(_, tenv)).foldLeft(tenv) {
      case (acc, Ast.TypeConstraint(Ast.TypeConstraint.Head(sym, _), arg, loc)) =>
        val inst = Ast.Instance(arg, Nil)
        val context = acc.get(sym) match {
          case Some(Ast.TraitContext(supers, insts)) => Ast.TraitContext(supers, inst :: insts)
          case None => throw InternalCompilerException(s"unexpected unknown trait sym: $sym", loc)
        }
        acc + (sym -> context)
    }
  }

  /**
    * Adds the given equality constraints as assumptions to the equality environment.
    *
    * For example, given the equality environment:
    * {{{
    *   Elm[List[a]] ~ a
    * }}}
    *
    * If we add
    * {{{
    *   Elm[b] ~ String
    * }}}
    *
    * then we get
    * {{{
    *   Elm[List[a]] ~ a
    *   Elm[b] ~ String
    * }}}
    */
  def expandEqualityEnv(eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef], econstrs: List[Ast.EqualityConstraint]): ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef] = {
    econstrs.foldLeft(eqEnv) {
      case (acc, Ast.EqualityConstraint(Ast.AssocTypeConstructor(sym, _), tpe1, tpe2, _)) =>
        val assoc = Ast.AssocTypeDef(tpe1, tpe2)
        acc + (sym -> assoc)
    }
  }

  /**
    * Attempts to resolve the given type constraints under the given environments,
    * and with the given initial substitution.
    *
    * The initial substitution should come from e.g., formal parameter type ascriptions.
    *
    * Returns a result, either:
    * - a substitution and leftover constraints, or
    * - an error if resolution failed
    */
  def resolve(constrs: List[TypeConstraint], subst0: Substitution, renv: RigidityEnv)(implicit tenv: Map[Symbol.TraitSym, Ast.TraitContext], eenv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef], flix: Flix): Result[ResolutionResult, TypeError] = {

    // We track changes to the resolution state through mutable variables.

    // The set of remaining type constraints.
    var curr = constrs.sortBy(_.index)

    // The current substitution.
    var subst = subst0

    // The number of resolution iterations we have made.
    // We give up if this exceeds MaxIterations.
    var count = 0

    // Effect constraints that we are deferring until the end
    var effs: List[TypeConstraint.Equality] = Nil

    // Trait constraints that we are deferring until the end
    var traits: List[TypeConstraint.Trait] = Nil

    // Whether the last resolution attempt made progress.
    // We give up if a resolution attempt does not make progress.
    var progress = true

    while (progress) {
      if (count >= MaxIterations) {
        return Result.Err(TypeError.TooComplex(constrs.head.loc))
      }

      count += 1

      Debug.recordGraph(curr, effs, traits, subst)

      resolveOneOf(curr, subst, renv) match {
        // Case 1: Success. Update the tracking variables.
        case Result.Ok(IntermediateResult(newSubst, newConstrs, moreEffs, moreTraits, newProgress)) =>
          curr = newConstrs
          subst = newSubst
          effs = moreEffs ::: effs
          traits = moreTraits ::: traits
          progress = newProgress

        // Case 2: Error. Break out of the loop and return the error.
        case Result.Err(err) =>
          Debug.stopRecording()
          return Result.Err(err)
      }
    }

    // After we reduced the normal constraints as far as possible, we try the effect constraints and the trait constraints.
    for {
      s <- resolveEffConstraints(effs, renv, subst)
      _ <- resolveFinalTraitConstraints(traits, renv, s)
    } yield ResolutionResult(s, curr)

  }

  /**
    * Tries to resolve one of the given constraints.
    *
    * Applies the initial substitution `subst0` before attempting to resolve.
    */
  private def resolveOneOf(constrs: List[TypeConstraint], subst0: Substitution, renv: RigidityEnv)(implicit tenv: Map[Symbol.TraitSym, Ast.TraitContext], eenv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef], flix: Flix): Result[IntermediateResult, TypeError] = {
    def tryResolve(cs: List[TypeConstraint]): Result[IntermediateResult, TypeError] = cs match {
      case Nil => Result.Ok(IntermediateResult(subst0, Nil, Nil, Nil, progress = false))
      case hd :: tl => resolveOne(hd, renv, subst0).flatMap {
        // if we're just returning the same constraint, then have made no progress and we need to find something else to reduce
        case hdRes if !hdRes.progress => tryResolve(tl).map {
          case tlRes => tlRes.copy(constrs = hd :: tlRes.constrs)
        }
        // otherwise we have made progress so we're happy
        case res => Result.Ok(res.copy(constrs = res.constrs ::: tl))
      }
    }

    tryResolve(constrs.sortBy(_.index))
  }

  /**
    * Tries to resolve the given constraint.
    */
  private def resolveOne(constr0: TypeConstraint, renv: RigidityEnv, subst0: Substitution)(implicit tenv: Map[Symbol.TraitSym, Ast.TraitContext], eenv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef], flix: Flix): Result[IntermediateResult, TypeError] = constr0 match {
    case TypeConstraint.Equality(tpe1, tpe2, prov0) =>
      val t1 = TypeMinimization.minimizeType(subst0(tpe1))
      val t2 = TypeMinimization.minimizeType(subst0(tpe2))
      val prov = subst0(prov0)
      resolveEquality(t1, t2, prov, renv, constr0.loc).map {
        case IntermediateResult(subst, constrs, effs, traits, p) => IntermediateResult(subst @@ subst0, constrs, effs, traits, progress = p)
      }

    case c: TypeConstraint.Trait =>
      // Push trait constraints into the trait constraint bucket
      Result.Ok(IntermediateResult(subst0, Nil, Nil, List(c), progress = true))

    // If we have resolved everything under the purification constraint,
    // then we can perform the purification
    case TypeConstraint.Purification(sym, eff1, eff2, prov, Nil, Nil, Nil) =>
      val e1 = subst0(eff1)
      // purify the inner type
      val e2Raw = subst0(eff2)
      val e2 = Substitution.singleton(sym, Type.Pure)(e2Raw)
      val qvars = e2Raw.typeVars.map(_.sym)
      val subst = qvars.foldLeft(subst0)(_.unbind(_))
      val constr = TypeConstraint.Equality(e1, TypeMinimization.minimizeType(e2), prov)
      Result.Ok(IntermediateResult(subst, List(constr), Nil, Nil, progress = true))

    // If we have resolved everything except the traits,
    // then we resolve the traits
    case TypeConstraint.Purification(sym, eff1, eff2, prov, Nil, Nil, nestedTraits0) =>
      resolveOneTraitOf(nestedTraits0, renv, subst0).map {
        case (nestedTraits, progress) =>
          val constr = TypeConstraint.Purification(sym, eff1, eff2, prov, Nil, Nil, nestedTraits)
          IntermediateResult(subst0, List(constr), Nil, Nil, progress)
      }


    // If we have resolved all the type constraints under the purification constraint,
    // then we can resolve the effects
    case TypeConstraint.Purification(sym, eff1, eff2, prov, Nil, nestedEffs0, nestedTraits) =>
      resolveEffConstraints(nestedEffs0, renv, subst0).map {
        case subst =>
          val constr = TypeConstraint.Purification(sym, eff1, eff2, prov, Nil, Nil, nestedTraits)
          IntermediateResult(subst, List(constr), Nil, Nil, progress = true)
      }

    // If we have not yet resolved all the constraints under the purification constraint,
    // Then we must resolve the type constraints first.
    case TypeConstraint.Purification(sym, eff1, eff2, prov, nested0, nestedEffs0, nestedTraits) =>
      // First reduce nested constraints
      resolveOneOf(nested0, subst0, renv).map {
        case IntermediateResult(subst, newConstrs, effs, traits, progress) =>
          val constr = TypeConstraint.Purification(sym, eff1, eff2, prov, newConstrs, effs ::: nestedEffs0, traits ::: nestedTraits)
          IntermediateResult(subst, List(constr), Nil, Nil, progress)
      }
  }

  /**
    * Resolves the given list of effect constraints, after applying the given substitution.
    *
    * Returns a substitution that unifies all the constraints and merges it with the given substitution.
    */
  private def resolveEffConstraints(effConstrs0: List[TypeConstraint.Equality], renv: RigidityEnv, subst0: Substitution)(implicit eenv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef], flix: Flix): Result[Substitution, TypeError] = {
    // Short-circuit if the constraint list is empty
    if (effConstrs0.isEmpty) {
      return Result.Ok(subst0)
    }

    val effConstrs1 = effConstrs0.map(subst0.apply)
    for {
      effConstrs2 <- Result.traverse(effConstrs1)(simplifyEffConstraint(_, renv))
      effs = effConstrs2.map { case TypeConstraint.Equality(eff1, eff2, prov) => (eff1, eff2, prov.loc) }
      subst1 <- EffUnification2.unifyAll(effs, renv, effs.head._3).mapErr(toTypeError(_, effConstrs0.head.prov))
    } yield subst1 @@ subst0
  }

  /**
    * Resolves one of the given trait constraints.
    *
    * Applies the substitution before attempting to resolve.
    */
  private def resolveOneTraitOf(tconstrs: List[TypeConstraint.Trait], renv: RigidityEnv, subst0: Substitution)(implicit tenv: Map[Symbol.TraitSym, Ast.TraitContext], eenv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef], flix: Flix): Result[(List[TypeConstraint.Trait], Boolean), TypeError] = {
    def tryResolve(cs: List[TypeConstraint.Trait]): Result[(List[TypeConstraint.Trait], Boolean), TypeError] = cs match {
      case Nil => Result.Ok((Nil, false))
      case (hd@TypeConstraint.Trait(sym, tpe, loc)) :: tl => resolveTraitConstraint(sym, subst0(tpe), renv, loc).flatMap {
        // if we're just returning the same constraint, then have made no progress and we need to find something else to reduce
        case (_, false) => tryResolve(tl).map {
          case (tlRes, tlProgress) => (hd :: tlRes, tlProgress)
        }
        // otherwise we have made progress so we're happy
        case (res, true) => Result.Ok((res ::: tl, true))
      }
    }

    tryResolve(tconstrs)
  }

  /**
    * Resolves the given list of trait constraints, after applying the given substitution.
    *
    * Should only be applied when all other constraints have been resolved.
    */
  private def resolveFinalTraitConstraints(tconstrs: List[TypeConstraint.Trait], renv: RigidityEnv, subst0: Substitution)(implicit tenv: Map[Symbol.TraitSym, Ast.TraitContext], eenv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef], flix: Flix): Result[Unit, TypeError] = {
    var curr = tconstrs.map(subst0.apply)

    while (curr.nonEmpty) {
      val TypeConstraint.Trait(sym, tpe, loc) = curr.head

      resolveTraitConstraint(sym, tpe, renv, loc) match {
        // Case 1.1: We broke down the constraint. Put the new constraints in the list.
        case Result.Ok((constrs, true)) =>
          curr = constrs ::: curr.tail

        // Case 1.2: We couldn't satisfy the constraint. Error.
        case Result.Err(e) =>
          return Result.Err(e)

        // Case 1.3: We couldn't make progress. Error.
        case Result.Ok((_, false)) =>
          return Result.Err(mkMissingInstance(sym, tpe, renv, loc))
      }
    }

    // We eliminated all the constraints, so we are done.
    Result.Ok(())
  }

  /**
    * Simplifies the types in the given effect constraint.
    */
  private def simplifyEffConstraint(effConstr: TypeConstraint.Equality, renv: RigidityEnv)(implicit eenv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef], flix: Flix): Result[TypeConstraint.Equality, TypeError] = effConstr match {
    case TypeConstraint.Equality(eff1, eff2, prov) =>
      for {
        (e1, _) <- TypeReduction.simplify(eff1, renv, prov.loc)
        (e2, _) <- TypeReduction.simplify(eff2, renv, prov.loc)
      } yield {
        TypeConstraint.Equality(e1, e2, prov)
      }
  }

  /**
    * Resolves the equality between the two given types.
    *
    * θ ⊩ᵤ τ₁ = τ₂ ⤳ u; r
    */
  private def resolveEquality(tpe1: Type, tpe2: Type, prov: Provenance, renv: RigidityEnv, loc: SourceLocation)(implicit eenv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef], flix: Flix): Result[IntermediateResult, TypeError] = (tpe1.kind, tpe2.kind) match {
    case (Kind.Eff, Kind.Eff) =>
      // push effect constraints into the eff constraints bucket
      Result.Ok(IntermediateResult.effConstraint(TypeConstraint.Equality(tpe1, tpe2, prov)))

    case (Kind.Bool, Kind.Bool) =>
      // first simplify the types to get rid of assocs if we can
      for {
        (t1, p1) <- TypeReduction.simplify(tpe1, renv, loc)
        (t2, p2) <- TypeReduction.simplify(tpe2, renv, loc)
        res0 <- BoolUnification.unify(t1, t2, renv).mapErr(toTypeError(_, prov))
        res =
          if (res0._2.isEmpty) {
            IntermediateResult.newSubst(res0._1)
          } else {
            IntermediateResult.constraints(List(TypeConstraint.Equality(t1, t2, prov)), p1 || p2)
          }
      } yield res


    case (Kind.RecordRow, Kind.RecordRow) =>
      // first simplify the types to get rid of assocs if we can
      for {
        (t1, p1) <- TypeReduction.simplify(tpe1, renv, loc)
        (t2, p2) <- TypeReduction.simplify(tpe2, renv, loc)
        res0 <- RecordUnification.unifyRows(t1, t2, renv).mapErr(toTypeError(_, prov))
        res =
          if (res0._2.isEmpty) {
            IntermediateResult.newSubst(res0._1)
          } else {
            IntermediateResult.constraints(List(TypeConstraint.Equality(t1, t2, prov)), p1 || p2)
          }
      } yield res

    case (Kind.SchemaRow, Kind.SchemaRow) =>
      // first simplify the types to get rid of assocs if we can
      for {
        (t1, _) <- TypeReduction.simplify(tpe1, renv, loc)
        (t2, _) <- TypeReduction.simplify(tpe2, renv, loc)
        res <- SchemaUnification.unifyRows(t1, t2, renv).mapErr(toTypeError(_, prov))
      } yield IntermediateResult.newSubst(res)

    case (Kind.CaseSet(sym1), Kind.CaseSet(sym2)) if sym1 == sym2 =>
      for {
        (t1, _) <- TypeReduction.simplify(tpe1, renv, loc)
        (t2, _) <- TypeReduction.simplify(tpe2, renv, loc)
        res <- CaseSetUnification.unify(t1, t2, renv, sym1.universe, sym1).mapErr(toTypeError(_, prov))
      } yield IntermediateResult.newSubst(res)

    case (k1, k2) if KindUnification.unifiesWith(k1, k2) => resolveEqualityStar(tpe1, tpe2, prov, renv, loc)

    case _ => Err(toTypeError(UnificationError.MismatchedTypes(tpe1, tpe2), prov))
  }

  /**
    * Resolves the equality between the two given types, where the types have some kind `... -> Star`
    *
    * θ ⊩ᵤ τ₁ = τ₂ ⤳ u; r
    */
  private def resolveEqualityStar(tpe1: Type, tpe2: Type, prov: Provenance, renv: RigidityEnv, loc: SourceLocation)(implicit eenv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef], flix: Flix): Result[IntermediateResult, TypeError] = (tpe1, tpe2) match {
    // reflU
    case (x: Type.Var, y: Type.Var) if (x == y) => Result.Ok(IntermediateResult.elimination)

    // varU
    case (x: Type.Var, t) if renv.isFlexible(x.sym) && !t.typeVars.contains(x) =>
      Result.Ok(IntermediateResult.newSubst(Substitution.singleton(x.sym, t)))

    // varU
    case (t, x: Type.Var) if renv.isFlexible(x.sym) && !t.typeVars.contains(x) =>
      Result.Ok(IntermediateResult.newSubst(Substitution.singleton(x.sym, t)))

    // reflU
    case (Type.Cst(c1, _), Type.Cst(c2, _)) if c1 == c2 => Result.Ok(IntermediateResult.elimination)

    case (Type.Alias(_, _, tpe, _), _) => resolveEquality(tpe, tpe2, prov, renv, loc)

    case (_, Type.Alias(_, _, tpe, _)) => resolveEquality(tpe1, tpe, prov, renv, loc)

    // appU
    case (Type.Apply(t11, t12, _), Type.Apply(t21, t22, _)) =>
      for {
        res1 <- resolveEquality(t11, t21, prov, renv, loc)
        res2 <- resolveEquality(res1.subst(t12), res1.subst(t22), res1.subst(prov), renv, loc)
      } yield res2 @@ res1

    // reflU
    case (Type.AssocType(cst1, args1, _, _), Type.AssocType(cst2, args2, _, _)) if cst1.sym == cst2.sym && args1 == args2 =>
      Result.Ok(IntermediateResult.elimination)

    // redU
    // If either side is an associated type, we try to reduce both sides.
    // This is to prevent erroneous no-progress reports when we actually could make progress on the non-matched side.
    case (assoc: Type.AssocType, tpe) =>
      for {
        (t1, p1) <- TypeReduction.simplify(assoc, renv, loc)
        (t2, p2) <- TypeReduction.simplify(tpe, renv, loc)
      } yield {
        IntermediateResult.constraints(List(TypeConstraint.Equality(t1, t2, prov)), p1 || p2)
      }

    // redU
    case (tpe, assoc: Type.AssocType) =>
      for {
        (t1, p1) <- TypeReduction.simplify(tpe, renv, loc)
        (t2, p2) <- TypeReduction.simplify(assoc, renv, loc)
      } yield {
        IntermediateResult.constraints(List(TypeConstraint.Equality(t1, t2, prov)), p1 || p2)
      }

    case _ =>
      Result.Err(toTypeError(UnificationError.MismatchedTypes(tpe1, tpe2), prov))
  }

  /**
    * Resolves the given trait constraint.
    *
    * Θ ⊩ₑ π ⤳ P
    *
    * Returns a list of resulting constraints and a Boolean flag to indicate whether progress was made.
    *
    * Constraints that cannot be resolved are left as they are.
    * These are constraints on associated types applied to variables and applied to other unresolvable types.
    *
    * Constraints that are illegal result in an Err.
    * These are constraints applied to types for which the traitEnv has no corresponding instance.
    *
    * For example:
    * {{{
    *   ToString[Int32]       ~> []
    *   ToString[(a, b)]      ~> [ToString[a], ToString[b]]
    *   ToString[a]           ~> ToString[a]
    *   ToString[Elm[a]]      ~> ToString[Elm[a]]
    *   ToString[a -> b \ ef] ~> <ERROR>
    * }}}
    */
  private def resolveTraitConstraint(trt: Symbol.TraitSym, tpe0: Type, renv0: RigidityEnv, loc: SourceLocation)(implicit tenv: Map[Symbol.TraitSym, Ast.TraitContext], eenv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef], flix: Flix): Result[(List[TypeConstraint.Trait], Boolean), TypeError] = {
    // redE
    TypeReduction.simplify(tpe0, renv0, loc).flatMap {
      case (t, progress) =>
        // Look at the head of the type.
        t.baseType match {
          // Case 1: Flexible var. It might be resolved later.
          case Type.Var(sym, _) if renv0.isFlexible(sym) =>
            Result.Ok((List(TypeConstraint.Trait(trt, t, loc)), progress))
          // Case 2: Assoc type. It might be resolved later.
          case _: Type.AssocType =>
            Result.Ok((List(TypeConstraint.Trait(trt, t, loc)), progress))
          // Case 3: Something rigid (const or rigid var). We can look this up immediately.
          case _ =>
            // we mark t's tvars as rigid so we get the substitution in the right direction
            val renv = t.typeVars.map(_.sym).foldLeft(renv0)(_.markRigid(_))
            val insts = tenv(trt).instances
            // find the first (and only) instance that matches
            val tconstrsOpt = ListOps.findMap(insts) {
              inst =>
                Unification.unifyTypes(t, inst.tpe, renv).toOption.flatMap {
                  case (subst, Nil) => Some(inst.tconstrs.map(subst.apply))
                  case (_, _ :: _) => None // if we have leftover constraints then it didn't actually unify
                }
            }
            tconstrsOpt match {
              case None =>
                t.baseType match {
                  // If it's a var, it's ok. It may be substituted later to a type we can reduce.
                  // Or it might be part of the signature an expected constraint.
                  case Type.Var(sym, loc) => Result.Ok(List(TypeConstraint.Trait(trt, t, loc)), progress)
                  // If it's an associated type, it's ok. It may be reduced later to a concrete type.
                  case _: Type.AssocType => Result.Ok(List(TypeConstraint.Trait(trt, t, loc)), progress)
                  // Otherwise it's a problem.
                  case _ => Result.Err(mkMissingInstance(trt, tpe0, renv, loc))
                }
              case Some(tconstrs) =>
                // simplify all the implied constraints
                Result.traverse(tconstrs) {
                  case Ast.TypeConstraint(Ast.TypeConstraint.Head(c, _), arg, _) =>
                    resolveTraitConstraint(c, arg, renv0, loc)
                } map {
                  case res =>
                    val cs = res.flatMap { case (c, _) => c }
                    (cs, true)
                }
            }
        }
    }
  }



  /**
    * Gets an error from the list of unresolved constraints.
    */
  private def getFirstError(deferred: List[TypeConstraint], renv: RigidityEnv)(implicit flix: Flix): Option[TypeError] = deferred match {
    case Nil => None
    case TypeConstraint.Equality(tpe1, tpe2, prov) :: _ => Some(toTypeError(UnificationError.MismatchedTypes(tpe1, tpe2), prov))
    case TypeConstraint.Trait(sym, tpe, loc) :: _ => Some(mkMissingInstance(sym, tpe, renv, loc))
    case TypeConstraint.Purification(_, _, _, _, nested, _, _) :: _ => getFirstError(nested, renv)
  }

  /**
    * Constructs a specific missing instance error for the given trait symbol `sym` and type `tpe`.
    */
  def mkMissingInstance(sym: Symbol.TraitSym, tpe: Type, renv: RigidityEnv, loc: SourceLocation)(implicit flix: Flix): TypeError = {
    val eqSym = Symbol.mkTraitSym("Eq")
    val orderSym = Symbol.mkTraitSym("Order")
    val sendableSym = Symbol.mkTraitSym("Sendable")
    val toStringSym = Symbol.mkTraitSym("ToString")
    if (sym == eqSym) {
      TypeError.MissingInstanceEq(tpe, renv, loc)
    } else if (sym == orderSym) {
      TypeError.MissingInstanceOrder(tpe, renv, loc)
    } else if (sym == sendableSym) {
      TypeError.MissingInstanceSendable(tpe, renv, loc)
    } else if (sym == toStringSym) {
      TypeError.MissingInstanceToString(tpe, renv, loc)
    } else {
      tpe.typeConstructor match {
        case Some(TypeConstructor.Arrow(_)) => TypeError.MissingInstanceArrow(sym, tpe, renv, loc)
        case _ => TypeError.MissingInstance(sym, tpe, renv, loc)
      }
    }
  }

  /**
    * Gets the list of type constraints implied by this type constraint due to a supertrait relationship,
    * including the type constraint itself.
    *
    * For example, `Order[a]` implies `Order[a]` and `Eq[a]`
    */
  def withSupers(tconstr: Ast.TypeConstraint, tenv: Map[Symbol.TraitSym, Ast.TraitContext]): List[Ast.TypeConstraint] = {
    val superSyms = tenv(tconstr.head.sym).superTraits
    val directSupers = superSyms.map {
      case sym => Ast.TypeConstraint(Ast.TypeConstraint.Head(sym, SourceLocation.Unknown), tconstr.arg, tconstr.loc)
    }
    val allSupers = directSupers.flatMap(withSupers(_, tenv))
    tconstr :: allSupers
  }

  /**
    * Opens schema types `#{A(Int32) | {}}` becomes `#{A(Int32) | r}` with a fresh
    * `r`. This only happens for if the row type is the topmost type, i.e. this
    * doesn't happen inside tuples or other such nesting.
    */
  private def openOuterSchema(tpe: Type)(implicit flix: Flix): Type = {
    @tailrec
    def transformRow(tpe: Type, acc: Type => Type): Type = tpe match {
      case Type.Cst(TypeConstructor.SchemaRowEmpty, loc) =>
        acc(Type.freshVar(TypeConstructor.SchemaRowEmpty.kind, loc))
      case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.SchemaRowExtend(pred), loc1), tpe1, loc2), rest, loc3) =>
        transformRow(rest, inner =>
          // copy into acc, just replacing `rest` with `inner`
          acc(Type.Apply(Type.Apply(Type.Cst(TypeConstructor.SchemaRowExtend(pred), loc1), tpe1, loc2), inner, loc3))
        )
      case other => acc(other)
    }

    tpe match {
      case Type.Apply(Type.Cst(TypeConstructor.Schema, loc1), row, loc2) =>
        Type.Apply(Type.Cst(TypeConstructor.Schema, loc1), transformRow(row, x => x), loc2)
      case other => other
    }
  }

  /**
    * Converts the given unification error into a type error.
    *
    * ExpectType
    * - pretend it's just unifyType
    * - if mismatchedtypes then map the error to PossibleChecked or UnexpectedType
    * - else return as is
    *
    * ExpectEffect
    * - pretend it's just unifyType
    * - if mismatchedEffects then map the error to possiblechecke or unexpectedeffect
    * - else return as is
    *
    * ExpectTypeArguments
    * - pretend it's just unifytype
    * - if mismatchedbools or mismatchedarroweffects or mismatchedtypes then map the error to unexpectedarg
    * - else return as is
    *
    * Match
    * - mismatched types
    *   - check for over/under applied
    *   - else return as is
    *     - mismatched bools -> mismatched bools
    *     - mismatched effects
    *   - check for mismatched arrow effects
    *   - else return as is
    *     - mismatched case sets -> mismatched case sets
    *     - mismatched arity -> mismatched arity
    *     - rigid var -> mismatched types
    *     - occurs check -> occurs check
    *     - undefined label -> undefined label
    *     - non-record type -> non-record type
    *     - undefined predicate -> undefined predicate
    *     - non-schema type -> non-schema type
    *     - no matching instance
    *   - check for specific instance
    *     - toString
    *     - eq
    *     - ord
    *     - hash
    *     - ?
    *       - (other cases should be impossible on this branch)
    */
  // TODO ASSOC-TYPES This translation does not work well
  // TODO ASSOC-TYPES because provenance is not propogated properly.
  // TODO ASSOC-TYPES We also need to track the renv for use in these errors.
  private def toTypeError(err0: UnificationError, prov: Provenance)(implicit flix: Flix): TypeError = (err0, prov) match {
    case (err, Provenance.ExpectType(expected, actual, loc)) =>
      toTypeError(err, Provenance.Match(expected, actual, loc)) match {
        case TypeError.MismatchedTypes(baseType1, baseType2, fullType1, fullType2, renv, _) =>
          (baseType1.typeConstructor, baseType2.typeConstructor) match {
            case (Some(TypeConstructor.Native(left)), Some(TypeConstructor.Native(right))) if left.isAssignableFrom(right) =>
              TypeError.PossibleCheckedTypeCast(expected, actual, renv, loc)
            case _ =>
              TypeError.UnexpectedType(baseType1, baseType2, renv, loc)
          }
        case e => e
      }

    case (err, Provenance.ExpectEffect(expected, actual, loc)) =>
      toTypeError(err, Provenance.Match(expected, actual, loc)) match {
        case TypeError.MismatchedEffects(baseType1, baseType2, fullType1, fullType2, renv, _) =>
          // TODO ASSOC-TYPES restore possible upcast error
          TypeError.UnexpectedEffect(baseType1, baseType2, renv, loc)
        case e => e
      }

    case (err, Provenance.ExpectArgument(expected, actual, sym, num, loc)) =>
      toTypeError(err, Provenance.Match(expected, actual, loc)) match {
        case TypeError.MismatchedBools(_, _, fullType1, fullType2, renv, loc) =>
          TypeError.UnexpectedArg(sym, num, fullType1, fullType2, renv, loc)

        case TypeError.MismatchedArrowEffects(_, _, fullType1, fullType2, renv, loc) =>
          TypeError.UnexpectedArg(sym, num, fullType1, fullType2, renv, loc)

        case TypeError.MismatchedTypes(_, _, fullType1, fullType2, renv, loc) =>
          TypeError.UnexpectedArg(sym, num, fullType1, fullType2, renv, loc)
        case e => e
      }

    case (UnificationError.MismatchedTypes(baseType1, baseType2), Provenance.Match(type1, type2, loc)) =>
      (baseType1.typeConstructor, baseType2.typeConstructor) match {
        case (Some(TypeConstructor.Arrow(_)), _) => getUnderOrOverAppliedError(baseType1, baseType2, type1, type2, RigidityEnv.empty, loc)
        case (_, Some(TypeConstructor.Arrow(_))) => getUnderOrOverAppliedError(baseType2, baseType1, type2, type1, RigidityEnv.empty, loc)
        case _ => TypeError.MismatchedTypes(baseType1, baseType2, type1, type2, RigidityEnv.empty, loc)
      }

    case (UnificationError.MismatchedBools(baseType1, baseType2), Provenance.Match(type1, type2, loc)) =>
      TypeError.MismatchedBools(baseType1, baseType2, type1, type2, RigidityEnv.empty, loc)

    case (UnificationError.MismatchedEffects(baseType1, baseType2), Provenance.Match(type1, type2, loc)) =>
      (type1.typeConstructor, type2.typeConstructor) match {
        case (Some(TypeConstructor.Arrow(_)), _) => TypeError.MismatchedArrowEffects(baseType1, baseType2, type1, type2, RigidityEnv.empty, loc)
        case (_, Some(TypeConstructor.Arrow(_))) => TypeError.MismatchedArrowEffects(baseType1, baseType2, type1, type2, RigidityEnv.empty, loc)
        case _ => TypeError.MismatchedEffects(baseType1, baseType2, type1, type2, RigidityEnv.empty, loc)
      }

    case (UnificationError.MismatchedCaseSets(baseType1, baseType2), Provenance.Match(type1, type2, loc)) =>
      TypeError.MismatchedCaseSets(baseType1, baseType2, type1, type2, RigidityEnv.empty, loc)

    case (UnificationError.MismatchedArity(ts1, ts2), Provenance.Match(tpe1, tpe2, loc)) =>
      TypeError.MismatchedArity(tpe1, tpe2, RigidityEnv.empty, loc)

    case (UnificationError.TooComplex(tpe1, tpe2), Provenance.Match(_, _, loc)) =>
      TypeError.TooComplex(loc)

    case (UnificationError.RigidVar(baseType1, baseType2), Provenance.Match(type1, type2, loc)) =>
      TypeError.MismatchedTypes(baseType1, baseType2, type1, type2, RigidityEnv.empty, loc)
    case (UnificationError.OccursCheck(baseType1, baseType2), Provenance.Match(type1, type2, loc)) =>
      TypeError.OccursCheck(baseType1, baseType2, type1, type2, RigidityEnv.empty, loc)
    case (UnificationError.UndefinedLabel(label, labelType, recordType), Provenance.Match(type1, type2, loc)) =>
      TypeError.UndefinedLabel(label, labelType, recordType, RigidityEnv.empty, loc)
    case (UnificationError.UndefinedPredicate(pred, predType, schemaType), Provenance.Match(type1, type2, loc)) =>
      TypeError.UndefinedPred(pred, predType, schemaType, RigidityEnv.empty, loc)
    case (UnificationError.NonRecordType(nonRecordType), Provenance.Match(type1, type2, loc)) =>
      TypeError.NonRecordType(nonRecordType, RigidityEnv.empty, loc)
    case (UnificationError.NonSchemaType(nonSchemaType), Provenance.Match(type1, type2, loc)) =>
      TypeError.NonSchemaType(nonSchemaType, RigidityEnv.empty, loc)
    case (UnificationError.NoMatchingInstance(tconstr), Provenance.Match(type1, type2, loc)) =>
      mkMissingInstance(tconstr.head.sym, tconstr.arg, RigidityEnv.empty, loc)

    // TODO ASSOC-TYPES these errors are relics of the old type system and should be removed
    case (UnificationError.UnsupportedEquality(t1, t2), _) => throw InternalCompilerException("unexpected error: " + err0, SourceLocation.Unknown)
    case (UnificationError.IrreducibleAssocType(sym, t), _) => throw InternalCompilerException("unexpected error: " + err0, SourceLocation.Unknown)
  }

  /**
    *
    * A result of performing type inference.
    *
    * @param constrs constraints inferred for the expression
    * @param tpe     the inferred type of the expression
    * @param eff     the inferred effect of the expression
    * @param renv    the inferred rigidity environment for the expression (marking region variables)
    */
  case class InfResult(constrs: List[TypeConstraint], tpe: Type, eff: Type, renv: RigidityEnv)

  /**
    * A result of constraint resolution.
    *
    * @param subst   a substitution
    * @param constrs leftover constraints
    */
  case class ResolutionResult(subst: Substitution, constrs: List[TypeConstraint])

  /**
    * An intermediate result of constraint resolution.
    *
    * @param subst    a substitution
    * @param constrs  leftover constraints
    * @param effs     effect constraints
    * @param progress a flag indicating whether this resolution attempt made progress
    */
  case class IntermediateResult(subst: Substitution, constrs: List[TypeConstraint], effs: List[TypeConstraint.Equality], traits: List[TypeConstraint.Trait], progress: Boolean) {

    /**
      * Composes `this` equality result with `that` equality result.
      *
      * - Composes the substitution,
      * - combines the leftover constraints, and
      * - indicates progress if one of the two made progress.
      */
    def @@(that: IntermediateResult): IntermediateResult = {
      val IntermediateResult(s1, cs1, es1, ts1, p1) = this
      val IntermediateResult(s2, cs2, es2, ts2, p2) = that
      IntermediateResult(s1 @@ s2, cs1 ++ cs2, es1 ++ es2, ts1 ++ ts2, p1 || p2)
    }
  }

  object IntermediateResult {

    /**
      * Indicates that a constraint was eliminated.
      */
    val elimination: IntermediateResult = IntermediateResult(Substitution.empty, Nil, Nil, Nil, progress = true)

    /**
      * Indicates that a constraint was resolved to a substitution.
      */
    def newSubst(subst: Substitution): IntermediateResult = IntermediateResult(subst, Nil, Nil, Nil, progress = true)

    /**
      * Indicates that a constraint was resolved to a set of constraints.
      *
      * This may be the same list of constraints as was given to the solver.
      * In this case, `progress` is `false`.
      */
    def constraints(constrs: List[TypeConstraint], progress: Boolean): IntermediateResult = IntermediateResult(Substitution.empty, constrs, Nil, Nil, progress)

    /**
      * Indicates that an effect constraint was moved to the effect constraints list.
      */
    def effConstraint(constr: TypeConstraint.Equality): IntermediateResult = IntermediateResult(Substitution.empty, Nil, List(constr), Nil, progress = true)

    /**
      * Indicates that trait constraint was moved to the effect constraints list.
      */
    def traitConstraint(constr: TypeConstraint.Trait): IntermediateResult = IntermediateResult(Substitution.empty, Nil, Nil, List(constr), progress = true)
  }

}
