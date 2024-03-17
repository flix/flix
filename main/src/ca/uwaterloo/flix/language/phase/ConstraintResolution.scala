/*
 * Copyright 2023 Matthew Lutze
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
package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{Ast, Kind, KindedAst, Level, RigidityEnv, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.errors.TypeError
import ca.uwaterloo.flix.language.phase.constraintgeneration.TypingConstraint.Provenance
import ca.uwaterloo.flix.language.phase.constraintgeneration.{Debug, TypingConstraint}
import ca.uwaterloo.flix.language.phase.unification.Unification.getUnderOrOverAppliedError
import ca.uwaterloo.flix.language.phase.unification._
import ca.uwaterloo.flix.util.Result.Err
import ca.uwaterloo.flix.util.collection.{ListMap, ListOps}
import ca.uwaterloo.flix.util.{InternalCompilerException, Result, Validation}

import java.nio.file.{Files, Path}
import scala.annotation.tailrec

object ConstraintResolution {

  /**
    * The maximum number of resolution iterations before we throw an error.
    */
  private val MaxIterations = 1000

  private var record = false

  private def startLogging(): Unit = record = true

  private def stopLogging(): Unit = record = false

  private def recordGraph(s: => String, number: Int): Unit = {
    if (record) {
      val path = Path.of(s"./personal/constraint-graphs/${number.toString.reverse.padTo(4, '0').reverse}.dot")
      Files.writeString(path, s)
    }
  }

  /**
    * Resolves constraints in the given definition using the given inference result.
    */
  def visitDef(defn: KindedAst.Def, infResult: InfResult, renv0: RigidityEnv, tconstrs0: List[Ast.TypeConstraint], cenv0: Map[Symbol.ClassSym, Ast.ClassContext], eqEnv0: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef], root: KindedAst.Root)(implicit flix: Flix): Validation[Substitution, TypeError] = defn match {
    case KindedAst.Def(_, spec, _) => visitSpec(spec, infResult, renv0, tconstrs0, cenv0, eqEnv0, root)
  }

  /**
    * Resolves constraints in the given signature using the given inference result.
    */
  def visitSig(sig: KindedAst.Sig, infResult: InfResult, renv0: RigidityEnv, tconstrs0: List[Ast.TypeConstraint], cenv0: Map[Symbol.ClassSym, Ast.ClassContext], eqEnv0: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef], root: KindedAst.Root)(implicit flix: Flix): Validation[Substitution, TypeError] = sig match {
    case KindedAst.Sig(_, _, None) => Validation.success(Substitution.empty)
    case KindedAst.Sig(_, spec, Some(_)) => visitSpec(spec, infResult, renv0, tconstrs0, cenv0, eqEnv0, root)
  }

  /**
    * Resolves constraints in the given spec using the given inference result.
    */
  def visitSpec(spec: KindedAst.Spec, infResult: InfResult, renv0: RigidityEnv, tconstrs0: List[Ast.TypeConstraint], cenv0: Map[Symbol.ClassSym, Ast.ClassContext], eqEnv0: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef], root: KindedAst.Root)(implicit flix: Flix): Validation[Substitution, TypeError] = spec match {
    case KindedAst.Spec(_, _, _, _, fparams, _, tpe, eff, tconstrs, econstrs, loc) =>

      val InfResult(infConstrs, infTpe, infEff, infRenv) = infResult

      val initialSubst = fparams.foldLeft(Substitution.empty) {
        case (acc, KindedAst.FormalParam(sym, mod, tpe, src, loc)) => acc ++ Substitution.singleton(sym.tvar.sym, openOuterSchema(tpe)(Level.Top, flix))
      }

      // Wildcard tparams are not counted in the tparams, so we need to traverse the types to get them.
      val allTparams = tpe.typeVars ++ eff.typeVars ++ fparams.flatMap(_.tpe.typeVars) ++ econstrs.flatMap(_.tpe2.typeVars)

      val renv = allTparams.foldLeft(infRenv ++ renv0) {
        case (acc, Type.Var(sym, _)) => acc.markRigid(sym)
      }

      val cenv = expandClassEnv(cenv0, tconstrs ++ tconstrs0)
      val eqEnv = expandEqualityEnv(eqEnv0, econstrs) // TODO ASSOC-TYPES allow econstrs on instances

      val tpeConstr = TypingConstraint.Equality(tpe, infTpe, Provenance.ExpectType(expected = tpe, actual = infTpe, loc))
      val effConstr = TypingConstraint.Equality(eff, infEff, Provenance.ExpectEffect(expected = eff, actual = infEff, loc))
      val constrs = tpeConstr :: effConstr :: infConstrs
      resolve(constrs, initialSubst, renv, cenv, eqEnv).flatMap {
        case ResolutionResult(subst, deferred, _) =>
          stopLogging()
          // TODO here we only consider the first error
          deferred match {
            case Nil => Result.Ok(subst)
            case TypingConstraint.Equality(tpe1, tpe2, prov) :: _ => Err(toTypeError(UnificationError.MismatchedTypes(tpe1, tpe2), prov))
            case TypingConstraint.Class(sym, tpe, loc) :: _ => Err(TypeError.MissingInstance(sym, tpe, renv, loc))
            case TypingConstraint.Purification(_, _, _, _, _, _) :: _ => throw InternalCompilerException("unexpected purificaiton error", loc)
          }
      }.toValidation
  }

  /**
    * Adds the given type constraints as assumptions to the class environment.
    */
  private def expandClassEnv(cenv: Map[Symbol.ClassSym, Ast.ClassContext], tconstrs: List[Ast.TypeConstraint]): Map[Symbol.ClassSym, Ast.ClassContext] = {

    tconstrs.flatMap(withSupers(_, cenv)).foldLeft(cenv) {
      case (acc, Ast.TypeConstraint(Ast.TypeConstraint.Head(sym, _), arg, loc)) =>
        val inst = Ast.Instance(arg, Nil)
        val context = acc.get(sym) match {
          case Some(Ast.ClassContext(supers, insts)) => Ast.ClassContext(supers, inst :: insts)
          case None => throw InternalCompilerException(s"unexpected unknown class sym: $sym", loc)
        }
        acc + (sym -> context)
    }
  }

  /**
    * Gets the list of type constraints implied by this type constraint due to a superclass relationship,
    * including the type constraint itself.
    *
    * For example, `Order[a]` implies `Order[a]` and `Eq[a]`
    */
  private def withSupers(tconstr: Ast.TypeConstraint, cenv: Map[Symbol.ClassSym, Ast.ClassContext]): List[Ast.TypeConstraint] = {
    val superSyms = cenv(tconstr.head.sym).superClasses
    val directSupers = superSyms.map {
      case sym => Ast.TypeConstraint(Ast.TypeConstraint.Head(sym, SourceLocation.Unknown), tconstr.arg, tconstr.loc)
    }
    val allSupers = directSupers.flatMap(withSupers(_, cenv))
    tconstr :: allSupers
  }

  /**
    * Adds the given equality constraints as assumptions to the equality environment.
    */
  private def expandEqualityEnv(eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef], econstrs: List[Ast.EqualityConstraint]): ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef] = {
    econstrs.foldLeft(eqEnv) {
      case (acc, Ast.EqualityConstraint(Ast.AssocTypeConstructor(sym, _), tpe1, tpe2, _)) =>
        val assoc = Ast.AssocTypeDef(tpe1, tpe2)
        acc + (sym -> assoc)
    }
  }

  /**
    * Resolves the equality between the two given types.
    *
    * θ ⊩ᵤ τ₁ = τ₂ ⤳ u; r
    */
  private def resolveEquality(tpe1: Type, tpe2: Type, prov: Provenance, renv: RigidityEnv, eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef], loc: SourceLocation)(implicit flix: Flix): Result[ResolutionResult, TypeError] = (tpe1.kind, tpe2.kind) match {
    case (Kind.Eff, Kind.Eff) =>
      // first simplify the types to get rid of assocs if we can
      for {
        (t1, p1) <- simplifyType(tpe1, renv, eqEnv, loc)
        (t2, p2) <- simplifyType(tpe2, renv, eqEnv, loc)
        res0 <- EffUnification.unify(t1, t2, renv).mapErr(toTypeError(_, prov))
        // If eff unification has any constrs in output, then we know it failed so the subst is empty
        res =
          if (res0._2.isEmpty) {
            ResolutionResult.newSubst(res0._1)
          } else {
            ResolutionResult.constraints(List(TypingConstraint.Equality(t1, t2, prov)), p1 || p2)
          }
      } yield res

    case (Kind.Bool, Kind.Bool) =>
      // first simplify the types to get rid of assocs if we can
      for {
        (t1, p1) <- simplifyType(tpe1, renv, eqEnv, loc)
        (t2, p2) <- simplifyType(tpe2, renv, eqEnv, loc)
        res0 <- BoolUnification.unify(t1, t2, renv).mapErr(toTypeError(_, prov))
        // If bool unification has any constrs in output, then we know it failed so the subst is empty
        res =
          if (res0._2.isEmpty) {
            ResolutionResult.newSubst(res0._1)
          } else {
            ResolutionResult.constraints(List(TypingConstraint.Equality(t1, t2, prov)), p1 || p2)
          }
      } yield res


    case (Kind.RecordRow, Kind.RecordRow) =>
      // first simplify the types to get rid of assocs if we can
      for {
        (t1, _) <- simplifyType(tpe1, renv, eqEnv, loc)
        (t2, _) <- simplifyType(tpe2, renv, eqEnv, loc)
        res0 <- RecordUnification.unifyRows(t1, t2, renv).mapErr(toTypeError(_, prov))
        // If eff unification has any constrs in output, then we know it failed so the subst is empty
        res = if (res0._2.isEmpty) ResolutionResult.newSubst(res0._1) else throw InternalCompilerException("can't handle complex record stuff", SourceLocation.Unknown)
      } yield res

    case (Kind.SchemaRow, Kind.SchemaRow) =>
      // first simplify the types to get rid of assocs if we can
      for {
        (t1, _) <- simplifyType(tpe1, renv, eqEnv, loc)
        (t2, _) <- simplifyType(tpe2, renv, eqEnv, loc)
        res <- SchemaUnification.unifyRows(t1, t2, renv).mapErr(toTypeError(_, prov))
      } yield ResolutionResult.newSubst(res)

    case (Kind.CaseSet(sym1), Kind.CaseSet(sym2)) if sym1 == sym2 =>
      for {
        (t1, _) <- simplifyType(tpe1, renv, eqEnv, loc)
        (t2, _) <- simplifyType(tpe2, renv, eqEnv, loc)
        res <- CaseSetUnification.unify(t1, t2, renv, sym1.universe, sym1).mapErr(toTypeError(_, prov))
      } yield ResolutionResult.newSubst(res)

    case (k1, k2) if KindUnification.unifiesWith(k1, k2) => resolveEqualityStar(tpe1, tpe2, prov, renv, eqEnv, loc)

    case _ => Err(toTypeError(UnificationError.MismatchedTypes(tpe1, tpe2), prov))
  }


  /**
    * Resolves the equality between the two given types.
    *
    * The types must have kinds that do not have special unification rules.
    *
    * θ ⊩ᵤ τ₁ = τ₂ ⤳ u; r
    */
  private def resolveEqualityStar(tpe1: Type, tpe2: Type, prov: Provenance, renv: RigidityEnv, eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef], loc: SourceLocation)(implicit flix: Flix): Result[ResolutionResult, TypeError] = (tpe1, tpe2) match {
    // varU
    case (x: Type.Var, t) if renv.isFlexible(x.sym) && !t.typeVars.contains(x) =>
      Result.Ok(ResolutionResult.newSubst(Substitution.singleton(x.sym, t)))

    // varU
    case (t, x: Type.Var) if renv.isFlexible(x.sym) && t.typeVars.contains(x) =>
      Result.Ok(ResolutionResult.newSubst(Substitution.singleton(x.sym, t)))


    // reflU
    case (Type.Cst(c1, _), Type.Cst(c2, _)) if c1 == c2 => Result.Ok(ResolutionResult.elimination)
    case (x: Type.Var, y: Type.Var) if (x == y) => Result.Ok(ResolutionResult.elimination)

    case (Type.Alias(_, _, tpe, _), _) => resolveEquality(tpe, tpe2, prov, renv, eqEnv, loc)

    case (_, Type.Alias(_, _, tpe, _)) => resolveEquality(tpe1, tpe, prov, renv, eqEnv, loc)

    // appU
    case (Type.Apply(t11, t12, _), Type.Apply(t21, t22, _)) =>
      for {
        res1 <- resolveEquality(t11, t21, prov, renv, eqEnv, loc)
        res2 <- resolveEquality(res1.subst(t12), res1.subst(t22), prov, renv, eqEnv, loc)
      } yield res2 @@ res1

    // reflU
    case (Type.AssocType(cst1, args1, _, _), Type.AssocType(cst2, args2, _, _)) if cst1.sym == cst2.sym && args1 == args2 =>
      Result.Ok(ResolutionResult.elimination)

    // redU
    case (assoc: Type.AssocType, t2) =>
      for {
        (t1, progress) <- simplifyType(assoc, renv, eqEnv, loc)
      } yield {
        ResolutionResult.constraints(List(TypingConstraint.Equality(t1, t2, prov)), progress)
      }

    case (t1, assoc: Type.AssocType) =>
      for {
        (t2, progress) <- simplifyType(assoc, renv, eqEnv, loc)
      } yield {
        ResolutionResult.constraints(List(TypingConstraint.Equality(t1, t2, prov)), progress)
      }

    case _ =>
      Result.Err(toTypeError(UnificationError.MismatchedTypes(tpe1, tpe2), prov))
  }

  /**
    * Simplifies the given type by reducing associated type applications.
    *
    * Θ ⊩ τ ⤳ τ'
    *
    * Returns the simplified type and a Boolean flag to indicate whether progress was made.
    *
    * Applications that cannot be resolved are left as they are.
    * These are applications to variables and applications to other unresolvable types.
    *
    * Applications that are illegal result in an Err.
    * These are applications to types for which the eqEnv has no corresponding instance.
    */
  private def simplifyType(tpe: Type, renv0: RigidityEnv, eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef], loc: SourceLocation)(implicit flix: Flix): Result[(Type, Boolean), TypeError] = tpe match {
    // A var is already simple.
    case t: Type.Var => Result.Ok((t, false))
    // A constant is already simple
    case t: Type.Cst => Result.Ok((t, false))
    // lapp_L and lapp_R
    case Type.Apply(tpe1, tpe2, loc) =>
      for {
        (t1, p1) <- simplifyType(tpe1, renv0, eqEnv, loc)
        (t2, p2) <- simplifyType(tpe2, renv0, eqEnv, loc)
      } yield {
        (Type.Apply(t1, t2, loc), p1 || p2)
      }
    // arg_R and syn_R
    case Type.AssocType(cst, arg, kind, _) =>
      simplifyType(arg, renv0, eqEnv, loc).flatMap {
        case (t, p) =>
          // we mark t's tvars as rigid so we get the substitution in the right direction
          val renv = t.typeVars.map(_.sym).foldLeft(RigidityEnv.empty)(_.markRigid(_))
          val insts = eqEnv(cst.sym)

          // find the first (and only) instance that matches
          val simplifiedOpt = ListOps.findMap(insts) {
            inst =>
              Unification.unifyTypes(t, inst.arg, renv).toOption.flatMap {
                case (subst, Nil) => Some(subst(inst.ret))
                case (_, _ :: _) => None // if we have leftover constraints then it didn't actually unify
              }
          }
          simplifiedOpt match {
            // Can't reduce. Check what the original type was.
            case None =>
              t.baseType match {
                // If it's a var, it's ok. It may be substituted later to a type we can reduce.
                // Or it might be part of the signature as an associated type.
                case Type.Var(sym, loc) => Result.Ok((Type.AssocType(cst, t, kind, loc), p))
                // If it's an associated type, it's ok. It may be reduced later to a concrete type.
                case _: Type.AssocType => Result.Ok((Type.AssocType(cst, t, kind, loc), p))
                // Otherwise it's a problem.
                case baseTpe => Result.Err(TypeError.MissingInstance(cst.sym.clazz, baseTpe, renv, loc))
              }
            // We could reduce! Simplify further if possible.
            case Some(t) => simplifyType(t, renv0, eqEnv, loc).map { case (res, _) => (res, true) }
          }
      }
    case Type.Alias(cst, args, t, _) => simplifyType(t, renv0, eqEnv, loc)
  }

  /**
    * Resolves the given class constraint.
    *
    * Θ ⊩ₑ π ⤳ P
    *
    * Returns a list of resulting constraints and a Boolean flag to indicate whether progress was made.
    *
    * Constraints that cannot be resolved are left as they are.
    * These are applications to variables and applications to other unresolvable types.
    *
    * Constraints that are illegal result in an Err.
    * These are applications to types for which the classEnv has no corresponding instance.
    */
  private def resolveClassConstraint(clazz: Symbol.ClassSym, tpe0: Type, classEnv: Map[Symbol.ClassSym, Ast.ClassContext], eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef], renv0: RigidityEnv, loc: SourceLocation)(implicit flix: Flix): Result[(List[TypingConstraint], Boolean), TypeError] = {
    // redE
    simplifyType(tpe0, renv0, eqEnv, loc).flatMap {
      case (t, progress) =>
        // Look at the head of the type.
        t.baseType match {
          // Case 1: Flexible var. It might be resolved later.
          case Type.Var(sym, _) if renv0.isFlexible(sym) =>
            Result.Ok((List(TypingConstraint.Class(clazz, t, loc)), progress))
          // Case 2: Assoc type. It might be resolved later.
          case _: Type.AssocType =>
            Result.Ok((List(TypingConstraint.Class(clazz, t, loc)), progress))
          // Case 3: Something rigid (const or rigid var). We can look this up immediately.
          case _ =>
            // we mark t's tvars as rigid so we get the substitution in the right direction
            val renv = t.typeVars.map(_.sym).foldLeft(renv0)(_.markRigid(_))
            val insts = classEnv(clazz).instances
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
                  case Type.Var(sym, loc) => Result.Ok(List(TypingConstraint.Class(clazz, t, loc)), progress)
                  // If it's an associated type, it's ok. It may be reduced later to a concrete type.
                  case _: Type.AssocType => Result.Ok(List(TypingConstraint.Class(clazz, t, loc)), progress)
                  // Otherwise it's a problem.
                  case _ => Result.Err(TypeError.MissingInstance(clazz, tpe0, renv, loc))
                }
              case Some(tconstrs) =>
                // simplify all the implied constraints
                Result.traverse(tconstrs) {
                  case Ast.TypeConstraint(Ast.TypeConstraint.Head(c, _), arg, _) =>
                    resolveClassConstraint(c, arg, classEnv, eqEnv, renv0, loc)
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
    * Attempts to resolve the given type constraints under the given environments,
    * and with the given initial substitution.
    *
    * The initial substitution should come from e.g., formal parameter type ascriptions.
    *
    * Returns a reduction result containing a substitution and any leftover constraints.
    */
  private def resolve(constrs: List[TypingConstraint], subst0: Substitution, renv: RigidityEnv, cenv: Map[Symbol.ClassSym, Ast.ClassContext], eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit flix: Flix): Result[ResolutionResult, TypeError] = {
    var curr = constrs.sortBy(_.numVars)
    var subst = subst0
    var count = 0
    var prog = true
    while (prog) {
      if (count >= MaxIterations) {
        return Result.Err(TypeError.TooComplex(SourceLocation.Unknown))
      }

      count += 1
      recordGraph(Debug.toDotWithSubst(curr, subst), count)

      resolveOneOf(curr, renv, cenv, eqEnv, subst) match {
        case Result.Ok(ResolutionResult(newSubst, newConstrs, progress)) =>
          curr = newConstrs
          subst = newSubst
          prog = progress
        case res@Result.Err(_) =>
          stopLogging()
          return res
      }
    }
    Result.Ok(ResolutionResult(subst, curr, progress = true))
  }

  /**
    * Tries to resolve one of the given constraints.
    *
    * Applies the initial substitution `subst0` before attempting to resolve.
    */
  private def resolveOneOf(constrs: List[TypingConstraint], subst0: Substitution, renv: RigidityEnv, cenv: Map[Symbol.ClassSym, Ast.ClassContext], eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit flix: Flix): Result[ResolutionResult, TypeError] = {
    def tryResolve(cs: List[TypingConstraint]): Result[ResolutionResult, TypeError] = cs match {
      case Nil => Result.Ok(ResolutionResult(subst0, cs, progress = false))
      case hd :: tl => resolveOne(hd, renv, cenv, eqEnv, subst0).flatMap {
        // if we're just returning the same constraint, then have made no progress and we need to find something else to reduce
        case res if !res.progress => tryResolve(tl).map {
          case res => res.copy(constrs = hd :: res.constrs) // MATT testing
//          // TODO ASSOC-TYPES does this make a difference since we sort anyway?
//          // Case 1: progress made. send the head to the end
//          case res if res.progress => res.copy(constrs = res.constrs :+ hd)
//          // Case 2: no progress. Keep the order
//          case res => res.copy(constrs = hd :: res.constrs)
        }
        // otherwise we have made progress so we're happy
        case res => Result.Ok(res.copy(constrs = tl ::: res.constrs))
      }
    }

    tryResolve(constrs.sortBy(_.index))
  }

  /**
    * Tries to resolve the given constraint.
    */
  private def resolveOne(constr0: TypingConstraint, renv: RigidityEnv, cenv: Map[Symbol.ClassSym, Ast.ClassContext], eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef], subst0: Substitution)(implicit flix: Flix): Result[ResolutionResult, TypeError] = constr0 match {
    case TypingConstraint.Equality(tpe1, tpe2, prov) =>
      val t1 = TypeMinimization.minimizeType(subst0(tpe1))
      val t2 = TypeMinimization.minimizeType(subst0(tpe2))
      resolveEquality(t1, t2, prov, renv, eqEnv, constr0.loc).map {
        case ResolutionResult(subst, constrs, p) => ResolutionResult(subst @@ subst0, constrs, progress = p)
      }
    case TypingConstraint.Class(sym, tpe, loc) =>
      resolveClassConstraint(sym, subst0(tpe), cenv, eqEnv, renv, loc).map {
        case (constrs, progress) => ResolutionResult(subst0, constrs, progress)
      }
    case TypingConstraint.Purification(sym, eff1, eff2, level, prov, nested0) =>
      // First reduce nested constraints
      resolveOneOf(nested0, renv, cenv, eqEnv, subst0).map {
        // Case 1: We have reduced everything below. Now reduce the purity constraint.
        case ResolutionResult(subst1, newConstrs, progress) if newConstrs.isEmpty =>
          val e1 = subst1(eff1)
          // purify the inner type
          val e2Raw = subst1(eff2)
          val e2 = Substitution.singleton(sym, Type.Pure)(e2Raw)
          val qvars = e2Raw.typeVars.map(_.sym).filter(_.level >= level)
          val subst = qvars.foldLeft(subst1)(_.unbind(_))
          val constr = TypingConstraint.Equality(e1, TypeMinimization.minimizeType(e2), prov)
          ResolutionResult(subst, List(constr), progress = true)
        // Case 2: Constraints remain below. Maintain the purity constraint.
        case ResolutionResult(subst, newConstrs, progress) =>
          val constr = TypingConstraint.Purification(sym, eff1, eff2, level, prov, newConstrs)
          ResolutionResult(subst, List(constr), progress)
      }
  }

  /**
    * Opens schema types `#{A(Int32) | {}}` becomes `#{A(Int32) | r}` with a fresh
    * `r`. This only happens for if the row type is the topmost type, i.e. this
    * doesn't happen inside tuples or other such nesting.
    */
  private def openOuterSchema(tpe: Type)(implicit level: Level, flix: Flix): Type = {
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
    * A result of performing type inference.
    */
  case class InfResult(constrs: List[TypingConstraint], tpe: Type, eff: Type, renv: RigidityEnv)

  /**
    * The result of constraint resolution.
    *
    * Consists of:
    * - a substitution,
    * - a list of leftover constraints, and
    * - a Boolean flag indicating whether progress was made
    */
  private case class ResolutionResult(subst: Substitution, constrs: List[TypingConstraint], progress: Boolean) {

    /**
      * Composes `this` equality result with `that` equality result.
      *
      * - Composes the substitution,
      * - combines the leftover constraints, and
      * - indicates progress if one of the two made progress.
      */
    def @@(that: ResolutionResult): ResolutionResult = {
      val ResolutionResult(s1, cs1, p1) = this
      val ResolutionResult(s2, cs2, p2) = that
      ResolutionResult(s1 @@ s2, cs1 ++ cs2, p1 || p2)
    }
  }

  private object ResolutionResult {

    /**
      * Indicates that a constraint was eliminated.
      */
    val elimination: ResolutionResult = ResolutionResult(Substitution.empty, Nil, progress = true)

    /**
      * Indicates that a constraint was resolved to a substitution.
      */
    def newSubst(subst: Substitution): ResolutionResult = ResolutionResult(subst, Nil, progress = true)

    /**
      * Indicates that a constraint was resolved to a set of constraints.
      *
      * This may be the same list of constraints as was given to the solver.
      * In this case, `progress` is `false`.
      */
    def constraints(constrs: List[TypingConstraint], progress: Boolean): ResolutionResult = ResolutionResult(Substitution.empty, constrs, progress)
  }

  /**
    * Converts the given unification error into a type error.
    *
    * Emulating logic from master branch
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
      TypeError.MissingInstance(tconstr.head.sym, tconstr.arg, RigidityEnv.empty, loc)

    // TODO ASSOC-TYPES these errors are relics of the old type system and should be removed
    case (UnificationError.UnsupportedEquality(t1, t2), _) => throw InternalCompilerException("unexpected error: " + err0, SourceLocation.Unknown)
    case (UnificationError.IrreducibleAssocType(sym, t), _) => throw InternalCompilerException("unexpected error: " + err0, SourceLocation.Unknown)
  }

}
