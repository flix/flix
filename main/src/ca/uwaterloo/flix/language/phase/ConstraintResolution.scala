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
import ca.uwaterloo.flix.language.ast.{Ast, Kind, KindedAst, RigidityEnv, SourceLocation, Symbol, Type}
import ca.uwaterloo.flix.language.errors.TypeError
import ca.uwaterloo.flix.language.errors.TypeError.HackError
import ca.uwaterloo.flix.language.phase.constraintgeneration.TypingConstraint
import ca.uwaterloo.flix.language.phase.constraintgeneration.TypingConstraint.Provenance
import ca.uwaterloo.flix.language.phase.unification._
import ca.uwaterloo.flix.util.collection.ListMap
import ca.uwaterloo.flix.util.{InternalCompilerException, Result, Validation}

import java.nio.file.{Files, Path}

object ConstraintResolution {

  private val MaxIterations = 1000

  private var record = false

  private def startLogging(): Unit = record = true

  private def stopLogging(): Unit = record = false

  private def log(s: => Any): Unit = {
    if (record) {
      println(s)
    }
  }

  private def recordGraph(s: String, number: Int): Unit = {
    if (record) {
      val path = Path.of(s"./personal/constraint-graphs/${number.toString.reverse.padTo(4, '0').reverse}.dot")
      Files.writeString(path, s)
    }
  }

  case class InfResult(constrs: List[TypingConstraint], tpe: Type, eff: Type, renv: RigidityEnv)

  case class ReductionResult(oldSubst: Substitution, newSubst: Substitution, oldConstrs: List[TypingConstraint], newConstrs: List[TypingConstraint], progress: Boolean)

  sealed trait EqualityResult {
    def subst: Substitution = this match {
      case EqualityResult.NoProgress => Substitution.empty
      case EqualityResult.Subst(s, cs) => s
      case EqualityResult.Constrs(cs) => Substitution.empty
    }

    def constrs: List[TypingConstraint] = this match {
      case EqualityResult.NoProgress => Nil
      case EqualityResult.Subst(s, cs) => cs
      case EqualityResult.Constrs(cs) => cs
    }


    def @@(that: EqualityResult): EqualityResult = (this, that) match {
      case (res1, EqualityResult.NoProgress) => res1
      case (EqualityResult.NoProgress, res2) => res2

      case (EqualityResult.Constrs(cs1), EqualityResult.Constrs(cs2)) => EqualityResult.Constrs(cs1 ++ cs2)

      case _ => EqualityResult.Subst(this.subst @@ that.subst, this.constrs ++ that.constrs)
    }
  }

  object EqualityResult {
    case object NoProgress extends EqualityResult

    case class Subst(s: Substitution, cs: List[TypingConstraint]) extends EqualityResult

    case class Constrs(cs: List[TypingConstraint]) extends EqualityResult
  }

  // TODO ASSOC-TYPES this is duplicated in TypeReconstruction

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
      case sym => Ast.TypeConstraint(Ast.TypeConstraint.Head(sym, SourceLocation.Unknown), tconstr.arg, SourceLocation.Unknown)
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

  def visitDef(defn: KindedAst.Def, renv0: RigidityEnv, tconstrs0: List[Ast.TypeConstraint], cenv0: Map[Symbol.ClassSym, Ast.ClassContext], eqEnv0: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef], root: KindedAst.Root, infResult: InfResult)(implicit flix: Flix): Validation[Substitution, TypeError] = defn match {
    case KindedAst.Def(sym, KindedAst.Spec(doc, ann, mod, tparams, fparams, sc, tpe, eff, tconstrs, econstrs, loc), exp) =>

      val InfResult(infConstrs, infTpe, infEff, infRenv) = infResult
      if (sym.toString == "Graph.withinDistanceOf") {
//        startLogging()
      }
      log(sym)


      val initialSubst = fparams.foldLeft(Substitution.empty) {
        case (acc, KindedAst.FormalParam(sym, mod, tpe, src, loc)) => acc ++ Substitution.singleton(sym.tvar.sym, tpe)
      }

      val renv = tparams.foldLeft(infRenv ++ renv0) {
        case (acc, KindedAst.TypeParam(name, sym, loc)) => acc.markRigid(sym)
      }

      val cenv = expandClassEnv(cenv0, tconstrs ++ tconstrs0)
      val eqEnv = expandEqualityEnv(eqEnv0, econstrs) // MATT need instance eq stuff

      val tpeConstr = TypingConstraint.Equality(tpe, infTpe, Provenance.ExpectLeft, loc)
      val effConstr = TypingConstraint.Equality(eff, infEff, Provenance.ExpectLeft, loc)
      val constrs = tpeConstr :: effConstr :: infConstrs
      resolve(constrs, renv, cenv, eqEnv, initialSubst).flatMap {
        case ReductionResult(_, subst, _, deferred, progress) =>
          stopLogging()
          if (deferred.nonEmpty) {
            Result.Err(TypeError.SomeError("leftover constraints: " + deferred))
          } else {
            Result.Ok(subst)
          }
      }.toValidation
  }

  def visitInstanceDef(defn: KindedAst.Def, instConstrs: List[Ast.TypeConstraint], cenv0: Map[Symbol.ClassSym, Ast.ClassContext], eqEnv0: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef], root: KindedAst.Root, rootConstrs: ConstraintGeneration.PhaseResult)(implicit flix: Flix): Validation[Substitution, TypeError] = defn match {
    case KindedAst.Def(sym, KindedAst.Spec(doc, ann, mod, tparams, fparams, sc, tpe, eff, tconstrs, econstrs, loc), exp) =>
      val (infConstrs, infTpe, infEff, infRenv) = rootConstrs.defs(sym)

      if (sym.name == "Fixpoint.Ast.Datalog.toString$30158") {
        startLogging()
      }
      log(sym)

      val initialSubst = fparams.foldLeft(Substitution.empty) {
        case (acc, KindedAst.FormalParam(sym, mod, tpe, src, loc)) => acc ++ Substitution.singleton(sym.tvar.sym, tpe)
      }

      val renv = tparams.foldLeft(infRenv) {
        case (acc, KindedAst.TypeParam(name, sym, loc)) => acc.markRigid(sym)
      }

      val cenv = expandClassEnv(cenv0, instConstrs ++ tconstrs)
      val eqEnv = expandEqualityEnv(eqEnv0, econstrs) // MATT consider econstrs from instance

      val tpeConstr = TypingConstraint.Equality(tpe, infTpe, Provenance.ExpectLeft, loc)
      val effConstr = TypingConstraint.Equality(eff, infEff, Provenance.ExpectLeft, loc)
      val constrs = tpeConstr :: effConstr :: infConstrs
      resolve(constrs, renv, cenv, eqEnv, initialSubst).map(_.newSubst).map(x => {
        stopLogging();
        x
      }).toValidation // TODO check leftovers
  }

  def visitSig(sig: KindedAst.Sig, renv0: RigidityEnv, tconstrs0: List[Ast.TypeConstraint], cenv0: Map[Symbol.ClassSym, Ast.ClassContext], eqEnv0: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef], root: KindedAst.Root, infResult: InfResult)(implicit flix: Flix): Validation[Substitution, TypeError] = sig match {
    case KindedAst.Sig(_, _, None) => Validation.success(Substitution.empty)
    case KindedAst.Sig(sym, KindedAst.Spec(doc, ann, mod, tparams, fparams, sc, tpe, eff, tconstrs, econstrs, loc), exp) =>
      val InfResult(infConstrs, infTpe, infEff, infRenv) = infResult

      val initialSubst = fparams.foldLeft(Substitution.empty) {
        case (acc, KindedAst.FormalParam(sym, mod, tpe, src, loc)) => acc ++ Substitution.singleton(sym.tvar.sym, tpe)
      }

      val renv = tparams.foldLeft(infRenv ++ renv0) {
        case (acc, KindedAst.TypeParam(name, sym, loc)) => acc.markRigid(sym)
      }

      val cenv = expandClassEnv(cenv0, tconstrs ++ tconstrs0)
      val eqEnv = expandEqualityEnv(eqEnv0, econstrs) // MATT consider class econstrs

      val tpeConstr = TypingConstraint.Equality(tpe, infTpe, Provenance.ExpectLeft, loc)
      val effConstr = TypingConstraint.Equality(eff, infEff, Provenance.ExpectLeft, loc)
      val constrs = tpeConstr :: effConstr :: infConstrs
      resolve(constrs, renv, cenv, eqEnv, initialSubst).map(_.newSubst).toValidation // TODO check leftovers
  }

  private def simplifyEquality(tpe1: Type, tpe2: Type, renv: RigidityEnv, loc: SourceLocation, eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit flix: Flix): Result[EqualityResult, UnificationError] = (tpe1.kind, tpe2.kind) match {
    case (Kind.Eff, Kind.Eff) =>
      // first simplify the types to get rid of assocs if we can
      for {
        (t1, p1) <- simplifyType(tpe1, renv, eqEnv)
        (t2, p2) <- simplifyType(tpe2, renv, eqEnv)
        res0 <- EffUnification.unify(t1, t2, renv)
        _ = if (t1.size > 1000) throw InternalCompilerException("blah", SourceLocation.Unknown)
        // MATT throwing away prov
        // If eff unification has any constrs in output, then we know it failed so the subst is empty
        res =
          if (res0._2.isEmpty) {
            EqualityResult.Subst(res0._1, Nil)
          } else if (p1 || p2) {
            EqualityResult.Constrs(List(TypingConstraint.Equality(t1, t2, Provenance.Match, loc)))
          } else {
            EqualityResult.NoProgress
          }
      } yield res

    case (Kind.Bool, Kind.Bool) =>
      // first simplify the types to get rid of assocs if we can
      for {
        (t1, p1) <- simplifyType(tpe1, renv, eqEnv)
        (t2, p2) <- simplifyType(tpe2, renv, eqEnv)
        res0 <- BoolUnification.unify(t1, t2, renv)
        // MATT throwing away prov
        // If bool unification has any constrs in output, then we know it failed so the subst is empty
        res =
          if (res0._2.isEmpty) {
            EqualityResult.Subst(res0._1, Nil)
          } else if (p1 || p2) {
            EqualityResult.Constrs(List(TypingConstraint.Equality(t1, t2, Provenance.Match, loc)))
          } else {
            EqualityResult.NoProgress
          }
      } yield res


    case (Kind.RecordRow, Kind.RecordRow) =>
      // first simplify the types to get rid of assocs if we can
      for {
        (t1, p1) <- simplifyType(tpe1, renv, eqEnv)
        (t2, p2) <- simplifyType(tpe2, renv, eqEnv)
        res0 <- RecordUnification.unifyRows(t1, t2, renv)
        // If eff unification has any constrs in output, then we know it failed so the subst is empty
        // MATT throwing away prov
        res = if (res0._2.isEmpty) EqualityResult.Subst(res0._1, Nil) else throw InternalCompilerException("can't handle complex record stuff", loc)
      } yield res

    case (Kind.SchemaRow, Kind.SchemaRow) =>
      // first simplify the types to get rid of assocs if we can
      for {
        (t1, p1) <- simplifyType(tpe1, renv, eqEnv)
        (t2, p2) <- simplifyType(tpe2, renv, eqEnv)
        res <- SchemaUnification.unifyRows(t1, t2, renv)
        // MATT throwing away prov
      } yield EqualityResult.Subst(res, Nil)

    case (Kind.CaseSet(sym1), Kind.CaseSet(sym2)) =>
      for {
        (t1, p1) <- simplifyType(tpe1, renv, eqEnv)
        (t2, p2) <- simplifyType(tpe2, renv, eqEnv)
        res <- CaseSetUnification.unify(t1, t2, renv, sym1.universe, sym1)
      } yield EqualityResult.Subst(res, Nil)

    case _ => simplifyEqualityStar(tpe1, tpe2, renv, loc, eqEnv)
  }

  // Θ ⊩ᵤ τ₁ = τ₂ ⤳ U; R
  private def simplifyEqualityStar(tpe1: Type, tpe2: Type, renv: RigidityEnv, loc: SourceLocation, eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit flix: Flix): Result[EqualityResult, UnificationError] = (tpe1, tpe2) match {
    // varU
    case (x: Type.Var, t) if renv.isFlexible(x.sym) =>
      Result.Ok(EqualityResult.Subst(Substitution.singleton(x.sym, t), Nil)) // MATT throwing out prov
    // MATT need to consider occurs, levels

    // varU
    case (t, x: Type.Var) if renv.isFlexible(x.sym) =>
      Result.Ok(EqualityResult.Subst(Substitution.singleton(x.sym, t), Nil)) // MATT throwing out prov
    // MATT need to consider occurs, levels


    // reflU
    case (Type.Cst(c1, _), Type.Cst(c2, _)) if c1 == c2 => Result.Ok(EqualityResult.Constrs(Nil))
    case (x: Type.Var, y: Type.Var) if (x == y) => Result.Ok(EqualityResult.Constrs(Nil))

    case (Type.Alias(_, _, tpe, _), _) => simplifyEquality(tpe, tpe2, renv, loc, eqEnv)

    case (_, Type.Alias(_, _, tpe, _)) => simplifyEquality(tpe1, tpe, renv, loc, eqEnv)

    // appU
    case (Type.Apply(t11, t12, _), Type.Apply(t21, t22, _)) =>
      for {
        res1 <- simplifyEquality(t11, t21, renv, loc, eqEnv)
        res2 <- simplifyEquality(res1.subst(t12), res1.subst(t22), renv, loc, eqEnv)
      } yield res2 @@ res1

    // reflU
    case (Type.AssocType(cst1, args1, _, _), Type.AssocType(cst2, args2, _, _)) if cst1.sym == cst2.sym && args1 == args2 =>
      Result.Ok(EqualityResult.Constrs(Nil))

    // redU
    case (assoc: Type.AssocType, t2) =>
      for {
        (t1, progress) <- simplifyType(assoc, renv, eqEnv)
      } yield {
        if (progress) {
          EqualityResult.Constrs(List(TypingConstraint.Equality(t1, t2, Provenance.Match, loc)))
        } else {
          EqualityResult.NoProgress
        }
      }

    case (t1, assoc: Type.AssocType) =>
      for {
        (t2, progress) <- simplifyType(assoc, renv, eqEnv)
      } yield {
        if (progress) {
          EqualityResult.Constrs(List(TypingConstraint.Equality(t1, t2, Provenance.Match, loc)))
        } else {
          EqualityResult.NoProgress
        }
      }

    case _ => Result.Err(UnificationError.MismatchedTypes(tpe1, tpe2))
  }

  // Θ ⊩ τ ⤳ τ'
  private def simplifyType(tpe: Type, renv0: RigidityEnv, eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit flix: Flix): Result[(Type, Boolean), UnificationError] = tpe match {
    // A var is already simple.
    case t: Type.Var => Result.Ok((t, false))
    // A constant is already simple
    case t: Type.Cst => Result.Ok((t, false))
    // lapp_L and lapp_R
    case Type.Apply(tpe1, tpe2, loc) =>
      for {
        (t1, p1) <- simplifyType(tpe1, renv0, eqEnv)
        (t2, p2) <- simplifyType(tpe2, renv0, eqEnv)
      } yield {
        (Type.Apply(t1, t2, loc), p1 || p2)
      }
    // arg_R and syn_R
    case Type.AssocType(cst, arg, kind, loc) =>
      simplifyType(arg, renv0, eqEnv).flatMap {
        case (t, p) =>
          // we mark t's tvars as rigid so we get the substitution in the right direction
          val renv = t.typeVars.map(_.sym).foldLeft(RigidityEnv.empty)(_.markRigid(_))
          val insts = eqEnv(cst.sym)
          // find the first (and only) instance that matches
          insts.iterator.flatMap { // TODO ASSOC-TYPES generalize this pattern (also in monomorph)
            inst =>
              Unification.unifyTypes(arg, inst.arg, renv).toOption.map {
                case (subst, Nil) => subst(inst.ret)
                case (_, _) => throw InternalCompilerException("unexpected leftover constraints", SourceLocation.Unknown)
              }
          }.nextOption() match {
            // Can't reduce. Check what the original type was.
            case None =>
              t.baseType match {
                // If it's a var, it's ok. It may be substituted later to a type we can reduce.
                // Or it might be part of the signature as an associated type.
                case Type.Var(sym, loc) => Result.Ok((Type.AssocType(cst, t, kind, loc), p))
                // Otherwise it's a problem.
                case _ => Result.Err(UnificationError.IrreducibleAssocType(cst.sym, arg))
              }
            // We could reduce! Simplify further if possible.
            case Some(t) => simplifyType(t, renv0, eqEnv).map { case (res, _) => (res, true) }
          }
      }
    case Type.Alias(cst, args, t, loc) => simplifyType(t, renv0, eqEnv)
  }

  // Θ ⊩ₑ π ⤳ P
  // paper contains substitution R but it is only needed for equality
  private def simplifyClass(clazz: Symbol.ClassSym, tpe0: Type, classEnv: Map[Symbol.ClassSym, Ast.ClassContext], eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef], renv0: RigidityEnv, loc: SourceLocation)(implicit flix: Flix): Result[(List[TypingConstraint], Boolean), UnificationError] = {
    // redE
    simplifyType(tpe0, renv0, eqEnv).flatMap {
      case (t, progress) =>
        // Look at the head of the type.
        t.baseType match {
          // Case 1: Flexible var. It might be resolved later.
          case Type.Var(sym, _) if renv0.isFlexible(sym) =>
            Result.Ok((List(TypingConstraint.Class(clazz, t, loc)), progress))
          // Case 2: Something rigid (const or rigid var). We can look this up immediately.
          case _ =>
            // we mark t's tvars as rigid so we get the substitution in the right direction
            val renv = t.typeVars.map(_.sym).foldLeft(RigidityEnv.empty)(_.markRigid(_))
            val insts = classEnv(clazz).instances
            // find the first (and only) instance that matches
            insts.iterator.flatMap { // TODO ASSOC-TYPES generalize this pattern (also in monomorph)
              inst =>
                Unification.unifyTypes(t, inst.tpe, renv).toOption.map {
                  case (subst, Nil) => inst.tconstrs.map(subst.apply)
                  case (_, _) => throw InternalCompilerException("unexpected leftover constraints", SourceLocation.Unknown)
                }
            }.nextOption() match {
              case None => Result.Err(UnificationError.NoMatchingInstance(Ast.TypeConstraint(Ast.TypeConstraint.Head(clazz, loc), tpe0, loc)))
              case Some(tconstrs) =>
                // simplify all the implied constraints
                Result.traverse(tconstrs) {
                  case Ast.TypeConstraint(Ast.TypeConstraint.Head(c, _), arg, _) =>
                    simplifyClass(c, arg, classEnv, eqEnv, renv0, loc)
                } map {
                  case res =>
                    // MATT always made progress?
//                    val prog = progress || res.exists { case (_, p) => p }
                    val cs = res.flatMap { case (c, _) => c }
                    (cs, true)
                }
            }
        }
    }
  }
  //
  //  private def simplify(constr: TypingConstraint, renv: RigidityEnv, classEnv: Map[Symbol.ClassSym, Ast.ClassContext], eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit flix: Flix): Result[List[SimpleTypingConstraint], UnificationError] = constr match {
  //    case TypingConstraint.Equality(tpe1, tpe2, prov, loc) => simplifyEquality(tpe1, tpe2, renv, loc, eqEnv)
  //    case TypingConstraint.Class(sym, tpe, loc) => simplifyClass(sym, tpe, classEnv, eqEnv, renv, loc)
  //    case TypingConstraint.Purification(tpe1, tpe2, sym, level, prov, loc) => SimpleTypingConstraint.Pure(tpe1, tpe2, sym, loc)
  //  }

  private def resolve(constrs: List[TypingConstraint], renv: RigidityEnv, cenv: Map[Symbol.ClassSym, Ast.ClassContext], eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef], subst0: Substitution)(implicit flix: Flix): Result[ReductionResult, TypeError] = {
    var last = List.empty[TypingConstraint]
    var curr = constrs.sortBy(_.numVars)
    var subst = subst0
    var count = 0
    var prog = true
    while (prog) {
      if (count >= MaxIterations) {
        return Result.Err(HackError(UnificationError.IterationLimit(MaxIterations)))
      }

      count += 1
      recordGraph(TypingConstraint.toDotWithSubst(curr, subst), count)
      log(
        count.toString + "\n" + TypingConstraint.toDotWithSubst(curr, subst) + "\n" + "========================================="
      )

      last = curr
      reduceAll3(curr, renv, cenv, eqEnv, subst) match {
        case Result.Ok(ReductionResult(oldSubst, newSubst, oldConstrs, newConstrs, progress)) =>
          curr = newConstrs
          subst = newSubst
          prog = progress
        case res@Result.Err(_) =>
          subst.m.toList.sortBy(_._1).foreach {
            case pair => log(pair); log(pair._1.level)
          }
          stopLogging()
          return res.mapErr(HackError)
      }
    }
    Result.Ok(ReductionResult(subst0, subst, Nil, curr, progress = true))
  }

  private def reduceAll3(constrs: List[TypingConstraint], renv: RigidityEnv, cenv: Map[Symbol.ClassSym, Ast.ClassContext], eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef], subst0: Substitution)(implicit flix: Flix): Result[ReductionResult, UnificationError] = {
    def tryReduce(cs: List[TypingConstraint]): Result[ReductionResult, UnificationError] = cs match {
      case Nil => Result.Ok(ReductionResult(oldSubst = subst0, newSubst = subst0, oldConstrs = cs, newConstrs = cs, progress = false))
      case hd :: tl => reduceOne3(hd, renv, cenv, eqEnv, subst0).flatMap {
        // if we're just returning the same constraint, then have made no progress and we need to find something else to reduce
        case res if !res.progress => tryReduce(tl).map {
          // Case 1: progress made. send the head to the end
          case res if res.progress => res.copy(newConstrs = res.newConstrs :+ hd)
          // Case 2: no progress. Keep the order
          case res => res.copy(newConstrs = hd :: res.newConstrs)
        }
        // otherwise we have made progress so we're happy
        case res => Result.Ok(res.copy(newConstrs = tl ::: res.newConstrs))
      }
    }

    tryReduce(sort(constrs))
  }

  def sort(constrs: List[TypingConstraint]): List[TypingConstraint] =
    constrs.sortBy(_.index)

  private def reduceOne3(constr0: TypingConstraint, renv: RigidityEnv, cenv: Map[Symbol.ClassSym, Ast.ClassContext], eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef], subst0: Substitution)(implicit flix: Flix): Result[ReductionResult, UnificationError] = constr0 match {
    case TypingConstraint.Equality(tpe1, tpe2, prov, loc) =>
      val t1 = TypeMinimization.minimizeType(subst0(tpe1))
      val t2 = TypeMinimization.minimizeType(subst0(tpe2))
      simplifyEquality(t1, t2, renv, loc, eqEnv).map {
        case EqualityResult.Subst(subst, constrs) => ReductionResult(subst0, subst @@ subst0, List(constr0), constrs, progress = true)
        case EqualityResult.Constrs(constrs) => ReductionResult(subst0, subst0, List(constr0), constrs, progress = true)
        case EqualityResult.NoProgress => ReductionResult(subst0, subst0, List(constr0), List(constr0), progress = false)
      }
    case TypingConstraint.Class(sym, tpe, loc) =>
      simplifyClass(sym, subst0(tpe), cenv, eqEnv, renv, loc).map {
        case (constrs, progress) => ReductionResult(subst0, subst0, List(constr0), constrs, progress)
      }
    case TypingConstraint.Purification(sym, eff1, eff2, level, prov, nested0, loc) =>
      // First reduce nested constraints
      reduceAll3(nested0, renv, cenv, eqEnv, subst0).map {
        // Case 1: We have reduced everything below. Now reduce the purity constraint.
        case ReductionResult(_oldSubst, subst1, oldConstrs, newConstrs, progress) if newConstrs.isEmpty =>
          val e1 = subst1(eff1)
          // purify the inner type
          val e2Raw = subst1(eff2)
          val e2 = Substitution.singleton(sym, Type.Pure)(e2Raw)
          val qvars = e2Raw.typeVars.map(_.sym).filter(_.level >= level)
          val subst = qvars.foldLeft(subst1)(_.unbind(_))
          val constr = TypingConstraint.Equality(e1, TypeMinimization.minimizeType(e2), prov, loc)
          ReductionResult(subst0, subst, oldConstrs, List(constr), progress = true)
        // Case 2: Constraints remain below. Maintain the purity constraint.
        case ReductionResult(_oldSubst, subst, oldConstrs, newConstrs, progress) =>
          val constr = TypingConstraint.Purification(sym, eff1, eff2, level, prov, newConstrs, loc)
          ReductionResult(subst0, subst, oldConstrs, List(constr), progress)
      }
  }

  def fromUnificationResult(res: Result[(Substitution, List[Ast.BroadEqualityConstraint]), UnificationError]): Result[(Substitution, List[TypingConstraint]), UnificationError] = {
    res.map {
      case (subst, constrs0) =>
        val constrs = constrs0.map {
          case Ast.BroadEqualityConstraint(tpe1, tpe2) =>
            TypingConstraint.Equality(tpe1, tpe2, Provenance.Match, SourceLocation.Unknown) // MATT prov and loc
        }
        (subst, constrs)
    }
  }

  //  private def toUnificationReductionResult(unifResult: Result[(Substitution, List[Ast.BroadEqualityConstraint]), UnificationError], prov: Provenance, loc: SourceLocation): ReductionResult = unifResult match {
  //    // Case 1: Empty substitution. Just constraints here.
  //    case Result.Ok((subst, econstrs)) if subst == Substitution.empty =>
  //      val constrs = econstrs.map(toEqualityTypingConstraint(_, prov, loc))
  //      ReductionResult.NewConstraints(constrs)
  //    // Case 2: Both substitution and constraints.
  //    case Result.Ok((subst, econstrs)) =>
  //      val constrs = econstrs.map(toEqualityTypingConstraint(_, prov, loc))
  //      ReductionResult.NewSubstitution(subst, constrs)
  //    case Result.Err(e) => ReductionResult.Failure(e)
  //  }
  //
  //  private def toClassReductionResult(classResult: Validation[List[Ast.TypeConstraint], UnificationError], loc: SourceLocation): ReductionResult = classResult match {
  //    case Validation.Success(tconstrs) =>
  //      val constrs = tconstrs.map(toClassTypingConstraint(_, loc))
  //      ReductionResult.NewConstraints(constrs)
  //    case Validation.SoftFailure(t, errors) =>
  //      ReductionResult.Failure(errors.head) // TODO ASSOC-TYPES multiple errors?
  //    case Validation.Failure(errors) =>
  //      ReductionResult.Failure(errors.head) // TODO ASSOC-TYPES multiple errors?
  //  }

  def toEqualityTypingConstraint(econstr: Ast.BroadEqualityConstraint, prov: Provenance, loc: SourceLocation): TypingConstraint = econstr match {
    case Ast.BroadEqualityConstraint(tpe1, tpe2) => TypingConstraint.Equality(tpe1, tpe2, prov, loc)
  }

  def toClassTypingConstraint(tconstr: Ast.TypeConstraint, loc: SourceLocation): TypingConstraint = tconstr match {
    case Ast.TypeConstraint(head, arg, _) => TypingConstraint.Class(head.sym, arg, loc)
  }

}
