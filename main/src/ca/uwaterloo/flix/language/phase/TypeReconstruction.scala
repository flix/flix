/*
 * Copyright 2015-2023 Magnus Madsen, Matthew Lutze
 * Copyright 2024 Alexander Dybdahl Troelsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.*
import ca.uwaterloo.flix.language.ast.TypedAst.ApplyPosition
import ca.uwaterloo.flix.language.ast.jvm.JavaMethod
import ca.uwaterloo.flix.language.ast.shared.Decreasing
import ca.uwaterloo.flix.language.errors.TypeError
import ca.uwaterloo.flix.language.phase.typer.SubstitutionTree
import ca.uwaterloo.flix.language.phase.typer.jvm.JavaTypes
import ca.uwaterloo.flix.util.InternalCompilerException

object TypeReconstruction {

  /**
    * Reconstructs types in the given def with the inferred body `exp0`.
    */
  def visitDef(defn: KindedAst.Def, exp0: InferredAst.Expr, subst: SubstitutionTree)(implicit flix: Flix): TypedAst.Def = defn match {
    case KindedAst.Def(sym, spec0, _, loc) =>
      val spec = visitSpec(spec0)
      val exp = visitExp(exp0)(subst, flix)
      TypedAst.Def(sym, spec, exp, loc)
  }

  /**
    * Reconstructs types in the given sig with the inferred body `exp0`, if any.
    */
  def visitSig(sig: KindedAst.Sig, exp0: Option[InferredAst.Expr], subst: SubstitutionTree)(implicit flix: Flix): TypedAst.Sig = sig match {
    case KindedAst.Sig(sym, spec0, _, loc) =>
      val spec = visitSpec(spec0)
      val exp = exp0.map(visitExp(_)(subst, flix))
      TypedAst.Sig(sym, spec, exp, loc)
  }

  /**
    * Reconstructs types in the given spec.
    */
  private def visitSpec(spec: KindedAst.Spec): TypedAst.Spec = spec match {
    case KindedAst.Spec(doc, ann, mod, tparams0, fparams0, sc, tpe, eff, tconstrs, econstrs) =>
      val tparams = tparams0.map(visitTypeParam)
      val fparams = fparams0.map(visitSpecFormalParam)
      val eff1 = eff.getOrElse(Type.Pure)
      // We do not perform substitution on any of the types because they should all be rigid.
      val typedEconstrs = econstrs.map(ec => TypedAst.EqualityConstraint(Type.AssocType(ec.symUse, ec.tpe1, ec.tpe2.kind, ec.loc), ec.tpe2, ec.loc))
      TypedAst.Spec(doc, ann, mod, tparams, fparams, sc, tpe, eff1, tconstrs, typedEconstrs)
  }

  /**
    * Reconstructs types in the given tparams.
    */
  private def visitTypeParam(tparam: KindedAst.TypeParam): TypedAst.TypeParam = tparam match {
    case KindedAst.TypeParam(name, sym, loc) => TypedAst.TypeParam(name, sym, loc)
  }

  /**
    * Reconstructs types in the given fparam of a spec.
    */
  private def visitSpecFormalParam(fparam: KindedAst.FormalParam): TypedAst.FormalParam = fparam match {
    case KindedAst.FormalParam(sym, tpe, src, loc) =>
      // We do not perform substitution on the type because it should be rigid.
      val bnd = TypedAst.Binder(sym, tpe)
      TypedAst.FormalParam(bnd, tpe, src, Decreasing.NonDecreasing, loc)
  }

  /**
    * Reconstructs types in the given operation.
    */
  def visitOp(op: KindedAst.Op): TypedAst.Op = op match {
    case KindedAst.Op(sym, spec0, loc) =>
      val spec = visitSpec(spec0)
      TypedAst.Op(sym, spec, loc)
  }

  /**
    * Reconstructs types in the given expression.
    */
  private def visitExp(exp0: InferredAst.Expr)(implicit subst: SubstitutionTree, flix: Flix): TypedAst.Expr = exp0 match {
    case InferredAst.Expr.Var(sym, tpe, loc) =>
      TypedAst.Expr.Var(sym, visitType(tpe), loc)

    case InferredAst.Expr.Hole(sym, scp, tpe, eff, loc) =>
      TypedAst.Expr.Hole(sym, scp, visitType(tpe), visitType(eff), loc)

    case InferredAst.Expr.HoleWithExp(exp, scp, tpe, eff, loc) =>
      val e = visitExp(exp)
      TypedAst.Expr.HoleWithExp(e, scp, visitType(tpe), visitType(eff), loc)

    case InferredAst.Expr.OpenAs(symUse, exp, tpe, loc) =>
      val e = visitExp(exp)
      TypedAst.Expr.OpenAs(symUse, e, visitType(tpe), loc)

    case InferredAst.Expr.Use(sym, alias, exp, loc) =>
      val e = visitExp(exp)
      TypedAst.Expr.Use(sym, alias, e, loc)

    case InferredAst.Expr.Cst(cst, tpe, loc) =>
      TypedAst.Expr.Cst(cst, visitType(tpe), loc)

    case InferredAst.Expr.ApplyClo(exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      TypedAst.Expr.ApplyClo(e1, e2, visitType(tpe), visitType(eff), ApplyPosition.NonTail, loc)

    case InferredAst.Expr.ApplyDef(symUse, exps, targs, itpe, tpe, eff, loc) =>
      val es = exps.map(visitExp)
      TypedAst.Expr.ApplyDef(symUse, es, targs.map(visitType), visitType(itpe), visitType(tpe), visitType(eff), ApplyPosition.NonTail, loc)

    case InferredAst.Expr.ApplyLocalDef(symUse, exps, arrowTpe, tpe, eff, loc) =>
      val es = exps.map(visitExp)
      TypedAst.Expr.ApplyLocalDef(symUse, es, visitType(arrowTpe), visitType(tpe), visitType(eff), ApplyPosition.NonTail, loc)

    case InferredAst.Expr.ApplyOp(symUse, exps, tpe, eff, loc) =>
      val es = exps.map(visitExp)
      TypedAst.Expr.ApplyOp(symUse, es, visitType(tpe), visitType(eff), ApplyPosition.NonTail, loc)

    case InferredAst.Expr.ApplySig(symUse, exps, targ, targs, itpe, tpe, eff, loc) =>
      val es = exps.map(visitExp)
      TypedAst.Expr.ApplySig(symUse, es, visitType(targ), targs.map(visitType), visitType(itpe), visitType(tpe), visitType(eff), ApplyPosition.NonTail, loc)

    case InferredAst.Expr.Lambda(fparam, exp, tpe, loc) =>
      val p = visitFormalParam(fparam)
      val e = visitExp(exp)
      TypedAst.Expr.Lambda(p, e, visitType(tpe), loc)

    case InferredAst.Expr.Unary(sop, exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      TypedAst.Expr.Unary(sop, e, visitType(tpe), visitType(eff), loc)

    case InferredAst.Expr.Binary(sop, exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      TypedAst.Expr.Binary(sop, e1, e2, visitType(tpe), visitType(eff), loc)

    case InferredAst.Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val e3 = visitExp(exp3)
      TypedAst.Expr.IfThenElse(e1, e2, e3, visitType(tpe), visitType(eff), loc)

    case InferredAst.Expr.Stm(exps, exp, tpe, eff, loc) =>
      val es = exps.map(visitExp)
      val e = visitExp(exp)
      TypedAst.Expr.Stm(es, e, visitType(tpe), visitType(eff), loc)

    case InferredAst.Expr.Discard(exp, eff, loc) =>
      val e = visitExp(exp)
      TypedAst.Expr.Discard(e, visitType(eff), loc)

    case InferredAst.Expr.Let(bnd, exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      TypedAst.Expr.Let(visitBinder(bnd), e1, e2, visitType(tpe), visitType(eff), loc)

    case InferredAst.Expr.LocalDef(ann, bnd, fparams, exp1, exp2, tpe, eff, loc) =>
      val fps = fparams.map(visitFormalParam)
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      TypedAst.Expr.LocalDef(ann, visitBinder(bnd), fps, e1, e2, visitType(tpe), visitType(eff), loc)

    case InferredAst.Expr.Region(bnd, regSym, exp, tpe, eff, loc) =>
      // Use the appropriate branch for the scope.
      val e = visitExp(exp)(subst.branches.getOrElse(regSym, SubstitutionTree.empty), flix)
      TypedAst.Expr.Region(visitBinder(bnd), regSym, e, visitType(tpe), visitType(eff), loc)

    case InferredAst.Expr.Match(exp, rules, tpe, eff, loc) =>
      val e = visitExp(exp)
      val rs = rules.map(visitMatchRule)
      TypedAst.Expr.Match(e, rs, visitType(tpe), visitType(eff), loc)

    case InferredAst.Expr.RestrictableChoose(star, exp, rules, tpe, eff, loc) =>
      val e = visitExp(exp)
      val rs = rules.map(visitRestrictableChooseRule)
      TypedAst.Expr.RestrictableChoose(star, e, rs, visitType(tpe), visitType(eff), loc)

    case InferredAst.Expr.ExtMatch(exp, rules, tpe, eff, loc) =>
      val e = visitExp(exp)
      val rs = rules.map(visitExtMatchRule)
      TypedAst.Expr.ExtMatch(e, rs, visitType(tpe), visitType(eff), loc)

    case InferredAst.Expr.Tag(symUse, exps, tpe, eff, loc) =>
      val es = exps.map(visitExp)
      TypedAst.Expr.Tag(symUse, es, visitType(tpe), visitType(eff), loc)

    case InferredAst.Expr.RestrictableTag(symUse, exps, tpe, eff, loc) =>
      val es = exps.map(visitExp)
      TypedAst.Expr.RestrictableTag(symUse, es, visitType(tpe), visitType(eff), loc)

    case InferredAst.Expr.ExtTag(label, exps, tpe, eff, loc) =>
      val es = exps.map(visitExp)
      TypedAst.Expr.ExtTag(label, es, visitType(tpe), visitType(eff), loc)

    case InferredAst.Expr.Tuple(exps, tpe, eff, loc) =>
      val es = exps.map(visitExp)
      TypedAst.Expr.Tuple(es, visitType(tpe), visitType(eff), loc)

    case InferredAst.Expr.RecordSelect(exp, label, tpe, eff, loc) =>
      val e = visitExp(exp)
      TypedAst.Expr.RecordSelect(e, label, visitType(tpe), visitType(eff), loc)

    case InferredAst.Expr.RecordExtend(label, exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      TypedAst.Expr.RecordExtend(label, e1, e2, visitType(tpe), visitType(eff), loc)

    case InferredAst.Expr.RecordRestrict(label, exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      TypedAst.Expr.RecordRestrict(label, e, visitType(tpe), visitType(eff), loc)

    case InferredAst.Expr.ArrayLit(exps, exp, tpe, eff, loc) =>
      val es = exps.map(visitExp)
      val e = visitExp(exp)
      TypedAst.Expr.ArrayLit(es, e, visitType(tpe), visitType(eff), loc)

    case InferredAst.Expr.ArrayNew(exp1, exp2, exp3, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val e3 = visitExp(exp3)
      TypedAst.Expr.ArrayNew(e1, e2, e3, visitType(tpe), visitType(eff), loc)

    case InferredAst.Expr.ArrayLoad(exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      TypedAst.Expr.ArrayLoad(e1, e2, visitType(tpe), visitType(eff), loc)

    case InferredAst.Expr.ArrayStore(exp1, exp2, exp3, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val e3 = visitExp(exp3)
      TypedAst.Expr.ArrayStore(e1, e2, e3, visitType(eff), loc)

    case InferredAst.Expr.ArrayLength(exp, eff, loc) =>
      val e = visitExp(exp)
      TypedAst.Expr.ArrayLength(e, visitType(eff), loc)

    case InferredAst.Expr.StructNew(sym, fields, region, tpe, eff, loc) =>
      val r = region.map(visitExp)
      val fs = fields.map { case (k, v) => (k, visitExp(v)) }
      TypedAst.Expr.StructNew(sym, fs, r, visitType(tpe), visitType(eff), loc)

    case InferredAst.Expr.StructGet(exp, symUse, tpe, eff, loc) =>
      val e = visitExp(exp)
      TypedAst.Expr.StructGet(e, symUse, visitType(tpe), visitType(eff), loc)

    case InferredAst.Expr.StructPut(exp1, symUse, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      TypedAst.Expr.StructPut(e1, symUse, e2, visitType(tpe), visitType(eff), loc)

    case InferredAst.Expr.VectorLit(exps, tpe, eff, loc) =>
      val es = exps.map(visitExp)
      TypedAst.Expr.VectorLit(es, visitType(tpe), visitType(eff), loc)

    case InferredAst.Expr.VectorLoad(exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      TypedAst.Expr.VectorLoad(e1, e2, visitType(tpe), visitType(eff), loc)

    case InferredAst.Expr.VectorLength(exp, loc) =>
      val e = visitExp(exp)
      TypedAst.Expr.VectorLength(e, loc)

    case InferredAst.Expr.Ascribe(exp, expectedType, expectedEff, tpe, eff, loc) =>
      val e = visitExp(exp)
      TypedAst.Expr.Ascribe(e, expectedType, expectedEff, visitType(tpe), visitType(eff), loc)

    case InferredAst.Expr.InstanceOf(exp, clazz, loc) =>
      val e = visitExp(exp)
      TypedAst.Expr.InstanceOf(e, clazz, loc)

    case InferredAst.Expr.CheckedCast(cast, exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      TypedAst.Expr.CheckedCast(cast, e, visitType(tpe), visitType(eff), loc)

    case InferredAst.Expr.UncheckedCast(exp, declaredType, declaredEff, tpe, eff, loc) =>
      val e = visitExp(exp)
      TypedAst.Expr.UncheckedCast(e, declaredType.map(visitType), declaredEff.map(visitType), visitType(tpe), visitType(eff), loc)

    case InferredAst.Expr.Unsafe(exp, runEff, asEff, tpe, eff, loc) =>
      val e = visitExp(exp)
      TypedAst.Expr.Unsafe(e, runEff, asEff, visitType(tpe), visitType(eff), loc)

    case InferredAst.Expr.TryCatch(exp, rules, tpe, eff, loc) =>
      val e = visitExp(exp)
      val rs = rules.map(visitCatchRule)
      TypedAst.Expr.TryCatch(e, rs, visitType(tpe), visitType(eff), loc)

    case InferredAst.Expr.Throw(exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      TypedAst.Expr.Throw(e, visitType(tpe), visitType(eff), loc)

    case InferredAst.Expr.Handler(symUse, rules, bodyType, bodyEff, handledEff, tpe, loc) =>
      val rs = rules.map(visitHandlerRule)
      TypedAst.Expr.Handler(symUse, rs, visitType(bodyType), visitType(bodyEff), visitType(handledEff), visitType(tpe), loc)

    case InferredAst.Expr.RunWith(exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      TypedAst.Expr.RunWith(e1, e2, visitType(tpe), visitType(eff), loc)

    case InferredAst.Expr.InvokeConstructor(exps, jvar, tpe, eff, loc) =>
      val es0 = exps.map(visitExp)
      val constructorTpe = subst(jvar)
      constructorTpe match {
        case Type.Cst(TypeConstructor.JvmConstructor(constructor), _) =>
          val es = getArgumentsWithVarArgs(constructor, es0, loc)
          TypedAst.Expr.InvokeConstructor(constructor, es, visitType(tpe), visitType(eff), loc)
        case _ =>
          TypedAst.Expr.Error(TypeError.UnresolvedConstructor(loc), visitType(tpe), visitType(eff))
      }

    case InferredAst.Expr.InvokeSuperConstructor(exps, jvar, tpe, eff, loc) =>
      val es0 = exps.map(visitExp)
      val constructorTpe = subst(jvar)
      constructorTpe match {
        case Type.Cst(TypeConstructor.JvmConstructor(constructor), _) =>
          val es = getArgumentsWithVarArgs(constructor, es0, loc)
          TypedAst.Expr.InvokeSuperConstructor(constructor, es, visitType(tpe), visitType(eff), loc)
        case _ =>
          TypedAst.Expr.Error(TypeError.UnresolvedConstructor(loc), visitType(tpe), visitType(eff))
      }

    case InferredAst.Expr.InvokeMethod(exp, exps, jvar, tpe, eff, loc) =>
      val e = visitExp(exp)
      val es0 = exps.map(visitExp)
      val methodTpe = subst(jvar)
      methodTpe.typeConstructor match {
        case Some(TypeConstructor.JvmMethod(method, _)) =>
          val es = getArgumentsWithVarArgs(method, es0, loc)
          TypedAst.Expr.InvokeMethod(method, e, es, visitType(tpe), visitType(eff), loc)
        case _ =>
          TypedAst.Expr.Error(TypeError.UnresolvedMethod(loc), methodTpe, visitType(eff))
      }

    case InferredAst.Expr.InvokeSuperMethod(exps, jvar, tpe, eff, loc) =>
      val es0 = exps.map(visitExp)
      val methodTpe = subst(jvar)
      methodTpe.typeConstructor match {
        case Some(TypeConstructor.JvmMethod(method, _)) =>
          val es = getArgumentsWithVarArgs(method, es0, loc)
          TypedAst.Expr.InvokeSuperMethod(method, es, visitType(tpe), visitType(eff), loc)
        case _ =>
          TypedAst.Expr.Error(TypeError.UnresolvedMethod(loc), visitType(tpe), visitType(eff))
      }

    case InferredAst.Expr.InvokeStaticMethod(exps, jvar, tpe, eff, loc) =>
      val es0 = exps.map(visitExp)
      val methodTpe = subst(jvar)
      methodTpe.typeConstructor match {
        case Some(TypeConstructor.JvmMethod(method, _)) =>
          val es = getArgumentsWithVarArgs(method, es0, loc)
          TypedAst.Expr.InvokeStaticMethod(method, es, visitType(tpe), visitType(eff), loc)
        case _ =>
          TypedAst.Expr.Error(TypeError.UnresolvedStaticMethod(loc), methodTpe, visitType(eff))
      }

    case InferredAst.Expr.GetField(exp, jvar, tpe, eff, loc) =>
      val e = visitExp(exp)
      val jvarType = subst(jvar)
      jvarType match {
        case Type.Cst(TypeConstructor.JvmField(field), fieldLoc) =>
          TypedAst.Expr.GetField(field, e, visitType(tpe), visitType(eff), fieldLoc)
        case _ =>
          TypedAst.Expr.Error(TypeError.UnresolvedField(loc), jvarType, visitType(eff))
      }

    case InferredAst.Expr.PutField(field, exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      TypedAst.Expr.PutField(field, e1, e2, visitType(tpe), visitType(eff), loc)

    case InferredAst.Expr.GetStaticField(field, tpe, eff, loc) =>
      TypedAst.Expr.GetStaticField(field, visitType(tpe), visitType(eff), loc)

    case InferredAst.Expr.PutStaticField(field, exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      TypedAst.Expr.PutStaticField(field, e, visitType(tpe), visitType(eff), loc)

    case InferredAst.Expr.NewObject(sym, clazz, tpe, eff, constructors, methods, loc) =>
      val cs = constructors.map(visitJvmConstructor)
      val ms = methods.map(visitJvmMethod)
      TypedAst.Expr.NewObject(sym, clazz, visitType(tpe), visitType(eff), cs, ms, loc)

    case InferredAst.Expr.NewChannel(exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      TypedAst.Expr.NewChannel(e, visitType(tpe), visitType(eff), loc)

    case InferredAst.Expr.GetChannel(exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      TypedAst.Expr.GetChannel(e, visitType(tpe), visitType(eff), loc)

    case InferredAst.Expr.PutChannel(exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      TypedAst.Expr.PutChannel(e1, e2, visitType(tpe), visitType(eff), loc)

    case InferredAst.Expr.SelectChannel(rules, default, tpe, eff, loc) =>
      val rs = rules.map(visitSelectChannelRule)
      val d = default.map(visitExp)
      TypedAst.Expr.SelectChannel(rs, d, visitType(tpe), visitType(eff), loc)

    case InferredAst.Expr.Spawn(exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      TypedAst.Expr.Spawn(e1, e2, visitType(tpe), visitType(eff), loc)

    case InferredAst.Expr.ParYield(frags, exp, tpe, eff, loc) =>
      val fs = frags.map(visitParYieldFragment)
      val e = visitExp(exp)
      TypedAst.Expr.ParYield(fs, e, visitType(tpe), visitType(eff), loc)

    case InferredAst.Expr.Lazy(exp, tpe, loc) =>
      val e = visitExp(exp)
      TypedAst.Expr.Lazy(e, visitType(tpe), loc)

    case InferredAst.Expr.Force(exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      TypedAst.Expr.Force(e, visitType(tpe), visitType(eff), loc)

    case InferredAst.Expr.FixpointConstraintSet(cs0, tpe, loc) =>
      val cs = cs0.map(visitConstraint)
      TypedAst.Expr.FixpointConstraintSet(cs, visitType(tpe), loc)

    case InferredAst.Expr.FixpointLambda(pparams, exp, tpe, eff, loc) =>
      val ps = pparams.map(visitPredicateParam)
      val e = visitExp(exp)
      TypedAst.Expr.FixpointLambda(ps, e, visitType(tpe), visitType(eff), loc)

    case InferredAst.Expr.FixpointMerge(exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      TypedAst.Expr.FixpointMerge(e1, e2, visitType(tpe), visitType(eff), loc)

    case InferredAst.Expr.FixpointQueryWithProvenance(exps, select, withh, tpe, eff, loc) =>
      val es = exps.map(visitExp)
      val s = visitHeadPredicate(select)
      TypedAst.Expr.FixpointQueryWithProvenance(es, s, withh, visitType(tpe), visitType(eff), loc)

    case InferredAst.Expr.FixpointQueryWithSelect(exps, queryExp, selects, from, where, pred, tpe, eff, loc) =>
      val es = exps.map(visitExp)
      val qe = visitExp(queryExp)
      val ss = selects.map(visitExp)
      val f = from.map(visitBodyPredicate)
      val w = where.map(visitExp)
      TypedAst.Expr.FixpointQueryWithSelect(es, qe, ss, f, w, pred, visitType(tpe), visitType(eff), loc)

    case InferredAst.Expr.FixpointSolveWithProject(exps, optPreds, mode, tpe, eff, loc) =>
      val es = exps.map(visitExp)
      TypedAst.Expr.FixpointSolveWithProject(es, optPreds, mode, visitType(tpe), visitType(eff), loc)

    case InferredAst.Expr.FixpointInjectInto(exps, predsAndArities, tpe, eff, loc) =>
      val es = exps.map(visitExp)
      TypedAst.Expr.FixpointInjectInto(es, predsAndArities, visitType(tpe), visitType(eff), loc)

    case InferredAst.Expr.Error(m, tpe, eff) =>
      TypedAst.Expr.Error(m, visitType(tpe), visitType(eff))
  }

  /**
    * Returns the given arguments `es` with varargs arguments wrapped in a VectorLit if needed.
    */
  private def getArgumentsWithVarArgs(method: JavaMethod, es: List[TypedAst.Expr], loc: SourceLocation)(implicit flix: Flix): List[TypedAst.Expr] = {
    if (!method.isVarArgs) return es

    val descriptor = method.ref.descriptor
    val declaredArity = descriptor.parameterCount()
    val actualArity = es.length

    if (actualArity == declaredArity - 1) {
      // Case 1: Varargs omitted entirely. Insert an empty vector.
      val componentDesc = descriptor.parameterType(declaredArity - 1).componentType()
      val varArgsType = JavaTypes.flixTypeOf(componentDesc, loc)
      val varArgs = TypedAst.Expr.VectorLit(Nil, Type.mkVector(varArgsType, loc), Type.Pure, loc)
      es :+ varArgs
    } else if (actualArity >= declaredArity) {
      val normalArgs = es.take(declaredArity - 1)
      val varArgExprs = es.drop(declaredArity - 1)

      // A single trailing argument is the varargs array itself if it is assignable to the array parameter.
      // Otherwise it is an element, e.g. a `Vector[Int32]` passed for `T...` (erased to `Object[]`).
      val alreadyWrapped = varArgExprs match {
        case single :: Nil => JavaTypes.isVarArgsArray(single.tpe, descriptor.parameterType(declaredArity - 1), loc)
        case _ => false
      }

      if (alreadyWrapped) {
        // Already the varargs array, no wrapping needed.
        es
      } else {
        // Case 2: Individual varargs arguments. Wrap them into a VectorLit.
        val componentType = varArgExprs.head.tpe
        val varArgsEff = Type.mkUnion(varArgExprs.map(_.eff), loc)
        val varArgs = TypedAst.Expr.VectorLit(varArgExprs, Type.mkVector(componentType, loc), varArgsEff, loc)
        normalArgs :+ varArgs
      }
    } else {
      // Too few args; impossible.
      throw InternalCompilerException("unexpected too-few varargs", loc)
    }
  }

  /**
    * Reconstructs types in the given constraint.
    */
  private def visitConstraint(c0: InferredAst.Constraint)(implicit subst: SubstitutionTree, flix: Flix): TypedAst.Constraint = c0 match {
    case InferredAst.Constraint(cparams0, head0, body0, loc) =>
      val head = visitHeadPredicate(head0)
      val body = body0.map(visitBodyPredicate)
      val cparams = cparams0.map(visitConstraintParam)
      TypedAst.Constraint(cparams, head, body, loc)
  }

  /**
    * Reconstructs types in the given constraint param.
    */
  private def visitConstraintParam(cparam: InferredAst.ConstraintParam)(implicit subst: SubstitutionTree): TypedAst.ConstraintParam = cparam match {
    case InferredAst.ConstraintParam(bnd, tpe, loc) =>
      TypedAst.ConstraintParam(visitBinder(bnd), visitType(tpe), loc)
  }

  /**
    * Reconstructs types in the given predicate param.
    */
  private def visitPredicateParam(pparam: InferredAst.PredicateParam)(implicit subst: SubstitutionTree): TypedAst.PredicateParam = pparam match {
    case InferredAst.PredicateParam(pred, tpe, loc) =>
      TypedAst.PredicateParam(pred, visitType(tpe), loc)
  }

  /**
    * Reconstructs types in the given JVM constructor.
    */
  private def visitJvmConstructor(constructor: InferredAst.JvmConstructor)(implicit subst: SubstitutionTree, flix: Flix): TypedAst.JvmConstructor = constructor match {
    case InferredAst.JvmConstructor(exp, retTpe, eff, loc) =>
      val e = visitExp(exp)
      TypedAst.JvmConstructor(e, retTpe, eff, loc)
  }

  /**
    * Reconstructs types in the given JVM method.
    */
  private def visitJvmMethod(method: InferredAst.JvmMethod)(implicit subst: SubstitutionTree, flix: Flix): TypedAst.JvmMethod = method match {
    case InferredAst.JvmMethod(ann, ident, fparams, exp, retTpe, eff, loc) =>
      val fps = fparams.map(visitFormalParam)
      val e = visitExp(exp)
      TypedAst.JvmMethod(ann, ident, fps, e, retTpe, eff, loc)
  }

  /**
    * Reconstructs types in the given catch rule.
    */
  private def visitCatchRule(rule: InferredAst.CatchRule)(implicit subst: SubstitutionTree, flix: Flix): TypedAst.CatchRule = rule match {
    case InferredAst.CatchRule(bnd, clazz, exp, loc) =>
      val e = visitExp(exp)
      TypedAst.CatchRule(visitBinder(bnd), clazz, e, loc)
  }

  /**
    * Reconstructs types in the given handler rule.
    */
  private def visitHandlerRule(rule: InferredAst.HandlerRule)(implicit subst: SubstitutionTree, flix: Flix): TypedAst.HandlerRule = rule match {
    case InferredAst.HandlerRule(symUse, fparams, exp, loc) =>
      val fps = fparams.map(visitFormalParam)
      val e = visitExp(exp)
      TypedAst.HandlerRule(symUse, fps, e, loc)
  }

  /**
    * Reconstructs types in the given match rule.
    */
  private def visitMatchRule(rule: InferredAst.MatchRule)(implicit subst: SubstitutionTree, flix: Flix): TypedAst.MatchRule = rule match {
    case InferredAst.MatchRule(pat, guard, exp, loc) =>
      val p = visitPattern(pat)
      val g = guard.map(visitExp)
      val e = visitExp(exp)
      TypedAst.MatchRule(p, g, e, loc)
  }

  /**
    * Reconstructs types in the given restrictable choose rule.
    */
  private def visitRestrictableChooseRule(rule: InferredAst.RestrictableChooseRule)(implicit subst: SubstitutionTree, flix: Flix): TypedAst.RestrictableChooseRule = rule match {
    case InferredAst.RestrictableChooseRule(pat, exp) =>
      val p = visitRestrictableChoosePattern(pat)
      val e = visitExp(exp)
      TypedAst.RestrictableChooseRule(p, e)
  }

  /**
    * Reconstructs types in the given ext-match rule.
    */
  private def visitExtMatchRule(rule: InferredAst.ExtMatchRule)(implicit subst: SubstitutionTree, flix: Flix): TypedAst.ExtMatchRule = rule match {
    case InferredAst.ExtMatchRule(pat, exp, loc) =>
      val p = visitExtPat(pat)
      val e = visitExp(exp)
      TypedAst.ExtMatchRule(p, e, loc)
  }

  /**
    * Reconstructs types in the given select channel rule.
    */
  private def visitSelectChannelRule(rule: InferredAst.SelectChannelRule)(implicit subst: SubstitutionTree, flix: Flix): TypedAst.SelectChannelRule = rule match {
    case InferredAst.SelectChannelRule(bnd, chan, exp, loc) =>
      val c = visitExp(chan)
      val e = visitExp(exp)
      TypedAst.SelectChannelRule(visitBinder(bnd), c, e, loc)
  }

  /**
    * Reconstructs types in the given par-yield fragment.
    */
  private def visitParYieldFragment(frag: InferredAst.ParYieldFragment)(implicit subst: SubstitutionTree, flix: Flix): TypedAst.ParYieldFragment = frag match {
    case InferredAst.ParYieldFragment(pat, exp, loc) =>
      val p = visitPattern(pat)
      val e = visitExp(exp)
      TypedAst.ParYieldFragment(p, e, loc)
  }

  /**
    * Reconstructs types in the given pattern.
    */
  private def visitPattern(pat0: InferredAst.Pattern)(implicit subst: SubstitutionTree): TypedAst.Pattern = pat0 match {
    case InferredAst.Pattern.Wild(tpe, loc) =>
      TypedAst.Pattern.Wild(visitType(tpe), loc)

    case InferredAst.Pattern.Var(bnd, tpe, loc) =>
      TypedAst.Pattern.Var(visitBinder(bnd), visitType(tpe), loc)

    case InferredAst.Pattern.Cst(cst, tpe, loc) =>
      TypedAst.Pattern.Cst(cst, visitType(tpe), loc)

    case InferredAst.Pattern.Tag(symUse, pats, tpe, loc) =>
      TypedAst.Pattern.Tag(symUse, pats.map(visitPattern), visitType(tpe), loc)

    case InferredAst.Pattern.Tuple(pats, tpe, loc) =>
      TypedAst.Pattern.Tuple(pats.map(visitPattern), visitType(tpe), loc)

    case InferredAst.Pattern.Record(pats, pat, tpe, loc) =>
      val ps = pats.map {
        case InferredAst.Pattern.Record.RecordLabelPattern(label, pat1, tpe1, loc1) =>
          TypedAst.Pattern.Record.RecordLabelPattern(label, visitPattern(pat1), visitType(tpe1), loc1)
      }
      val p = visitPattern(pat)
      TypedAst.Pattern.Record(ps, p, visitType(tpe), loc)

    case InferredAst.Pattern.Error(tpe, loc) =>
      TypedAst.Pattern.Error(visitType(tpe), loc)
  }

  /**
    * Reconstructs types in the given restrictable choose pattern.
    */
  private def visitRestrictableChoosePattern(pat0: InferredAst.RestrictableChoosePattern)(implicit subst: SubstitutionTree): TypedAst.RestrictableChoosePattern = pat0 match {
    case InferredAst.RestrictableChoosePattern.Tag(symUse, pats, tpe, loc) =>
      val ps = pats.map(visitVarOrWild)
      TypedAst.RestrictableChoosePattern.Tag(symUse, ps, visitType(tpe), loc)

    case InferredAst.RestrictableChoosePattern.Error(tpe, loc) =>
      TypedAst.RestrictableChoosePattern.Error(visitType(tpe), loc)
  }

  /**
    * Reconstructs types in the given variable or wildcard pattern.
    */
  private def visitVarOrWild(pat0: InferredAst.RestrictableChoosePattern.VarOrWild)(implicit subst: SubstitutionTree): TypedAst.RestrictableChoosePattern.VarOrWild = pat0 match {
    case InferredAst.RestrictableChoosePattern.Wild(tpe, loc) =>
      TypedAst.RestrictableChoosePattern.Wild(visitType(tpe), loc)

    case InferredAst.RestrictableChoosePattern.Var(bnd, tpe, loc) =>
      TypedAst.RestrictableChoosePattern.Var(visitBinder(bnd), visitType(tpe), loc)

    case InferredAst.RestrictableChoosePattern.Error(tpe, loc) =>
      TypedAst.RestrictableChoosePattern.Error(visitType(tpe), loc)
  }

  /**
    * Reconstructs types in the given ext pattern.
    */
  private def visitExtPat(pat0: InferredAst.ExtPattern)(implicit subst: SubstitutionTree): TypedAst.ExtPattern = pat0 match {
    case InferredAst.ExtPattern.Default(loc) =>
      TypedAst.ExtPattern.Default(loc)

    case InferredAst.ExtPattern.Tag(label, pats, loc) =>
      val ps = pats.map(visitExtTagPat)
      TypedAst.ExtPattern.Tag(label, ps, loc)

    case InferredAst.ExtPattern.Error(loc) =>
      TypedAst.ExtPattern.Error(loc)
  }

  /**
    * Reconstructs types in the given ext tag pattern.
    */
  private def visitExtTagPat(pat0: InferredAst.ExtTagPattern)(implicit subst: SubstitutionTree): TypedAst.ExtTagPattern = pat0 match {
    case InferredAst.ExtTagPattern.Wild(tpe, loc) =>
      TypedAst.ExtTagPattern.Wild(visitType(tpe), loc)

    case InferredAst.ExtTagPattern.Var(bnd, tpe, loc) =>
      TypedAst.ExtTagPattern.Var(visitBinder(bnd), visitType(tpe), loc)

    case InferredAst.ExtTagPattern.Unit(tpe, loc) =>
      TypedAst.ExtTagPattern.Unit(visitType(tpe), loc)

    case InferredAst.ExtTagPattern.Error(tpe, loc) =>
      TypedAst.ExtTagPattern.Error(visitType(tpe), loc)
  }

  /**
    * Reconstructs types in the given head predicate.
    */
  private def visitHeadPredicate(head0: InferredAst.Predicate.Head)(implicit subst: SubstitutionTree, flix: Flix): TypedAst.Predicate.Head = head0 match {
    case InferredAst.Predicate.Head.Atom(pred, den, terms, tpe, loc) =>
      val ts = terms.map(visitExp)
      TypedAst.Predicate.Head.Atom(pred, den, ts, visitType(tpe), loc)
  }

  /**
    * Reconstructs types in the given body predicate.
    */
  private def visitBodyPredicate(body0: InferredAst.Predicate.Body)(implicit subst: SubstitutionTree, flix: Flix): TypedAst.Predicate.Body = body0 match {
    case InferredAst.Predicate.Body.Atom(pred, den, polarity, fixity, terms, tpe, loc) =>
      val ts = terms.map(visitPattern)
      TypedAst.Predicate.Body.Atom(pred, den, polarity, fixity, ts, visitType(tpe), loc)

    case InferredAst.Predicate.Body.Functional(outBnds, exp, loc) =>
      val e = visitExp(exp)
      TypedAst.Predicate.Body.Functional(outBnds.map(visitBinder), e, loc)

    case InferredAst.Predicate.Body.Guard(exp, loc) =>
      val e = visitExp(exp)
      TypedAst.Predicate.Body.Guard(e, loc)

  }

  /**
    * Applies the substitution to the given type `tpe0`.
    *
    * Simplifies the effect formulas of `tpe0` as they are rebuilt.
    */
  private def visitType(tpe0: Type)(implicit subst: SubstitutionTree): Type = tpe0 match {
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Union, _), tpe1, _), tpe2, loc) =>
      Type.mkUnion(visitType(tpe1), visitType(tpe2), loc)

    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Intersection, _), tpe1, _), tpe2, loc) =>
      Type.mkIntersection(visitType(tpe1), visitType(tpe2), loc)

    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Difference, _), tpe1, _), tpe2, loc) =>
      Type.mkDifference(visitType(tpe1), visitType(tpe2), loc)

    case Type.Apply(Type.Cst(TypeConstructor.Complement, _), tpe, loc) =>
      Type.mkComplement(visitType(tpe), loc)

    case app@Type.Apply(tpe1, tpe2, loc) =>
      app.renew(visitType(tpe1), visitType(tpe2), loc)

    case _ => subst(tpe0)
  }

  /**
    * Reconstructs types in the given binder.
    */
  private def visitBinder(bnd: InferredAst.Binder)(implicit subst: SubstitutionTree): TypedAst.Binder = bnd match {
    case InferredAst.Binder(sym, tpe) => TypedAst.Binder(sym, visitType(tpe))
  }

  /**
    * Reconstructs types in the given fparam.
    */
  private def visitFormalParam(fparam: InferredAst.FormalParam)(implicit subst: SubstitutionTree): TypedAst.FormalParam = fparam match {
    case InferredAst.FormalParam(bnd, tpe, src, loc) =>
      TypedAst.FormalParam(visitBinder(bnd), visitType(tpe), src, Decreasing.NonDecreasing, loc)
  }
}
