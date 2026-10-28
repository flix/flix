/*
 * Copyright 2015-2023 Magnus Madsen, Matthew Lutze
 * Copyright 2024 Alexander Dybdahl Troelsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.phase.typer

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.KindedAst.{Expr, ExtPattern, ExtTagPattern}
import ca.uwaterloo.flix.language.ast.jvm.JavaField
import ca.uwaterloo.flix.language.ast.shared.{CheckedCastType, RegionScope, VarText}
import ca.uwaterloo.flix.language.ast.{InferredAst, Kind, KindedAst, Name, Scheme, SemanticOp, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.phase.typer.jvm.JavaTypes
import ca.uwaterloo.flix.language.phase.unification.Substitution
import ca.uwaterloo.flix.util.collection.{ListOps, Nel}
import ca.uwaterloo.flix.util.InternalCompilerException

import scala.annotation.tailrec


/**
  * This phase generates a list of type constraints, which include
  *   - equality constraints `tpe1 ~ tpe2`
  *   - trait constraints `C[tpe1]`
  *   - purification constraints `eff1 ~ eff2[sym ↦ Pure]`
  *
  * We gather constraints as we traverse each def.
  * Constraints are later resolved in ConstraintResolution.
  */
object ConstraintGen {

  /**
    * Generates constraints for the given expression `exp0`, adding them to the type context `c`.
    *
    * Returns the expression annotated with its type and effect.
    *
    * The annotated types and effects, on the returned node and on every node below it, are generally
    * not fully resolved: they may be, or may contain, type variables. That is the point of inference:
    * this function only records the constraints on those variables in `c`, and it is the constraint
    * solver that later determines what they stand for. The tree is then reconstructed with the solution.
    */
  def visitExp(exp0: KindedAst.Expr)(implicit c: TypeContext, root: KindedAst.Root, flix: Flix): InferredAst.Expr = {
    implicit val scope: RegionScope = c.getScope
    exp0 match {
      case Expr.Var(sym, loc) =>
        val resTpe = sym.tvar
        InferredAst.Expr.Var(sym, resTpe, loc)

      case Expr.Hole(sym, scp, tpe, eff, loc) =>
        val resTpe = tpe
        val resEff = eff
        InferredAst.Expr.Hole(sym, scp, resTpe, resEff, loc)

      case Expr.HoleWithExp(exp, scp, tvar, evar, loc) =>
        // We ignore exp's type and allow the hole to have any type.
        // The effect of the hole may be any effect that includes the effect of exp.
        val e = visitExp(exp)
        c.subEff(e.eff, evar, loc)
        val resTpe = tvar
        val resEff = evar
        InferredAst.Expr.HoleWithExp(e, scp, resTpe, resEff, loc)

      case e: Expr.OpenAs => RestrictableChooseConstraintGen.visitOpenAs(e)

      case Expr.Use(sym, alias, exp, loc) =>
        val e = visitExp(exp)
        InferredAst.Expr.Use(sym, alias, e, loc)

      case Expr.Cst(cst, loc) =>
        val resTpe = Type.constantType(cst)
        InferredAst.Expr.Cst(cst, resTpe, loc)

      case Expr.ApplyClo(exp1, exp2, loc) =>
        val tvar = freshVar(Kind.Star, loc)
        val evar = freshVar(Kind.Eff, loc)
        val lambdaBodyType = freshVar(Kind.Star, loc)
        val lambdaBodyEff = freshVar(Kind.Eff, loc)
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        c.expectType(Type.mkArrowWithEffect(e2.tpe, lambdaBodyEff, lambdaBodyType, loc), e1.tpe, loc)
        c.unifyType(tvar, lambdaBodyType, loc)
        c.unifyType(evar, Type.mkUnion(lambdaBodyEff :: e1.eff :: e2.eff :: Nil, loc), loc)
        val resTpe = tvar
        val resEff = evar
        InferredAst.Expr.ApplyClo(e1, e2, resTpe, resEff, loc)

      case Expr.ApplyDef(symUse, exps, loc) =>
        val defn = root.defs(symUse.sym)

        val tparams = defn.spec.tparams.map(_.sym)
        val targs = tparams.map(sym => Type.freshVar(sym.kind, sym.loc))
        val itvar = freshVar(Kind.Star, symUse.loc)
        val tvar = freshVar(Kind.Star, loc)
        val evar = freshVar(Kind.Eff, loc)
        val subst = Substitution(tparams.zip(targs).toMap)

        val tconstrs = defn.spec.tconstrs.map(subst.apply).map(_.copy(loc = loc))
        val econstrs = defn.spec.econstrs.map(subst.apply).map(_.copy(loc = loc))

        // If no effect specified, we assume the function is pure
        val declaredEff = subst(defn.spec.eff.getOrElse(Type.Pure))
        val declaredResultType = generalizeVoid(subst(defn.spec.tpe))
        val declaredArgumentTypes = defn.spec.fparams.map(_.tpe).map(subst.apply)
        val declaredType = Type.mkUncurriedArrowWithEffect(declaredArgumentTypes, declaredEff, declaredResultType, loc)

        val es = exps.map(visitExp)
        val tpes = es.map(_.tpe)
        val effs = es.map(_.eff)

        c.unifyType(itvar, declaredType, loc)
        c.expectTypeArguments(symUse.sym, declaredArgumentTypes.toList, tpes, exps.map(_.loc))
        c.addClassConstraints(tconstrs, loc)
        c.addEqualityConstraints(econstrs, loc)
        c.unifyType(tvar, declaredResultType, loc)
        val eff = if (declaredEff == Type.Pure) {
          // A pure def is not the source of an effect.
          Type.mkUnion(effs, loc)
        } else {
          // Pseudo variable for source to flow into
          val pvar = Type.freshVar(Kind.Eff, symUse.loc)
          c.unifySource(pvar, declaredEff, loc)
          Type.mkUnion(pvar :: effs, loc)
        }
        c.unifyType(evar, eff, loc)
        val resTpe = tvar
        val resEff = evar
        InferredAst.Expr.ApplyDef(symUse, es, targs, itvar, resTpe, resEff, loc)

      case Expr.ApplyLocalDef(symUse, exps, loc) =>
        val arrowTvar = freshVar(Kind.Star, loc)
        val tvar = freshVar(Kind.Star, loc)
        val evar = freshVar(Kind.Eff, loc)
        val es = exps.map(visitExp)
        val tpes = es.map(_.tpe)
        val effs = es.map(_.eff)
        val defEff = freshVar(Kind.Eff, symUse.loc)
        val actualDefTpe = Type.mkUncurriedArrowWithEffect(Nel.unsafeFrom(tpes), defEff, tvar, symUse.loc)
        c.unifyType(actualDefTpe, arrowTvar, symUse.loc)
        c.expectType(symUse.sym.tvar, actualDefTpe, symUse.loc)
        c.unifyType(evar, Type.mkUnion(defEff :: effs, loc), loc)
        val resTpe = tvar
        val resEff = evar
        InferredAst.Expr.ApplyLocalDef(symUse, es, arrowTvar, resTpe, resEff, loc)

      case Expr.ApplyOp(symUse, exps, loc) =>
        val tvar = Type.freshVar(Kind.Star, loc)
        val evar = Type.freshVar(Kind.Eff, loc)
        val op = lookupOp(symUse.sym, symUse.loc)
        val (tconstrs0, econstrs0, declaredType, _) = Scheme.instantiate(op.spec.sc, symUse.loc.asSynthetic)
        val tconstrs = tconstrs0.map(_.copy(loc = loc))
        val econstrs = econstrs0.map(_.copy(loc = loc))
        val declaredEff = declaredType.arrowEffectType
        val declaredArgumentTypes = declaredType.arrowArgTypes
        val declaredResultType = generalizeVoid(declaredType.arrowResultType)
        val es = exps.map(visitExp)
        val tpes = es.map(_.tpe)
        val effs = es.map(_.eff)
        c.expectTypeArguments(symUse.sym, declaredArgumentTypes, tpes, exps.map(_.loc))
        c.addClassConstraints(tconstrs, loc)
        c.addEqualityConstraints(econstrs, loc)
        c.unifyType(tvar, declaredResultType, loc)
        c.unifyType(evar, Type.mkUnion(declaredEff :: effs, loc), loc)
        val resTpe = tvar
        val resEff = evar
        InferredAst.Expr.ApplyOp(symUse, es, resTpe, resEff, loc)

      case Expr.ApplySig(symUse, exps, loc) =>
        val trt = root.traits(symUse.sym.trt)
        val sig = trt.sigs(symUse.sym)
        val targ = freshVar(trt.tparam.sym.kind, symUse.loc)
        val targs = sig.spec.tparams.map(tparam => Type.freshVar(tparam.sym.kind, tparam.sym.loc))
        val itvar = freshVar(Kind.Star, symUse.loc)
        val tvar = freshVar(Kind.Star, loc)
        val evar = freshVar(Kind.Eff, loc)

        val mapping = (trt.tparam.sym -> targ) :: sig.spec.tparams.map(_.sym).zip(targs)
        val subst = Substitution(mapping.toMap)
        val tconstrs = sig.spec.tconstrs.map(subst.apply).map(_.copy(loc = loc))
        val econstrs = sig.spec.econstrs.map(subst.apply).map(_.copy(loc = loc))

        val declaredEff = subst(sig.spec.eff.getOrElse(Type.Pure))
        val declaredResultType = subst(sig.spec.tpe)
        val declaredArgumentTypes = sig.spec.fparams.map(_.tpe).map(subst.apply)
        val declaredType = Type.mkUncurriedArrowWithEffect(declaredArgumentTypes, declaredEff, declaredResultType, symUse.loc)

        val es = exps.map(visitExp)
        val tpes = es.map(_.tpe)
        val effs = es.map(_.eff)
        c.expectTypeArguments(symUse.sym, declaredArgumentTypes.toList, tpes, exps.map(_.loc))
        c.addClassConstraints(tconstrs, loc)
        c.addEqualityConstraints(econstrs, loc)
        c.unifyType(itvar, declaredType, loc)
        c.unifyType(tvar, declaredResultType, loc)
        c.unifyType(evar, Type.mkUnion(declaredEff :: effs, loc), loc)
        val resTpe = tvar
        val resEff = evar
        InferredAst.Expr.ApplySig(symUse, es, targ, targs, itvar, resTpe, resEff, loc)

      case Expr.Lambda(fparam, exp, loc) =>
        c.unifyType(fparam.sym.tvar, fparam.tpe, loc)
        val e = visitExp(exp)
        // Sub-effecting: the lambda is given a fresh effect variable with the effect of its body as a lower bound.
        // Hence the lambda can be used where a larger effect is expected, e.g. a pure lambda where IO is required.
        // A lambda whose body is itself a lambda has the exact effect Pure: the bound Pure ⊑ x would hold trivially
        // and a curried lambda would otherwise introduce a useless effect variable for every inner lambda.
        val bound = if (isCurriedLambdaBody(exp)) EffectBound.Exact else EffectBound.Upper
        val eff = mkBoundedEff(bound, e.eff, loc)
        val fp = visitFormalParam(fparam)
        val resTpe = Type.mkArrowWithEffect(fparam.tpe, eff, e.tpe, loc)
        InferredAst.Expr.Lambda(fp, e, resTpe, loc)

      case KindedAst.Expr.Unary(sop, exp, loc) =>
        val tvar = freshVar(Kind.Star, loc)
        sop match {
        case SemanticOp.BoolOp.Not =>
          val e = visitExp(exp)
          c.expectType(expected = Type.Bool, actual = e.tpe, exp.loc)
          c.unifyType(Type.Bool, tvar, exp.loc)
          val resTpe = tvar
          val resEff = e.eff
          InferredAst.Expr.Unary(sop, e, resTpe, resEff, loc)

        case SemanticOp.Float32Op.Neg =>
          val e = visitExp(exp)
          c.expectType(expected = Type.Float32, actual = e.tpe, exp.loc)
          c.unifyType(Type.Float32, tvar, exp.loc)
          val resTpe = tvar
          val resEff = e.eff
          InferredAst.Expr.Unary(sop, e, resTpe, resEff, loc)

        case SemanticOp.Float64Op.Neg =>
          val e = visitExp(exp)
          c.expectType(expected = Type.Float64, actual = e.tpe, exp.loc)
          c.unifyType(Type.Float64, tvar, exp.loc)
          val resTpe = tvar
          val resEff = e.eff
          InferredAst.Expr.Unary(sop, e, resTpe, resEff, loc)

        case SemanticOp.Int8Op.Neg | SemanticOp.Int8Op.Not =>
          val e = visitExp(exp)
          c.expectType(expected = Type.Int8, actual = e.tpe, exp.loc)
          c.unifyType(Type.Int8, tvar, exp.loc)
          val resTpe = tvar
          val resEff = e.eff
          InferredAst.Expr.Unary(sop, e, resTpe, resEff, loc)

        case SemanticOp.Int16Op.Neg | SemanticOp.Int16Op.Not =>
          val e = visitExp(exp)
          c.expectType(expected = Type.Int16, actual = e.tpe, exp.loc)
          c.unifyType(Type.Int16, tvar, exp.loc)
          val resTpe = tvar
          val resEff = e.eff
          InferredAst.Expr.Unary(sop, e, resTpe, resEff, loc)

        case SemanticOp.Int32Op.Neg | SemanticOp.Int32Op.Not =>
          val e = visitExp(exp)
          c.expectType(expected = Type.Int32, actual = e.tpe, exp.loc)
          c.unifyType(Type.Int32, tvar, exp.loc)
          val resTpe = tvar
          val resEff = e.eff
          InferredAst.Expr.Unary(sop, e, resTpe, resEff, loc)

        case SemanticOp.Int64Op.Neg | SemanticOp.Int64Op.Not =>
          val e = visitExp(exp)
          c.expectType(expected = Type.Int64, actual = e.tpe, exp.loc)
          c.unifyType(Type.Int64, tvar, exp.loc)
          val resTpe = tvar
          val resEff = e.eff
          InferredAst.Expr.Unary(sop, e, resTpe, resEff, loc)

        case SemanticOp.ReflectOp.ReflectEff =>
          val effVar = freshVar(Kind.Eff, exp.loc)
          val e = visitExp(exp)
          val proxyEffSym = Symbol.mkEnumSym("ProxyEff")
          val proxyEffType = Type.mkEnum(proxyEffSym, List(effVar), exp.loc)
          c.expectType(expected = proxyEffType, actual = e.tpe, exp.loc)
          val puritySym = Symbol.mkEnumSym("Reflect.Purity")
          val purityType = Type.mkEnum(puritySym, Nil, exp.loc)
          c.unifyType(purityType, tvar, exp.loc)
          val resTpe = tvar
          val resEff = e.eff
          InferredAst.Expr.Unary(sop, e, resTpe, resEff, loc)

        case SemanticOp.ReflectOp.ReflectType =>
          val elmVar = freshVar(Kind.Star, exp.loc)
          val e = visitExp(exp)
          val proxySym = Symbol.mkEnumSym("Proxy")
          val proxyType = Type.mkEnum(proxySym, List(elmVar), exp.loc)
          c.expectType(expected = proxyType, actual = e.tpe, exp.loc)
          val jvmTypeSym = Symbol.mkEnumSym("Reflect.JvmType")
          val jvmTypeType = Type.mkEnum(jvmTypeSym, Nil, exp.loc)
          c.unifyType(jvmTypeType, tvar, exp.loc)
          val resTpe = tvar
          val resEff = e.eff
          InferredAst.Expr.Unary(sop, e, resTpe, resEff, loc)

        case SemanticOp.ReflectOp.ReflectValue =>
          val e = visitExp(exp)
          val jvmValueSym = Symbol.mkEnumSym("Reflect.JvmValue")
          val jvmValueType = Type.mkEnum(jvmValueSym, Nil, exp.loc)
          c.unifyType(jvmValueType, tvar, exp.loc)
          val resTpe = tvar
          val resEff = e.eff
          InferredAst.Expr.Unary(sop, e, resTpe, resEff, loc)

        case SemanticOp.ObjectOp.Ordinal =>
          val e = visitExp(exp)
          c.unifyType(tvar, Type.Int32, exp.loc)
          val resTpe = tvar
          val resEff = e.eff
          InferredAst.Expr.Unary(sop, e, resTpe, resEff, loc)

        }

      case KindedAst.Expr.Binary(sop, exp1, exp2, loc) =>
        val tvar = freshVar(Kind.Star, loc)
        sop match {

        case SemanticOp.BoolOp.And | SemanticOp.BoolOp.Or =>
          val e1 = visitExp(exp1)
          val e2 = visitExp(exp2)
          c.expectType(expected = Type.Bool, actual = e1.tpe, exp1.loc)
          c.expectType(expected = Type.Bool, actual = e2.tpe, exp2.loc)
          c.unifyType(tvar, Type.Bool, loc)
          val resTpe = tvar
          val resEff = Type.mkUnion(e1.eff, e2.eff, loc)
          InferredAst.Expr.Binary(sop, e1, e2, resTpe, resEff, loc)

        case SemanticOp.Float32Op.Add | SemanticOp.Float32Op.Sub | SemanticOp.Float32Op.Mul | SemanticOp.Float32Op.Div
             | SemanticOp.Float32Op.Exp =>
          val e1 = visitExp(exp1)
          val e2 = visitExp(exp2)
          c.expectType(expected = Type.Float32, actual = e1.tpe, exp1.loc)
          c.expectType(expected = Type.Float32, actual = e2.tpe, exp2.loc)
          c.unifyType(tvar, Type.Float32, loc)
          val resTpe = tvar
          val resEff = Type.mkUnion(e1.eff, e2.eff, loc)
          InferredAst.Expr.Binary(sop, e1, e2, resTpe, resEff, loc)

        case SemanticOp.Float64Op.Add | SemanticOp.Float64Op.Sub | SemanticOp.Float64Op.Mul | SemanticOp.Float64Op.Div
             | SemanticOp.Float64Op.Exp =>
          val e1 = visitExp(exp1)
          val e2 = visitExp(exp2)
          c.expectType(expected = Type.Float64, actual = e1.tpe, exp1.loc)
          c.expectType(expected = Type.Float64, actual = e2.tpe, exp2.loc)
          c.unifyType(tvar, Type.Float64, loc)
          val resTpe = tvar
          val resEff = Type.mkUnion(e1.eff, e2.eff, loc)
          InferredAst.Expr.Binary(sop, e1, e2, resTpe, resEff, loc)

        case SemanticOp.Int8Op.Add | SemanticOp.Int8Op.Sub | SemanticOp.Int8Op.Mul | SemanticOp.Int8Op.Div
             | SemanticOp.Int8Op.Rem | SemanticOp.Int8Op.Exp
             | SemanticOp.Int8Op.And | SemanticOp.Int8Op.Or | SemanticOp.Int8Op.Xor =>
          val e1 = visitExp(exp1)
          val e2 = visitExp(exp2)
          c.expectType(expected = Type.Int8, actual = e1.tpe, exp1.loc)
          c.expectType(expected = Type.Int8, actual = e2.tpe, exp2.loc)
          c.unifyType(tvar, Type.Int8, loc)
          val resTpe = tvar
          val resEff = Type.mkUnion(e1.eff, e2.eff, loc)
          InferredAst.Expr.Binary(sop, e1, e2, resTpe, resEff, loc)

        case SemanticOp.Int16Op.Add | SemanticOp.Int16Op.Sub | SemanticOp.Int16Op.Mul | SemanticOp.Int16Op.Div
             | SemanticOp.Int16Op.Rem | SemanticOp.Int16Op.Exp
             | SemanticOp.Int16Op.And | SemanticOp.Int16Op.Or | SemanticOp.Int16Op.Xor =>
          val e1 = visitExp(exp1)
          val e2 = visitExp(exp2)
          c.expectType(expected = Type.Int16, actual = e1.tpe, exp1.loc)
          c.expectType(expected = Type.Int16, actual = e2.tpe, exp2.loc)
          c.unifyType(tvar, Type.Int16, loc)
          val resTpe = tvar
          val resEff = Type.mkUnion(e1.eff, e2.eff, loc)
          InferredAst.Expr.Binary(sop, e1, e2, resTpe, resEff, loc)

        case SemanticOp.Int32Op.Add | SemanticOp.Int32Op.Sub | SemanticOp.Int32Op.Mul | SemanticOp.Int32Op.Div
             | SemanticOp.Int32Op.Rem | SemanticOp.Int32Op.Exp
             | SemanticOp.Int32Op.And | SemanticOp.Int32Op.Or | SemanticOp.Int32Op.Xor =>
          val e1 = visitExp(exp1)
          val e2 = visitExp(exp2)
          c.expectType(expected = Type.Int32, actual = e1.tpe, exp1.loc)
          c.expectType(expected = Type.Int32, actual = e2.tpe, exp2.loc)
          c.unifyType(tvar, Type.Int32, loc)
          val resTpe = tvar
          val resEff = Type.mkUnion(e1.eff, e2.eff, loc)
          InferredAst.Expr.Binary(sop, e1, e2, resTpe, resEff, loc)

        case SemanticOp.Int64Op.Add | SemanticOp.Int64Op.Sub | SemanticOp.Int64Op.Mul | SemanticOp.Int64Op.Div
             | SemanticOp.Int64Op.Rem | SemanticOp.Int64Op.Exp
             | SemanticOp.Int64Op.And | SemanticOp.Int64Op.Or | SemanticOp.Int64Op.Xor =>
          val e1 = visitExp(exp1)
          val e2 = visitExp(exp2)
          c.expectType(expected = Type.Int64, actual = e1.tpe, exp1.loc)
          c.expectType(expected = Type.Int64, actual = e2.tpe, exp2.loc)
          c.unifyType(tvar, Type.Int64, loc)
          val resTpe = tvar
          val resEff = Type.mkUnion(e1.eff, e2.eff, loc)
          InferredAst.Expr.Binary(sop, e1, e2, resTpe, resEff, loc)

        case SemanticOp.Int8Op.Shl | SemanticOp.Int8Op.Shr
             | SemanticOp.Int16Op.Shl | SemanticOp.Int16Op.Shr
             | SemanticOp.Int32Op.Shl | SemanticOp.Int32Op.Shr
             | SemanticOp.Int64Op.Shl | SemanticOp.Int64Op.Shr =>
          val e1 = visitExp(exp1)
          val e2 = visitExp(exp2)
          c.unifyType(tvar, e1.tpe, loc)
          c.expectType(expected = Type.Int32, actual = e2.tpe, exp2.loc)
          val resTpe = tvar
          val resEff = Type.mkUnion(e1.eff, e2.eff, loc)
          InferredAst.Expr.Binary(sop, e1, e2, resTpe, resEff, loc)

        case SemanticOp.BoolOp.Eq | SemanticOp.BoolOp.Neq
             | SemanticOp.CharOp.Eq | SemanticOp.CharOp.Neq
             | SemanticOp.Float32Op.Eq | SemanticOp.Float32Op.Neq
             | SemanticOp.Float64Op.Eq | SemanticOp.Float64Op.Neq
             | SemanticOp.Int8Op.Eq | SemanticOp.Int8Op.Neq
             | SemanticOp.Int16Op.Eq | SemanticOp.Int16Op.Neq
             | SemanticOp.Int32Op.Eq | SemanticOp.Int32Op.Neq
             | SemanticOp.Int64Op.Eq | SemanticOp.Int64Op.Neq =>
          val e1 = visitExp(exp1)
          val e2 = visitExp(exp2)
          c.unifyType(e1.tpe, e2.tpe, loc)
          c.unifyType(tvar, Type.Bool, loc)
          val resTpe = tvar
          val resEff = Type.mkUnion(e1.eff, e2.eff, loc)
          InferredAst.Expr.Binary(sop, e1, e2, resTpe, resEff, loc)

        case SemanticOp.CharOp.Lt | SemanticOp.CharOp.Le | SemanticOp.CharOp.Gt | SemanticOp.CharOp.Ge
             | SemanticOp.Float32Op.Lt | SemanticOp.Float32Op.Le | SemanticOp.Float32Op.Gt | SemanticOp.Float32Op.Ge
             | SemanticOp.Float64Op.Lt | SemanticOp.Float64Op.Le | SemanticOp.Float64Op.Gt | SemanticOp.Float64Op.Ge
             | SemanticOp.Int8Op.Lt | SemanticOp.Int8Op.Le | SemanticOp.Int8Op.Gt | SemanticOp.Int8Op.Ge
             | SemanticOp.Int16Op.Lt | SemanticOp.Int16Op.Le | SemanticOp.Int16Op.Gt | SemanticOp.Int16Op.Ge
             | SemanticOp.Int32Op.Lt | SemanticOp.Int32Op.Le | SemanticOp.Int32Op.Gt | SemanticOp.Int32Op.Ge
             | SemanticOp.Int64Op.Lt | SemanticOp.Int64Op.Le | SemanticOp.Int64Op.Gt | SemanticOp.Int64Op.Ge =>
          val e1 = visitExp(exp1)
          val e2 = visitExp(exp2)
          c.unifyType(e1.tpe, e2.tpe, loc)
          c.unifyType(tvar, Type.Bool, loc)
          val resTpe = tvar
          val resEff = Type.mkUnion(e1.eff, e2.eff, loc)
          InferredAst.Expr.Binary(sop, e1, e2, resTpe, resEff, loc)

        case SemanticOp.StringOp.Concat =>
          val e1 = visitExp(exp1)
          val e2 = visitExp(exp2)
          c.expectType(expected = Type.Str, actual = e1.tpe, exp1.loc)
          c.expectType(expected = Type.Str, actual = e2.tpe, exp2.loc)
          c.unifyType(tvar, Type.Str, loc)
          val resTpe = tvar
          val resEff = Type.mkUnion(e1.eff, e2.eff, loc)
          InferredAst.Expr.Binary(sop, e1, e2, resTpe, resEff, loc)

        case SemanticOp.ObjectOp.RefEq =>
          val e1 = visitExp(exp1)
          val e2 = visitExp(exp2)
          c.unifyType(e1.tpe, e2.tpe, loc)
          c.unifyType(tvar, Type.Bool, loc)
          val resTpe = tvar
          val resEff = Type.mkUnion(e1.eff, e2.eff, loc)
          InferredAst.Expr.Binary(sop, e1, e2, resTpe, resEff, loc)
        }

      case Expr.IfThenElse(exp1, exp2, exp3, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        val e3 = visitExp(exp3)
        c.expectType(expected = Type.Bool, actual = e1.tpe, exp1.loc)
        c.unifyType(e2.tpe, e3.tpe, loc)
        val resTpe = e3.tpe
        val resEff = Type.mkUnion(e1.eff, e2.eff, e3.eff, loc)
        InferredAst.Expr.IfThenElse(e1, e2, e3, resTpe, resEff, loc)

      case Expr.Stm(exps, exp, loc) =>
        val es = exps.map(visitExp)
        val tpes = es.map(_.tpe)
        val effs = es.map(_.eff)
        val e2 = visitExp(exp)

        // JVM invocations may return non-Unit; don't require Unit for those.
        exps.zip(tpes).foreach { case (e, tpe1) =>
          if (!isJvmInvoke(e)) {
            c.expectStmt(actual = tpe1, e.loc)
          }
        }

        val resTpe = e2.tpe
        val resEff = Type.mkUnion(e2.eff :: effs, loc)
        InferredAst.Expr.Stm(es, e2, resTpe, resEff, loc)

      case Expr.Discard(exp, loc) =>
        val e = visitExp(exp)
        val resEff = e.eff
        InferredAst.Expr.Discard(e, resEff, loc)

      case Expr.Let(sym, exp1, exp2, loc) =>
        val e1 = visitExp(exp1)
        c.unifyType(sym.tvar, e1.tpe, exp1.loc)
        val e2 = visitExp(exp2)
        val bnd = InferredAst.Binder(sym, e1.tpe)
        val resTpe = e2.tpe
        val resEff = Type.mkUnion(e1.eff, e2.eff, loc)
        InferredAst.Expr.Let(bnd, e1, e2, resTpe, resEff, loc)

      case Expr.LocalDef(ann, sym, fparams, exp1, exp2, loc) =>
        val e1 = visitExp(exp1)
        fparams.foreach(fp => c.unifyType(fp.sym.tvar, fp.tpe, loc))
        // Sub-effecting: the local def is given a fresh effect variable with the effect of its body as a lower bound.
        // Hence the local def can be used where a larger effect is expected, e.g. a pure def where IO is required.
        // A recursive call has the effect of the local def itself, so the effect variable may occur in its own bound.
        val defEff = mkBoundedEff(EffectBound.Upper, e1.eff, loc)
        val defTpe = Type.mkUncurriedArrowWithEffect(fparams.map(_.tpe), defEff, e1.tpe, sym.loc)
        c.unifyType(sym.tvar, defTpe, sym.loc)
        val e2 = visitExp(exp2)
        val bnd = InferredAst.Binder(sym, defTpe)
        val fps = fparams.map(visitFormalParam)
        val resTpe = e2.tpe
        val resEff = e2.eff
        InferredAst.Expr.LocalDef(ann, bnd, fps, e1, e2, resTpe, resEff, loc)

      case Expr.Region(sym, regSym, exp, tvar, evar, loc) =>
        // We must visit exp INSIDE the region
        // (i.e. between `enter` and `exit`)
        // because we need to resolve local constraints
        // BEFORE purifying the region as we exit,
        // and we need to be sure that we don't "learn" anything
        // about the outside of the region while inside.
        //
        // We must unify sym.tvar and the region var INSIDE the region
        // because we need to ensure that references to the region are
        // resolved BEFORE purifying the region as we exit.
        c.enterRegion(regSym)
        c.unifyType(sym.tvar, Type.mkRegionToStar(Type.mkRegion(regSym, loc), loc), loc)
        val e = visitExp(exp)
        c.unifyType(tvar, e.tpe, loc)
        c.exitRegion(evar, e.eff, loc)
        val bnd = InferredAst.Binder(sym, evar)
        val resTpe = tvar
        val resEff = evar
        InferredAst.Expr.Region(bnd, regSym, e, resTpe, resEff, loc)

      case Expr.Match(exp, rules, loc) =>
        val e = visitExp(exp)
        val rs = rules.map(visitMatchRule)
        val patTpes = rs.map(_.pat.tpe)
        val tpes = rs.map(_.exp.tpe)
        val effs = rs.map(_.exp.eff)
        c.unifyAllTypes(e.tpe :: patTpes, loc)
        c.unifyAllTypes(tpes, loc)
        val resTpe = tpes.headOption.getOrElse(freshVar(Kind.Star, loc))
        val resEff = Type.mkUnion(e.eff :: effs, loc)
        InferredAst.Expr.Match(e, rs, resTpe, resEff, loc)

      case e: Expr.RestrictableChoose => RestrictableChooseConstraintGen.visitRestrictableChoose(e)

      case Expr.ExtMatch(exp, rules, loc) =>
        // Note that x_i_m mean last variable term `x` of tag `i`. Think of `m` as shorthand for `max` or `last`.
        // Thus, it may be the case that `|x_i_1, ..., x_i_m| != |x_j_1, ..., x_j_m|` but equality may occur.
        // This also means `t_i_k != t_j_k` can happen and that equality can also occur.
        //
        // Gamma |- x_i_j : t_i_j
        // Gamma, x_i_1 : t_i_1, ..., x_i_m : t_i_m |- exp_i : t // This is the result type
        // Gamma |- exp_0 : #| Tag_1(t_1_1, ..., t_1_m), ..., Tag_n(t_n_1, ..., t_n_m) |#
        // ef = Union(effs(exp_0), effs(exp_1), ..., effs(exp_n))
        // -----------------------------------------------
        // Gamma |- ematch exp_0 {
        //              case Tag_1(x_1_1, ..., x_1_m) => exp_1
        //              ...
        //              case Tag_n(x_n_1, ..., x_n_m) => exp_n
        //           } : t \ ef
        //
        // Lastly, if `case _ => exp_default` occurs (or an error pattern), then
        //   Gamma |- exp_default : t                // This is just a special case of the second rule from the top.
        // and the type of exp_0 is updated to have a fresh row variable, i.e., for fresh variable `r`:
        //   Gamma |- exp_0 : #| Tag_1(t_1_1, ..., t_1_m), ..., Tag_n(t_n_1, ..., t_n_m) | r |#
        //
        val e = visitExp(exp)
        val (rs, patTypes) = rules.map(visitExtMatchRule).unzip
        val ruleBodyTypes = rs.map(_.exp.tpe)
        val ruleBodyEffs = rs.map(_.exp.eff)
        val tagPatTypes = patTypes.collect { case Left(tag) => tag }
        val defaultPatternTvars = patTypes.collect { case Right(tvar) => tvar }.map { tvar => Type.mkExtensible(tvar, tvar.loc) }
        val defaultSchemaRow = // Note: An empty list of patterns cannot occur and errors are treated as default cases.
          if (defaultPatternTvars.isEmpty) // Implies that Tag pattern is present
            Type.mkSchemaRowEmpty(loc.asSynthetic)
          else
            freshVar(Kind.SchemaRow, loc.asSynthetic)
        val expectedRowType =
          if (tagPatTypes.isEmpty) // Implies that error or default case is present
            freshVar(Kind.SchemaRow, loc.asSynthetic)
          else
            tagPatTypes
              .foldRight(defaultSchemaRow) {
                case ((pred, tpes), acc) =>
                  val relation = Type.mkRelation(tpes, pred.loc.asSynthetic)
                  Type.mkSchemaRowExtend(pred, relation, acc, pred.loc.asSynthetic)
              }
        val expectedExtensibleType = Type.mkExtensible(expectedRowType, loc.asSynthetic)
        c.unifyAllTypes(e.tpe :: expectedExtensibleType :: defaultPatternTvars, loc)
        c.unifyAllTypes(ruleBodyTypes, loc)
        val resTpe = ruleBodyTypes.head // Note: We are guaranteed to have one rule.
        val resEff = Type.mkUnion(e.eff :: ruleBodyEffs, loc)
        InferredAst.Expr.ExtMatch(e, rs, resTpe, resEff, loc)

      case KindedAst.Expr.Tag(symUse, exps, loc) =>
        val tvar = freshVar(Kind.Star, loc)
        val decl = root.enums(symUse.sym.enumSym)
        val caze = decl.cases(symUse.sym)
        // We ignore constraints as tag schemes do not have them
        val (_, _, tagType, _) = Scheme.instantiate(caze.sc, loc.asSynthetic)

        // The tag type is a function from the types of terms to the type of the enum.
        val es = exps.map(visitExp)
        val tpes = es.map(_.tpe)
        val effs = es.map(_.eff)
        // A nullary tag is not a function, but the enum type itself.
        val constructorBase = tpes match {
          case Nil => tvar
          case t :: ts => Type.mkPureUncurriedArrow(Nel(t, ts), tvar, loc)
        }
        c.unifyType(tagType, constructorBase, loc)
        val resTpe = tvar
        val resEff = Type.mkUnion(effs, loc)
        InferredAst.Expr.Tag(symUse, es, resTpe, resEff, loc)

      case e: Expr.RestrictableTag => RestrictableChooseConstraintGen.visitApplyRestrictableTag(e)

      case KindedAst.Expr.ExtTag(label, exps, loc) =>
        val tvar = freshVar(Kind.Star, loc)
        val pred = Name.Pred(label.name, label.loc)
        val es = exps.map(visitExp)
        val tpes = es.map(_.tpe)
        val effs = es.map(_.eff)
        val rest = Type.freshVar(Kind.SchemaRow, loc)
        val tpe = Type.mkRelation(tpes, loc)
        val row = Type.mkSchemaRowExtend(pred, tpe, rest, loc)
        val tagType = Type.mkExtensible(row, loc)
        c.unifyType(tvar, tagType, loc)
        val resTpe = tagType
        val resEff = Type.mkUnion(effs, loc)
        InferredAst.Expr.ExtTag(label, es, resTpe, resEff, loc)

      case Expr.Tuple(exps, loc) =>
        val es = exps.map(visitExp)
        val resTpe = Type.mkTuple(es.map(_.tpe), loc)
        val resEff = Type.mkUnion(es.map(_.eff), loc)
        InferredAst.Expr.Tuple(es, resTpe, resEff, loc)

      case Expr.RecordSelect(exp, label, loc) =>
        //
        // r : { label = tpe | row }
        // -------------------------
        //       r.label : tpe
        //
        val tvar = freshVar(Kind.Star, loc)
        val freshRowVar = freshVar(Kind.RecordRow, loc)
        val expectedRowType = Type.mkRecordRowExtend(label, tvar, freshRowVar, loc)
        val expectedRecordType = Type.mkRecord(expectedRowType, loc)
        val e = visitExp(exp)
        c.unifyType(e.tpe, expectedRecordType, loc)
        val resTpe = tvar
        val resEff = e.eff
        InferredAst.Expr.RecordSelect(e, label, resTpe, resEff, loc)

      case Expr.RecordExtend(label, exp1, exp2, loc) =>
        //
        //       exp1 : tpe        exp2 : {| r }
        // ---------------------------------------------
        // { label = exp1 | exp2 } : { label  :: tpe | r }
        //
        val tvar = freshVar(Kind.Star, loc)
        val freshRowVar = freshVar(Kind.RecordRow, loc)
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        c.unifyType(e2.tpe, Type.mkRecord(freshRowVar, loc), loc)
        c.unifyType(tvar, Type.mkRecord(Type.mkRecordRowExtend(label, e1.tpe, freshRowVar, loc), loc), loc)
        val resTpe = tvar
        val resEff = Type.mkUnion(e1.eff, e2.eff, loc)
        InferredAst.Expr.RecordExtend(label, e1, e2, resTpe, resEff, loc)

      case Expr.RecordRestrict(label, exp, loc) =>
        //
        //  exp : { label  :: t | r }
        // -------------------------
        // { -label | exp } : {| r }
        //
        val tvar = freshVar(Kind.Star, loc)
        val freshLabelType = freshVar(Kind.Star, loc)
        val freshRowVar = freshVar(Kind.RecordRow, loc)
        val e = visitExp(exp)
        c.unifyType(e.tpe, Type.mkRecord(Type.mkRecordRowExtend(label, freshLabelType, freshRowVar, loc), loc), loc)
        c.unifyType(tvar, Type.mkRecord(freshRowVar, loc), loc)
        val resTpe = tvar
        val resEff = e.eff
        InferredAst.Expr.RecordRestrict(label, e, resTpe, resEff, loc)

      case Expr.ArrayLit(exps, exp, loc) =>
        val tvar = freshVar(Kind.Star, loc)
        val evar = freshVar(Kind.Eff, loc)
        val regionVar = freshVar(Kind.Eff, loc)
        val regionType = Type.mkRegionToStar(regionVar, loc)
        val es = exps.map(visitExp)
        val tpes = es.map(_.tpe)
        val effs = es.map(_.eff)
        val e = visitExp(exp)
        c.expectType(expected = regionType, actual = e.tpe, exp.loc)
        c.unifyAllTypes(tpes, loc)
        val elmTpe = tpes.headOption.getOrElse(freshVar(Kind.Star, loc))
        c.unifyType(tvar, Type.mkArray(elmTpe, regionVar, loc), loc)
        c.unifyType(evar, Type.mkUnion(Type.mkUnion(effs, loc), e.eff, regionVar, loc), loc)
        val resTpe = tvar
        val resEff = evar
        InferredAst.Expr.ArrayLit(es, e, resTpe, resEff, loc)

      case Expr.ArrayNew(exp1, exp2, exp3, loc) =>
        val tvar = freshVar(Kind.Star, loc)
        val evar = freshVar(Kind.Eff, loc)
        val regionVar = freshVar(Kind.Eff, loc)
        val regionType = Type.mkRegionToStar(regionVar, loc)
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        val e3 = visitExp(exp3)
        c.expectType(expected = regionType, actual = e1.tpe, loc)
        c.expectType(expected = Type.Int32, actual = e3.tpe, exp3.loc)
        c.unifyType(tvar, Type.mkArray(e2.tpe, regionVar, loc), loc)
        c.unifyType(evar, Type.mkUnion(e1.eff, e2.eff, e3.eff, regionVar, loc), loc)
        val resTpe = tvar
        val resEff = evar
        InferredAst.Expr.ArrayNew(e1, e2, e3, resTpe, resEff, loc)

      case Expr.ArrayLoad(exp1, exp2, loc) =>
        val tvar = freshVar(Kind.Star, loc)
        val evar = freshVar(Kind.Eff, loc)
        val regionVar = freshVar(Kind.Eff, loc)
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        c.expectType(expected = Type.mkArray(tvar, regionVar, loc), actual = e1.tpe, exp1.loc)
        c.expectType(expected = Type.Int32, actual = e2.tpe, exp2.loc)
        c.unifyType(evar, Type.mkUnion(regionVar, e1.eff, e2.eff, loc), loc)
        val resTpe = tvar
        val resEff = evar
        InferredAst.Expr.ArrayLoad(e1, e2, resTpe, resEff, loc)

      case Expr.ArrayStore(exp1, exp2, exp3, loc) =>
        val evar = freshVar(Kind.Eff, loc)
        val elmVar = freshVar(Kind.Star, loc)
        val regionVar = freshVar(Kind.Eff, loc)
        val arrayType = Type.mkArray(elmVar, regionVar, loc)
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        val e3 = visitExp(exp3)
        c.expectType(expected = arrayType, actual = e1.tpe, exp1.loc)
        c.expectType(expected = Type.Int32, actual = e2.tpe, exp2.loc)
        c.expectType(expected = elmVar, actual = e3.tpe, exp3.loc)
        c.unifyType(evar, Type.mkUnion(regionVar, e1.eff, e2.eff, e3.eff, loc), loc)
        val resEff = evar
        InferredAst.Expr.ArrayStore(e1, e2, e3, resEff, loc)

      case Expr.ArrayLength(exp, loc) =>
        val evar = freshVar(Kind.Eff, loc)
        val elmVar = freshVar(Kind.Star, loc)
        val regionVar = freshVar(Kind.Eff, loc)
        val e = visitExp(exp)
        c.expectType(Type.mkArray(elmVar, regionVar, loc), e.tpe, exp.loc)
        c.unifyType(evar, e.eff, loc)
        val resEff = evar
        InferredAst.Expr.ArrayLength(e, resEff, loc)

      case Expr.StructNew(sym, fields, regionOpt, loc) =>
        // This case needs to handle expressions like `new S { f = rhs } @ r` where `f` was not present in the struct declaration
        // Here, we check that `rhs` is itself valid by visiting it but make sure not to unify it with anything
        val tvar = freshVar(Kind.Star, loc)
        val evar = freshVar(Kind.Eff, loc)
        val (instantiatedFieldTpes, structTpe, regionVarOpt) = instantiateStruct(sym, root.structs)
        val fs = fields.map { case (k, v) => (k, visitExp(v)) }
        val fieldTpes = fs.map { case (_, e) => e.tpe }
        val fieldEffs = fs.map { case (_, e) => e.eff }
        c.unifyType(tvar, structTpe, loc)
        for {
          ((fieldSymUse, expr), fieldTpe1) <- ListOps.zip(fields, fieldTpes)
        } {
          instantiatedFieldTpes.get(fieldSymUse.sym) match {
            case None => () // if not an actual field, there is nothing to unify
            case Some((_, fieldTpe2)) => c.unifyType(fieldTpe1, fieldTpe2, expr.loc)
          }
        }
        val r = (regionOpt, regionVarOpt) match {
          case (Some(region), Some(regionVar)) =>
            val e = visitExp(region)
            c.unifyType(Type.mkRegionToStar(regionVar, loc), e.tpe, region.loc)
            c.unifyType(evar, Type.mkUnion(e.eff :: regionVar :: fieldEffs, loc), loc)
            Some(e)
          case (Some(_), None) =>
            // Unreachable: every struct has a region parameter. The parser requires at least one type
            // parameter, and the kinder adds a synthetic region parameter if the user left them out.
            throw InternalCompilerException("Unexpected struct without a region parameter", loc)
          case (None, _) => None
        }
        val resTpe = tvar
        val resEff = evar
        InferredAst.Expr.StructNew(sym, fs, r, resTpe, resEff, loc)

      case Expr.StructGet(exp, symUse, loc) =>
        val tvar = freshVar(Kind.Star, loc)
        val evar = freshVar(Kind.Eff, loc)
        val (instantiatedFieldTpes, structTpe, regionVarOpt) = instantiateStruct(symUse.sym.structSym, root.structs)
        val e = visitExp(exp)
        c.expectType(structTpe, e.tpe, exp.loc)
        val (mutable, fieldTpe) = instantiatedFieldTpes(symUse.sym)
        c.unifyType(fieldTpe, tvar, loc)
        // If the field is mutable, then it emits a region effect, otherwise not.
        val accessEffect = if (!mutable) Type.mkPure(loc) else regionVarOpt match {
          case Some(value) => value
          case None => throw InternalCompilerException(s"Unexpected missing region var in struct get near ${exp}", loc)
        }
        c.unifyType(Type.mkUnion(e.eff, accessEffect, loc), evar, exp.loc)
        val resTpe = tvar
        val resEff = evar
        InferredAst.Expr.StructGet(e, symUse, resTpe, resEff, loc)

      case Expr.StructPut(exp1, symUse, exp2, loc) =>
        val tvar = freshVar(Kind.Star, loc)
        val evar = freshVar(Kind.Eff, loc)
        val (instantiatedFieldTpes, structTpe, regionVarOpt) = instantiateStruct(symUse.sym.structSym, root.structs)
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        c.expectType(structTpe, e1.tpe, exp1.loc)
        val (_, fieldTpe) = instantiatedFieldTpes(symUse.sym)
        c.expectType(fieldTpe, e2.tpe, exp2.loc)
        c.unifyType(Type.mkUnit(loc), tvar, loc)
        c.unifyType(Type.mkUnion(e1.eff, e2.eff, regionVarOpt.get, loc), evar, loc)
        val resTpe = tvar
        val resEff = evar
        InferredAst.Expr.StructPut(e1, symUse, e2, resTpe, resEff, loc)

      case Expr.VectorLit(exps, loc) =>
        val tvar = freshVar(Kind.Star, loc)
        val evar = freshVar(Kind.Eff, loc)
        val es = exps.map(visitExp)
        val tpes = es.map(_.tpe)
        val effs = es.map(_.eff)
        c.unifyAllTypes(tpes, loc)
        val tpe = tpes.headOption.getOrElse(freshVar(Kind.Star, loc))
        c.unifyType(tvar, Type.mkVector(tpe, loc), loc)
        c.unifyType(evar, Type.mkUnion(effs, loc), loc)
        val resTpe = tvar
        val resEff = evar
        InferredAst.Expr.VectorLit(es, resTpe, resEff, loc)

      case Expr.VectorLoad(exp1, exp2, loc) =>
        val tvar = freshVar(Kind.Star, loc)
        val evar = freshVar(Kind.Eff, loc)
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        c.expectType(expected = Type.mkVector(tvar, loc), actual = e1.tpe, exp1.loc)
        c.expectType(expected = Type.Int32, actual = e2.tpe, exp2.loc)
        c.unifyType(evar, Type.mkUnion(e1.eff, e2.eff, loc), loc)
        val resTpe = tvar
        val resEff = evar
        InferredAst.Expr.VectorLoad(e1, e2, resTpe, resEff, loc)

      case Expr.VectorLength(exp, loc) =>
        val elmVar = freshVar(Kind.Star, loc)
        val e = visitExp(exp)
        c.expectType(Type.mkVector(elmVar, loc), e.tpe, exp.loc)
        InferredAst.Expr.VectorLength(e, loc)

      case Expr.Ascribe(exp, expectedTpe, expectedEff, tvar, loc) =>
        // An ascribe expression is sound; the type system checks that the declared type matches the inferred type.
        val e = visitExp(exp)
        expectedTpe.foreach { tpe => c.expectType(expected = tpe, actual = e.tpe, loc) }
        c.unifyType(e.tpe, tvar, loc)
        expectedEff.foreach { eff => c.expectType(expected = eff, actual = e.eff, loc) }
        val resTpe = tvar
        val resEff = e.eff
        InferredAst.Expr.Ascribe(e, expectedTpe, expectedEff, resTpe, resEff, loc)

      case Expr.InstanceOf(exp, clazz, loc) =>
        val e = visitExp(exp)
        InferredAst.Expr.InstanceOf(e, clazz, loc)

      case Expr.CheckedCast(cast, exp, tvar, evar, loc) =>
        // A cast expression is sound; the type system ensures the declared type is correct.
        cast match {
          case CheckedCastType.TypeCast =>
            // We replace the type with a fresh variable to allow any type.
            // The validity of this cast is checked in the Safety phase.
            val e = visitExp(exp)
            c.unifyType(evar, e.eff, loc)
            val resTpe = tvar
            val resEff = evar
            InferredAst.Expr.CheckedCast(cast, e, resTpe, resEff, loc)

          case CheckedCastType.EffectCast =>
            // A checked effect cast is an explicit use of subeffecting: the effect of the cast may
            // be any effect that includes the effect of exp.
            val e = visitExp(exp)
            c.unifyType(tvar, e.tpe, loc)
            c.subEff(e.eff, evar, loc)
            val resTpe = tvar
            val resEff = evar
            InferredAst.Expr.CheckedCast(cast, e, resTpe, resEff, loc)
        }

      case Expr.UncheckedCast(exp, declaredTpe, declaredEff, tvar, loc) =>
        // An unchecked cast expression is unsound; the type system assumes the declared type and effect are correct.
        val e = visitExp(exp)
        c.unifyType(tvar, declaredTpe.getOrElse(e.tpe), loc)
        val resTpe = tvar
        val resEff = declaredEff.getOrElse(e.eff)
        InferredAst.Expr.UncheckedCast(e, declaredTpe, declaredEff, resTpe, resEff, loc)

      case Expr.Unsafe(exp, eff0, asEff0, loc) =>
        val e = visitExp(exp)
        val resTpe = e.tpe
        val resEff = Type.mkUnion(Type.mkDifference(e.eff, eff0, loc), asEff0.getOrElse(Type.Pure), loc)
        InferredAst.Expr.Unsafe(e, eff0, asEff0, resTpe, resEff, loc)


      case Expr.TryCatch(exp, rules, loc) =>
        val e = visitExp(exp)
        val rs = rules.map(visitCatchRule)
        val tpes = rs.map(_.exp.tpe)
        val effs = rs.map(_.exp.eff)
        c.unifyAllTypes(tpes, loc)
        val ruleTpe = tpes.headOption.getOrElse(freshVar(Kind.Star, loc))
        c.unifyType(e.tpe, ruleTpe, loc)
        val resTpe = e.tpe
        val resEff = Type.mkUnion(e.eff :: effs, loc)
        InferredAst.Expr.TryCatch(e, rs, resTpe, resEff, loc)

      case KindedAst.Expr.Throw(exp, tvar, evar, loc) =>
        val e = visitExp(exp)
        c.unifyType(evar, Type.mkUnion(e.eff, Type.IO, loc), loc)
        val resultTpe = tvar
        val resultEff = evar
        InferredAst.Expr.Throw(e, resultTpe, resultEff, loc)

      case Expr.Handler(symUse, rules, tvar, evar1, evar2, loc) =>
        //
        // β̄ fresh
        // ∀i. Γ, opix1: opit1[ᾱ ↦ β̄], .., ki: opit[ᾱ ↦ β̄] -> t \ k_ef ⊢ ei: t \ ei_ef
        //     k_ef = (ef - Eff[β̄]) ∪ (∪_i ei_ef)
        // ---------------------------------------------------------------------
        // Γ ⊢ handler Eff {
        //   def op1(op1x1, .., k1) = e1
        //   def op2(op2x1, .., k2) = e2
        //   ..
        // }: (Unit -> t \ ef) -> t \ k_ef
        //
        // where:
        // eff Eff[ᾱ] {
        //  def op1(op1x1: op1t1, ..): op1t
        //  def op2(op2x1: op2t2, ..): op2t
        //  ..
        // }
        //
        val effect = root.effects(symUse.sym)
        val effectArgs = effect.tparams.map(tparam => freshVar(tparam.sym.kind, loc))
        val effectSubst = Substitution(ListOps.zip(effect.tparams.map(_.sym), effectArgs).toMap)
        val effectKind = Kind.mkArrowTo(effect.tparams.map(_.sym.kind), Kind.Eff)

        val rs = rules.map(visitHandlerRule(_, tvar, evar2, effectSubst))
        val tpes = rs.map(_.exp.tpe)
        val effs = rs.map(_.exp.eff)
        c.unifyAllTypes(tvar :: tpes, loc)

        val handledEffectConstructor = Type.Cst(TypeConstructor.Effect(symUse.sym, effectKind), symUse.qname.loc)
        val handledEffect = Type.mkApply(handledEffectConstructor, effectArgs, symUse.qname.loc)
        // Subtract the effect from the body effect and add the handler effects.
        val continuationEffect = Type.mkUnion(Type.mkDifference(evar1, handledEffect, symUse.qname.loc), Type.mkUnion(effs, loc), loc)
        c.unifyType(evar2, continuationEffect, loc)
        val resultTpe = Type.mkArrowWithEffect(Type.mkArrowWithEffect(Type.Unit, evar1, tvar, loc), evar2, tvar, loc)
        InferredAst.Expr.Handler(symUse, rs, tvar, evar1, evar2, resultTpe, loc)

      case Expr.RunWith(exp1, exp2, tvar, evar, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        val handlerArg = Type.mkArrowWithEffect(Type.Unit, e1.eff, e1.tpe, loc.asSynthetic)
        c.unifyType(Type.mkArrowWithEffect(handlerArg, evar, tvar, loc.asSynthetic), e2.tpe, loc)
        val resultTpe = tvar
        val resultEff = Type.mkUnion(evar, e2.eff, loc.asSynthetic)
        InferredAst.Expr.RunWith(e1, e2, resultTpe, resultEff, loc)

      case Expr.InvokeConstructor(clazz, exps, jvar, tvar, evar, loc) =>
        // Γ ⊢ eᵢ ... : τ₁ ...    Γ ⊢ ι ~ JvmConstructor(k, eᵢ ...)
        // --------------------------------------------------------
        // Γ ⊢ new k(e₁ ...) : k \ JvmToEff[ι]
        val baseEff = Type.JvmToEff(jvar, loc)
        val clazzTpe = JavaTypes.instantiateWithFreshVars(clazz, scope, loc)
        val es = exps.map(visitExp)
        val tpes = es.map(_.tpe)
        val effs = es.map(_.eff)
        c.unifyType(jvar, Type.UnresolvedJvmType(Type.JvmMember.JvmConstructor(clazz, tpes), loc), loc)
        c.unifyType(tvar, clazzTpe, loc)
        c.unifyType(evar, Type.mkUnion(baseEff :: effs, loc), loc)
        val resTpe = tvar
        val resEff = evar
        InferredAst.Expr.InvokeConstructor(es, jvar, resTpe, resEff, loc)

      case Expr.InvokeSuperConstructor(clazz, exps, jvar, tvar, evar, loc) =>
        // Γ ⊢ eᵢ ... : τ₁ ...    Γ ⊢ ι ~ JvmConstructor(k, eᵢ ...)
        // --------------------------------------------------------
        // Γ ⊢ super(e₁ ...) : k \ JvmToEff[ι]
        val baseEff = Type.JvmToEff(jvar, loc)
        val clazzTpe = JavaTypes.instantiateWithFreshVars(clazz, scope, loc)
        val es = exps.map(visitExp)
        val tpes = es.map(_.tpe)
        val effs = es.map(_.eff)
        c.unifyType(jvar, Type.UnresolvedJvmType(Type.JvmMember.JvmConstructor(clazz, tpes), loc), loc)
        c.unifyType(tvar, clazzTpe, loc)
        c.unifyType(evar, Type.mkUnion(baseEff :: effs, loc), loc)
        val resTpe = tvar
        val resEff = evar
        InferredAst.Expr.InvokeSuperConstructor(es, jvar, resTpe, resEff, loc)

      case Expr.InvokeMethod(exp, methodName, exps, jvar, tvar, evar, loc) =>
        // Γ ⊢ e : τ    Γ ⊢ eᵢ ... : τ₁ ...    Γ ⊢ ι ~ JvmMethod(τ, m, τᵢ ...)
        // ---------------------------------------------------------------
        // Γ ⊢ e.m(eᵢ ...) : JvmToType[ι] \ JvmToEff[ι]
        val baseEff = Type.JvmToEff(jvar, loc)
        val e = visitExp(exp)
        val es = exps.map(visitExp)
        val tpes = es.map(_.tpe)
        val effs = es.map(_.eff)
        c.unifyType(jvar, Type.UnresolvedJvmType(Type.JvmMember.JvmMethod(e.tpe, methodName, tpes), loc), loc)
        c.unifyType(tvar, Type.JvmToType(jvar, loc), loc)
        c.unifyType(evar, Type.mkUnion(baseEff :: e.eff :: effs, loc), loc)
        val resTpe = tvar
        val resEff = evar
        InferredAst.Expr.InvokeMethod(e, es, jvar, resTpe, resEff, loc)

      case Expr.InvokeSuperMethod(clazz, methodName, exps, targs, jvar, tvar, evar, loc) =>
        val baseEff = Type.JvmToEff(jvar, loc)
        val clazzTpe = if (targs.nonEmpty) Type.mkApply(JavaTypes.flixTypeOf(clazz, loc), targs, loc)
                       else JavaTypes.instantiateWithObjectArgs(clazz, loc)
        val es = exps.map(visitExp)
        val tpes = es.map(_.tpe)
        val effs = es.map(_.eff)
        c.unifyType(jvar, Type.UnresolvedJvmType(Type.JvmMember.JvmMethod(clazzTpe, methodName, tpes), loc), loc)
        c.unifyType(tvar, Type.JvmToType(jvar, loc), loc)
        c.unifyType(evar, Type.mkUnion(baseEff :: effs, loc), loc)
        val resTpe = tvar
        val resEff = evar
        InferredAst.Expr.InvokeSuperMethod(es, jvar, resTpe, resEff, loc)

      case Expr.InvokeStaticMethod(clazz, methodName, exps, jvar, tvar, evar, loc) =>
        // Γ ⊢ eᵢ ... : τ₁ ...    Γ ⊢ ι ~ JvmStaticMethod(m, τᵢ ...)
        // ---------------------------------------------------------------
        // Γ ⊢ m(eᵢ ...) : JvmToType[ι] \ JvmToEff[ι]
        val baseEff = Type.JvmToEff(jvar, loc)
        val es = exps.map(visitExp)
        val tpes = es.map(_.tpe)
        val effs = es.map(_.eff)
        c.unifyType(jvar, Type.UnresolvedJvmType(Type.JvmMember.JvmStaticMethod(clazz, methodName, tpes), loc), loc)
        c.unifyType(tvar, Type.JvmToType(jvar, loc), loc)
        c.unifyType(evar, Type.mkUnion(baseEff :: effs, loc), loc)
        val resTpe = tvar
        val resEff = evar
        InferredAst.Expr.InvokeStaticMethod(es, jvar, resTpe, resEff, loc)

      case Expr.GetField(exp, fieldName, jvar, tvar, evar, loc) =>
        // Γ ⊢ e : τ    Γ ⊢ ι ~ JvmFieldMethod(τ, m)
        // ---------------------------------------------------------------
        // Γ ⊢ e.f : JvmToType[ι]
        val e = visitExp(exp)
        c.unifyType(jvar, Type.UnresolvedJvmType(Type.JvmMember.JvmField(exp.loc, e.tpe, fieldName), loc), loc)
        c.unifyType(tvar, Type.JvmToType(jvar, loc), loc) // unify field type
        c.unifyType(evar, Type.mkUnion(Type.IO :: e.eff :: Nil, loc), loc) // unify effects
        val resTpe = tvar
        val resEff = evar
        InferredAst.Expr.GetField(e, jvar, resTpe, resEff, loc)

      case Expr.PutField(field, clazz, exp1, exp2, loc) =>
        val fieldType = JavaTypes.instantiateWithObjectArgs(field.ref.descriptor, loc)
        val classType = JavaTypes.instantiateWithObjectArgs(clazz, loc)
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        c.expectType(expected = classType, actual = e1.tpe, exp1.loc)
        c.expectType(expected = fieldType, actual = e2.tpe, exp2.loc)
        val resTpe = Type.Unit
        val resEff = Type.mkUnion(e1.eff, e2.eff, Type.IO, loc)
        InferredAst.Expr.PutField(field, e1, e2, resTpe, resEff, loc)

      case Expr.GetStaticField(field, tvar, loc) =>
        val isFinal = field.isFinal
        val fieldType = JavaTypes.instantiateWithObjectArgs(field.ref.descriptor, loc)
        val fieldReadEff = if (isFinal) Type.Pure else Type.IO
        c.unifyType(tvar, fieldType, loc)
        val resTpe = tvar
        val resEff = fieldReadEff
        InferredAst.Expr.GetStaticField(field, resTpe, resEff, loc)

      case Expr.PutStaticField(field, exp, loc) =>
        val e = visitExp(exp)
        c.expectType(expected = JavaTypes.instantiateWithObjectArgs(field.ref.descriptor, loc), actual = e.tpe, exp.loc)
        val resTpe = Type.Unit
        val resEff = Type.mkUnion(e.eff, Type.IO, loc)
        InferredAst.Expr.PutStaticField(field, e, resTpe, resEff, loc)

      case Expr.NewObject(sym, clazz, targs, constructors, methods, tvar, loc) =>
        val cs = constructors.map(visitJvmConstructor)
        val resTpe = Type.mkApply(JavaTypes.flixTypeOf(clazz.desc, loc), targs, loc)
        c.unifyType(tvar, resTpe, loc)

        val ms = methods.map(visitJvmMethod)

        val resEff = Type.IO
        InferredAst.Expr.NewObject(sym, clazz, tvar, resEff, cs, ms, loc)

      case Expr.NewChannel(exp, tvar, loc) =>
        val elmTpe = freshVar(Kind.Star, loc)
        val e = visitExp(exp)
        c.expectType(expected = Type.Int32, actual = e.tpe, exp.loc)
        c.unifyType(tvar, Type.mkTuple(List(Type.mkSender(elmTpe, loc), Type.mkReceiver(elmTpe, loc)), loc), loc)
        val resTpe = tvar
        val resEff = Type.mkUnion(e.eff, Type.Chan, loc)
        InferredAst.Expr.NewChannel(e, resTpe, resEff, loc)

      case Expr.GetChannel(exp, tvar, evar, loc) =>
        val elmTpe = freshVar(Kind.Star, loc)
        val receiverTpe = Type.mkReceiver(elmTpe, loc)
        val e = visitExp(exp)
        c.expectType(expected = receiverTpe, actual = e.tpe, exp.loc)
        c.unifyType(tvar, elmTpe, loc)
        c.unifyType(evar, Type.mkUnion(e.eff, Type.Chan, Type.NonDet, loc), loc)
        val resTpe = tvar
        val resEff = evar
        InferredAst.Expr.GetChannel(e, resTpe, resEff, loc)

      case Expr.PutChannel(exp1, exp2, evar, loc) =>
        val elmTpe = freshVar(Kind.Star, loc)
        val senderTpe = Type.mkSender(elmTpe, loc)
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        c.expectType(expected = senderTpe, actual = e1.tpe, exp1.loc)
        c.expectType(expected = elmTpe, actual = e2.tpe, exp2.loc)
        c.unifyType(evar, Type.mkUnion(e1.eff, e2.eff, Type.Chan, loc), loc)
        val resTpe = Type.mkUnit(loc)
        val resEff = evar
        InferredAst.Expr.PutChannel(e1, e2, resTpe, resEff, loc)

      case Expr.SelectChannel(rules, default, tvar, evar, loc) =>
        val rs = rules.map(visitSelectRule)
        val ruleTypes = rs.map(_.exp.tpe)
        val ruleEffs = rs.map(r => Type.mkUnion(r.chan.eff, r.exp.eff, Type.Chan, Type.NonDet, r.exp.loc))
        val d = default.map(visitExp)
        val (defaultType, defaultEff) = d match {
          case None => (freshVar(Kind.Star, loc), Type.Pure)
          case Some(e) => (e.tpe, e.eff)
        }
        c.unifyAllTypes(tvar :: defaultType :: ruleTypes, loc)
        c.unifyType(evar, Type.mkUnion(defaultEff :: ruleEffs, loc), loc)
        val resTpe = tvar
        val resEff = evar
        InferredAst.Expr.SelectChannel(rs, d, resTpe, resEff, loc)

      case Expr.Spawn(exp1, exp2, loc) =>
        // Γ ⊢ e1 : τ \ ef1 ∩ prims      Γ ⊢ e2 : Region[r] \ ef2
        // --------------------------------------------------------
        // Γ ⊢ spawn e1 @ e2 : Unit \ (ef1 ∩ prims) ∪ ef2
        val regionVar = freshVar(Kind.Eff, loc)
        val regionType = Type.mkRegionToStar(regionVar, loc)
        val anyEff = freshVar(Kind.Eff, loc)
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        c.unifyType(e1.eff, Type.mkIntersection(anyEff, Type.PrimitiveEffs, loc), exp1.loc)
        c.expectType(expected = regionType, actual = e2.tpe, exp2.loc)
        val resTpe = Type.Unit
        // `regionVar` should be included but is omitted to allow spawn nesting.
        val resEff = Type.mkUnion(e1.eff, e2.eff, loc)
        InferredAst.Expr.Spawn(e1, e2, resTpe, resEff, loc)

      case Expr.ParYield(frags, exp, loc) =>
        // We don't need to keep the types of the fragments
        // since they are all bound to the patterns

        // The result effect is only the effect of exp
        // because the fragments must all be pure.
        val fs = frags.map(visitParYieldFragment)
        val e = visitExp(exp)
        val resTpe = e.tpe
        val resEff = e.eff
        InferredAst.Expr.ParYield(fs, e, resTpe, resEff, loc)

      case Expr.Lazy(exp, loc) =>
        val e = visitExp(exp)
        c.expectType(expected = Type.Pure, actual = e.eff, exp.loc)
        val resTpe = Type.mkLazy(e.tpe, loc)
        InferredAst.Expr.Lazy(e, resTpe, loc)

      case Expr.Force(exp, tvar, loc) =>
        val e = visitExp(exp)
        c.expectType(expected = Type.mkLazy(tvar, loc), actual = e.tpe, exp.loc)
        val resTpe = tvar
        val resEff = e.eff
        InferredAst.Expr.Force(e, resTpe, resEff, loc)

      case e: Expr.FixpointConstraintSet => SchemaConstraintGen.visitFixpointConstraintSet(e)
      case e: Expr.FixpointLambda => SchemaConstraintGen.visitFixpointLambda(e)
      case e: Expr.FixpointMerge => SchemaConstraintGen.visitFixpointMerge(e)
      case e: Expr.FixpointQueryWithProvenance => SchemaConstraintGen.visitFixpointQueryWithProvenance(e)
      case e: Expr.FixpointQueryWithSelect => SchemaConstraintGen.visitFixpointQueryWithSelect(e)
      case e: Expr.FixpointSolveWithProject => SchemaConstraintGen.visitFixpointSolveWithProject(e)
      case e: Expr.FixpointInjectInto => SchemaConstraintGen.visitFixpointInjectInto(e)

      case Expr.Error(m, tvar, evar) =>
        // The error expression has whatever type and effect it needs to have.
        val resTpe = tvar
        val resEff = evar
        InferredAst.Expr.Error(m, resTpe, resEff)
    }
  }

  /**
    * Returns the given expression `exp0` annotated with its type and effect, but discards its constraints.
    *
    * The expression is visited with a fresh `TypeContext` that is simply thrown away afterwards. None of
    * the constraints generated for `exp0` or its sub-expressions ever reach the solver, so the returned
    * node is *not type checked*: its types and effects are left as the unsolved variables that `visitExp`
    * assigns, and a type error inside `exp0` goes unreported.
    *
    * DANGER: This function silently disables type checking for `exp0`. It must only be used for an
    * expression that is type checked elsewhere, or that cannot be type checked because of an earlier
    * error. Do not use it unless you know exactly why that holds for your expression.
    *
    * Current uses:
    *   - The `select` and `where` clauses of a fixpoint query, in `visitFixpointQueryWithSelect`. The
    *     desugarer copies them into the pseudo-constraint of the query expression, where they are type
    *     checked.
    *   - The scrutinee and the rule bodies of a restrictable choose whose pattern is a
    *     `RestrictableChoosePattern.Error`, in `visitRestrictableChoose` and
    *     `visitRestrictableChooseRuleUnchecked`. The expression is already an error, so it is given fresh
    *     error types instead.
    */
  def visitExpUnchecked(exp0: KindedAst.Expr)(implicit root: KindedAst.Root, flix: Flix): InferredAst.Expr =
    visitExp(exp0)(new TypeContext, root, flix)

  /**
    * Generates constraints for the pattern.
    *
    * Returns the pattern annotated with its type. The type may be a variable which must later be resolved.
    */
  def visitPattern(pat0: KindedAst.Pattern)(implicit c: TypeContext, root: KindedAst.Root, flix: Flix): InferredAst.Pattern = {
    implicit val scope: RegionScope = c.getScope
    pat0 match {
      case KindedAst.Pattern.Wild(tvar, loc) =>
        InferredAst.Pattern.Wild(tvar, loc)

      case KindedAst.Pattern.Var(sym, tvar, loc) =>
        c.unifyType(sym.tvar, tvar, loc)
        val bnd = InferredAst.Binder(sym, tvar)
        InferredAst.Pattern.Var(bnd, tvar, loc)

      case KindedAst.Pattern.Cst(cst, loc) =>
        InferredAst.Pattern.Cst(cst, Type.constantType(cst), loc)

      case KindedAst.Pattern.Tag(symUse, pats, tvar, loc) =>
        val decl = root.enums(symUse.sym.enumSym)
        val caze = decl.cases(symUse.sym)
        // We ignore constraints as tag schemes do not have them
        val (_, _, tagType, _) = Scheme.instantiate(caze.sc, loc.asSynthetic)

        // The tag type is a function from the type of variant to the type of the enum.
        val ps = pats.map(visitPattern)
        val tpes = ps.map(_.tpe)
        // A nullary tag is not a function, but the enum type itself.
        val constructorBase = tpes match {
          case Nil => tvar
          case t :: ts => Type.mkPureUncurriedArrow(Nel(t, ts), tvar, loc)
        }
        c.unifyType(tagType, constructorBase, loc)
        InferredAst.Pattern.Tag(symUse, ps, tvar, loc)

      case KindedAst.Pattern.Tuple(elms, loc) =>
        val ps = elms.map(visitPattern)
        val resTpe = Type.mkTuple(ps.map(_.tpe), loc)
        InferredAst.Pattern.Tuple(ps, resTpe, loc)

      case KindedAst.Pattern.Record(pats, pat, tvar, loc) =>
        val freshRowVar = freshVar(Kind.RecordRow, loc)
        val freshRecord = Type.mkRecord(freshRowVar, loc.asSynthetic)

        val p = visitPattern(pat)
        c.unifyType(freshRecord, p.tpe, loc.asSynthetic)
        val ps = pats.map(visitRecordLabelPattern)
        val patTpes = ps.map(p => (p.label, p.pat.tpe, p.loc))
        val resTpe = mkRecordType(patTpes, freshRowVar, loc)
        c.unifyType(resTpe, tvar, loc)
        InferredAst.Pattern.Record(ps, p, resTpe, loc)

      case KindedAst.Pattern.Error(tvar, loc) =>
        InferredAst.Pattern.Error(tvar, loc)

    }
  }

  /**
    * Generates constraints for the given extensible match rule.
    *
    * Returns the rule together with the type information of its pattern.
    *
    * See [[visitExtPattern]] for more information on the return type.
    */
  private def visitExtMatchRule(rule: KindedAst.ExtMatchRule)(implicit c: TypeContext, scope: RegionScope, root: KindedAst.Root, flix: Flix): (InferredAst.ExtMatchRule, Either[(Name.Pred, List[Type]), Type.Var]) = rule match {
    case KindedAst.ExtMatchRule(pat, exp, loc) =>
      val (p, patTpe) = visitExtPattern(pat)
      val e = visitExp(exp)
      (InferredAst.ExtMatchRule(p, e, loc), patTpe)
  }

  /**
    * Generates constraints for the patterns inside the ext pattern.
    *
    * Returns the pattern together with either the tag name along with the types of its term patterns or the type
    * variable of the pattern.
    *
    * The [[Either]] type is required since the caller must eventually separate non-tag patterns from tag patterns
    * to build schema rows.
    */
  private def visitExtPattern(pat0: KindedAst.ExtPattern)(implicit c: TypeContext, scope: RegionScope, flix: Flix): (InferredAst.ExtPattern, Either[(Name.Pred, List[Type]), Type.Var]) = pat0 match {
    case ExtPattern.Default(tvar, loc) =>
      (InferredAst.ExtPattern.Default(loc), Right(tvar))

    case ExtPattern.Tag(label, pats, loc) =>
      val name = Name.Pred(label.name, label.loc)
      val ps = pats.map(visitExtTagPattern)
      (InferredAst.ExtPattern.Tag(label, ps, loc), Left((name, ps.map(_.tpe))))

    case ExtPattern.Error(tvar, loc) =>
      (InferredAst.ExtPattern.Error(loc), Right(tvar))
  }

  /**
    * Generates constraints for the patterns inside the ext tag pattern.
    *
    * Returns the pattern annotated with its type.
    */
  private def visitExtTagPattern(pat0: KindedAst.ExtTagPattern)(implicit c: TypeContext): InferredAst.ExtTagPattern = pat0 match {
    case ExtTagPattern.Wild(tvar, loc) =>
      InferredAst.ExtTagPattern.Wild(tvar, loc)

    case ExtTagPattern.Var(sym, tvar, loc) =>
      c.unifyType(sym.tvar, tvar, loc)
      val bnd = InferredAst.Binder(sym, tvar)
      InferredAst.ExtTagPattern.Var(bnd, tvar, loc)

    case ExtTagPattern.Unit(loc) =>
      InferredAst.ExtTagPattern.Unit(Type.Unit, loc)

    case ExtTagPattern.Error(tvar, loc) =>
      InferredAst.ExtTagPattern.Error(tvar, loc)
  }

  /**
    * Generates constraints for the patterns inside the record label pattern.
    *
    * Returns the record label pattern annotated with its type.
    */
  private def visitRecordLabelPattern(pat: KindedAst.Pattern.Record.RecordLabelPattern)(implicit c: TypeContext, root: KindedAst.Root, flix: Flix): InferredAst.Pattern.Record.RecordLabelPattern = pat match {
    case KindedAst.Pattern.Record.RecordLabelPattern(label, pat1, tvar, loc) =>
      // { Label = Pattern ... }
      val p = visitPattern(pat1)
      c.unifyType(p.tpe, tvar, loc)
      InferredAst.Pattern.Record.RecordLabelPattern(label, p, tvar, loc)
  }

  /**
    * Generates constraints for the given match rule.
    *
    * Returns the rule with its pattern, guard, and body annotated with their types and effects.
    */
  private def visitMatchRule(rule: KindedAst.MatchRule)(implicit c: TypeContext, root: KindedAst.Root, flix: Flix): InferredAst.MatchRule = rule match {
    case KindedAst.MatchRule(pat, guard, exp, loc) =>
      val p = visitPattern(pat)
      val g = guard.map {
        case guardExp =>
          val ge = visitExp(guardExp)
          c.expectType(expected = Type.Bool, actual = ge.tpe, guardExp.loc)
          c.expectType(expected = Type.Pure, actual = ge.eff, guardExp.loc)
          ge
      }

      val e = visitExp(exp)
      InferredAst.MatchRule(p, g, e, loc)
  }

  /**
    * Generates constraints for the given catch rule.
    *
    * Returns the rule with its body annotated with its type and effect.
    */
  private def visitCatchRule(rule: KindedAst.CatchRule)(implicit c: TypeContext, root: KindedAst.Root, flix: Flix): InferredAst.CatchRule = rule match {
    case KindedAst.CatchRule(sym, clazz, exp, loc) =>
      c.expectType(expected = JavaTypes.flixTypeOf(clazz, sym.loc), sym.tvar, sym.loc)
      val e = visitExp(exp)
      val bnd = InferredAst.Binder(sym, JavaTypes.flixTypeOf(clazz, loc))
      InferredAst.CatchRule(bnd, clazz, e, loc)
  }

  /**
    * Generates constraints unifying the given expected and actual formal parameters.
    */
  private def unifyFormalParams(op: Symbol.OpSym, expected: List[Type], actual: List[KindedAst.FormalParam])(implicit c: TypeContext): Unit = {
    // length check done in Resolver
    c.expectTypeArguments(op, expectedTypes = expected, actualTypes = actual.map(_.tpe), actual.map(_.loc))
  }

  /**
    * Generates constraints for the given handler rule.
    *
    * Returns the body's type and the body's effect.
    *
    * @param tryBlockTpe        the type of the try-block associated with the handler
    * @param continuationEffect the effect of the continuation
    * @param effectSubst        the shared instantiation of the effect's type parameters
    */
  private def visitHandlerRule(rule: KindedAst.HandlerRule, tryBlockTpe: Type, continuationEffect: Type, effectSubst: Substitution)(implicit c: TypeContext, root: KindedAst.Root, flix: Flix): InferredAst.HandlerRule = rule match {
    case KindedAst.HandlerRule(symUse, actualFparams0, body, opTvar, loc) =>
      val effect = root.effects(symUse.sym.eff)
      val ops = effect.ops.map(op => op.sym -> op).toMap
      // The effect parameters have already been instantiated once for the enclosing handler.
      // Don't need to handle unknown op because resolver would have caught this
      // The last formal parameter is the resumption, the rest correspond to the operation's parameters.
      val actualFparams = actualFparams0.init
      val resumptionFparam = actualFparams0.last
      ops(symUse.sym) match {
        case KindedAst.Op(_, KindedAst.Spec(_, _, _, _, expectedFparams, _, opTpe, _, _, _), _) =>
          val expectedParamTypes = expectedFparams.toList.map(fparam => effectSubst(fparam.tpe))
          val resumptionArgType = effectSubst(opTpe)
          val resumptionResType = tryBlockTpe
          val resumptionEff = continuationEffect
          val expectedResumptionType = Type.mkArrowWithEffect(resumptionArgType, resumptionEff, resumptionResType, loc.asSynthetic)
          unifyFormalParams(symUse.sym, expected = expectedParamTypes, actual = actualFparams)
          c.expectType(expected = expectedResumptionType, actual = resumptionFparam.tpe, resumptionFparam.loc)
          val e = visitExp(body)

          // unify the operation return type with its tvar
          c.unifyType(e.tpe, opTvar, body.loc)

          val fps = actualFparams0.map(visitFormalParam)
          InferredAst.HandlerRule(symUse, fps, e, loc)
      }
  }

  /**
    * Returns the given formal parameter `fparam0` annotated with the type of its binder.
    */
  private def visitFormalParam(fparam0: KindedAst.FormalParam): InferredAst.FormalParam = fparam0 match {
    case KindedAst.FormalParam(sym, tpe, src, loc) =>
      val bnd = InferredAst.Binder(sym, tpe)
      InferredAst.FormalParam(bnd, tpe, src, loc)
  }

  /**
    * Generates constraints for the JVM constructor.
    */
  private def visitJvmConstructor(constructor: KindedAst.JvmConstructor)(implicit c: TypeContext, root: KindedAst.Root, flix: Flix): InferredAst.JvmConstructor = constructor match {
    case KindedAst.JvmConstructor(exp, returnTpe, eff, loc) =>
      val e = visitExp(exp)
      c.expectType(expected = returnTpe, actual = e.tpe, exp.loc)
      c.expectType(expected = eff, actual = e.eff, exp.loc)
      InferredAst.JvmConstructor(e, returnTpe, eff, loc)
  }

  /**
    * Generates constraints for the JVM method.
    */
  private def visitJvmMethod(method: KindedAst.JvmMethod)(implicit c: TypeContext, root: KindedAst.Root, flix: Flix): InferredAst.JvmMethod = method match {
    case KindedAst.JvmMethod(ann, ident, fparams, exp, returnTpe, eff, loc) =>
      // Constrain each formal parameter to its declared type.
      fparams.foreach(fp => c.unifyType(fp.sym.tvar, fp.tpe, fp.loc))
      val fps = fparams.map(visitFormalParam)
      val e = visitExp(exp)
      c.expectType(expected = returnTpe, actual = e.tpe, exp.loc)
      c.expectType(expected = eff, actual = e.eff, exp.loc)
      InferredAst.JvmMethod(ann, ident, fps, e, returnTpe, eff, loc)
  }

  /**
    * Generates constraints for the SelectChannelRule.
    *
    * Returns the rule with its channel and body annotated with their types and effects.
    */
  private def visitSelectRule(sr0: KindedAst.SelectChannelRule)(implicit c: TypeContext, root: KindedAst.Root, flix: Flix): InferredAst.SelectChannelRule = {
    sr0 match {
      case KindedAst.SelectChannelRule(sym, chan, body, loc) =>
        val e1 = visitExp(chan)
        val e2 = visitExp(body)
        c.unifyType(e1.tpe, Type.mkReceiver(sym.tvar, sym.loc), sym.loc)
        val bnd = InferredAst.Binder(sym, e1.tpe)
        InferredAst.SelectChannelRule(bnd, e1, e2, loc)
    }
  }

  /**
    * Generates constraints for the given ParYieldFragment.
    */
  private def visitParYieldFragment(frag: KindedAst.ParYieldFragment)(implicit c: TypeContext, root: KindedAst.Root, flix: Flix): InferredAst.ParYieldFragment = frag match {
    case KindedAst.ParYieldFragment(pat, exp, loc) =>
      val p = visitPattern(pat)
      val e = visitExp(exp)
      c.unifyType(p.tpe, e.tpe, loc)
      c.expectType(expected = Type.Pure, actual = e.eff, exp.loc)
      InferredAst.ParYieldFragment(p, e, loc)
  }

  /**
    * Builds a record type from the given fields and rest type.
    *
    * The rest type represents the "tail" of the record.
    */
  private def mkRecordType(patTypes: List[(Name.Label, Type, SourceLocation)], rest: Type, loc: SourceLocation): Type = {
    val ps = patTypes.foldRight(rest) {
      case ((lbl, t, l), acc) => Type.mkRecordRowExtend(
        lbl, t, acc, l)
    }
    Type.mkRecord(ps, loc)
  }

  /**
    * Returns the operation corresponding to the given symbol.
    */
  private def lookupOp(sym: Symbol.OpSym, loc: SourceLocation)(implicit root: KindedAst.Root): KindedAst.Op = {
    val eff = root.effects(sym.eff)
    eff.ops.find(_.sym == sym)
      .getOrElse(throw InternalCompilerException(s"Unexpected missing operation $sym in effect ${sym.eff}", loc))
  }

  /**
    * Converts the given type to a free variable if it is `Void`.
    *
    * Otherwise, returns the given type.
    *
    * This is used for operation return types, which cannot be polymorphic.
    */
  private def generalizeVoid(t: Type)(implicit c: TypeContext, flix: Flix): Type = {
    implicit val scope: RegionScope = c.getScope
    t.typeConstructor match {
      case Some(TypeConstructor.Void) =>
        // The operation type is `Void`. Flix does not have subtyping, but here we want something close to it.
        // Hence, we treat `Void` as a fresh type variable.
        // An alternative would be to allow empty pattern matches, but that is cumbersome.
        Type.freshVar(Kind.Star, t.loc, VarText.Absent)
      case _ => t
    }
  }

  /**
    * Instantiates the scheme of the struct in corresponding to `sym` in `structs`
    * Returns a map from field name to its instantiated type, the type of the instantiated struct, and the instantiated struct's optional region variable
    *
    * For example, for the struct `struct S [v, r] { a: v, b: Int32 }` where `v` instantiates to `v'` and `r` instantiates to `r'`
    * The first element of the return tuple would be a map with entries `a -> v'` and `b -> Int32`
    * The second element of the return tuple would be(locations omitted) `Apply(Apply(Cst(Struct(S)), v'), r')`
    * The third element of the return tuple would be `Some(r')`
    *
    * For a immutable struct `struct S [v] { a: v, b: Int32 }` the third element would be `None`.
    */
  private def instantiateStruct(sym: Symbol.StructSym, structs: Map[Symbol.StructSym, KindedAst.Struct])(implicit c: TypeContext, flix: Flix): (Map[Symbol.StructFieldSym, (Boolean, Type)], Type, Option[Type.Var]) = {
    implicit val scope: RegionScope = c.getScope
    val struct = structs(sym)
    if (struct.mod.isMutable) {
      if (struct.tparams.last.sym.kind != Kind.Eff) {
        throw InternalCompilerException(s"Unexpected kind ${struct.tparams.last.sym.kind} in struct. Expected `${Kind.Eff}`", struct.loc)
      }
    }
    val fields = struct.fields
    val (_, _, tpe, substMap) = Scheme.instantiate(struct.sc, struct.loc)
    val subst = Substitution(substMap)
    val instantiatedFields = fields.map {
      case KindedAst.StructField(_, mod, fieldSym, fieldTpe, _) =>
        fieldSym -> (mod.isMutable, subst(fieldTpe))
    }
    val regionOpt = struct.tparams.lastOption.map(region => substMap(region.sym))
    (instantiatedFields.toMap, tpe, regionOpt)
  }

  /**
    * Returns `true` if `exp` is the body of a curried lambda, i.e. if `exp` is itself a lambda.
    *
    * A match-lambda `match p -> e` desugars to `x -> match x { case p => e }`, so a match on a variable
    * whose only rule has no guard is looked through to the body `e` of that rule.
    */
  @tailrec
  private def isCurriedLambdaBody(exp: KindedAst.Expr): Boolean = exp match {
    case Expr.Lambda(_, _, _) => true
    case Expr.Match(Expr.Var(_, _), List(KindedAst.MatchRule(_, None, body, _)), _) => isCurriedLambdaBody(body)
    case _ => false
  }

  /** Returns `true` if `exp` is a JVM interop invocation (constructor, method, or static method). */
  private def isJvmInvoke(exp: KindedAst.Expr): Boolean = exp match {
    case _: Expr.InvokeConstructor => true
    case _: Expr.InvokeSuperConstructor => true
    case _: Expr.InvokeMethod => true
    case _: Expr.InvokeSuperMethod => true
    case _: Expr.InvokeStaticMethod => true
    case _ => false
  }

  /** Returns a fresh variable with a synthetic location. */
  /**
    * Returns the effect of a lambda or local def whose body has the effect `eff`, according to `bound`.
    *
    * With [[EffectBound.Exact]] the function has exactly the effect of its body. With [[EffectBound.Upper]]
    * the function has a fresh effect variable of which the effect of the body must be a subeffect, so the
    * function may be given a larger effect than its body.
    */
  private def mkBoundedEff(bound: EffectBound, eff: Type, loc: SourceLocation)(implicit c: TypeContext, scope: RegionScope, flix: Flix): Type = bound match {
    case EffectBound.Exact => eff
    case EffectBound.Upper =>
      val evar = freshVar(Kind.Eff, loc)
      c.subEff(eff, evar, loc)
      evar
  }

  private def freshVar(k: Kind, loc: SourceLocation)(implicit scope: RegionScope, flix: Flix): Type.Var =
    Type.freshVar(k, loc.asSynthetic)
}
