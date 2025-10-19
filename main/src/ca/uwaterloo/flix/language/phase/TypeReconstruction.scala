/*
 * Copyright 2015-2023 Magnus Madsen, Matthew Lutze
 * Copyright 2024 Alexander Dybdahl Troelsen
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

import ca.uwaterloo.flix.language.ast.*
import ca.uwaterloo.flix.language.ast.Type.getFlixType
import ca.uwaterloo.flix.language.ast.shared.{CheckedCastType, Constant}
import ca.uwaterloo.flix.language.errors.TypeError
import ca.uwaterloo.flix.language.phase.typer.SubstitutionTree

import java.lang.reflect.Executable

object TypeReconstruction {

  /**
    * Reconstructs types in the given def.
    */
  def visitDef(defn: KindedAst.Def, subst: SubstitutionTree): TypedAst.Def = defn match {
    case KindedAst.Def(sym, spec0, exp0, loc) =>
      val spec = visitSpec(spec0)
      val exp = visitExp(exp0)(subst)
      TypedAst.Def(sym, spec, exp, loc)
  }

  /**
    * Reconstructs types in the given sig.
    */
  def visitSig(sig: KindedAst.Sig, subst: SubstitutionTree): TypedAst.Sig = sig match {
    case KindedAst.Sig(sym, spec0, exp0, loc) =>
      val spec = visitSpec(spec0)
      val exp = exp0.map(visitExp(_)(subst))
      TypedAst.Sig(sym, spec, exp, loc)
  }

  /**
    * Reconstructs types in the given spec.
    */
  private def visitSpec(spec: KindedAst.Spec): TypedAst.Spec = spec match {
    case KindedAst.Spec(doc, ann, mod, tparams0, fparams0, sc, tpe, eff, tconstrs, econstrs) =>
      val tparams = tparams0.map(visitTypeParam)
      val fparams = fparams0.map(visitFormalParam(_, SubstitutionTree.empty))
      // We do not perform substitution on any of the types because they should all be rigid.
      TypedAst.Spec(doc, ann, mod, tparams, fparams, sc, tpe, eff, tconstrs, econstrs)
  }

  /**
    * Reconstructs types in the given tparams.
    */
  private def visitTypeParam(tparam: KindedAst.TypeParam): TypedAst.TypeParam = tparam match {
    case KindedAst.TypeParam(name, sym, loc) => TypedAst.TypeParam(name, sym, loc)
  }

  /**
    * Reconstructs types in the given fparams.
    */
  private def visitFormalParam(fparam: KindedAst.FormalParam, subst: SubstitutionTree): TypedAst.FormalParam = fparam match {
    case KindedAst.FormalParam(sym, tpe0, src, loc) =>
      val tpe = subst(tpe0)
      val bnd = TypedAst.Binder(sym, tpe)
      TypedAst.FormalParam(bnd, tpe, src, loc)
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
  private def visitExp(exp0: KindedAst.Exp)(implicit subst: SubstitutionTree): TypedAst.Exp = exp0 match {
    case KindedAst.Exp.Var(sym, loc) =>
      TypedAst.Exp.Var(sym, subst(sym.tvar), loc)

    case KindedAst.Exp.Hole(sym, env, tpe, evar, loc) =>
      TypedAst.Exp.Hole(sym, env, subst(tpe), subst(evar), loc)

    case KindedAst.Exp.HoleWithExp(exp, env, tvar, evar, loc) =>
      val e = visitExp(exp)
      TypedAst.Exp.HoleWithExp(e, env, subst(tvar), subst(evar), loc)

    case KindedAst.Exp.OpenAs(symUse, exp, tvar, loc) =>
      val e = visitExp(exp)
      TypedAst.Exp.OpenAs(symUse, e, subst(tvar), loc)

    case KindedAst.Exp.Use(sym, alias, exp, loc) =>
      val e = visitExp(exp)
      TypedAst.Exp.Use(sym, alias, e, loc)

    case KindedAst.Exp.Cst(Constant.Null, loc) =>
      TypedAst.Exp.Cst(Constant.Null, Type.Null, loc)

    case KindedAst.Exp.Cst(cst, loc) => TypedAst.Exp.Cst(cst, Type.constantType(cst), loc)

    case KindedAst.Exp.ApplyClo(exp1, exp2, tvar, evar, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      TypedAst.Exp.ApplyClo(e1, e2, subst(tvar), subst(evar), loc)

    case KindedAst.Exp.ApplyDef(symUse, exps, targs, itvar, tvar, evar, loc) =>
      val es = exps.map(visitExp)
      val tas = targs.map(subst.apply)
      TypedAst.Exp.ApplyDef(symUse, es, tas, subst(itvar), subst(tvar), subst(evar), loc)

    case KindedAst.Exp.ApplyLocalDef(symUse, exps, arrowTvar, tvar, evar, loc) =>
      val es = exps.map(visitExp)
      val at = subst(arrowTvar)
      val t = subst(tvar)
      val ef = subst(evar)
      TypedAst.Exp.ApplyLocalDef(symUse, es, at, t, ef, loc)

    case KindedAst.Exp.ApplyOp(symUse, exps, tvar, evar, loc) =>
      val es = exps.map(visitExp(_))
      val tpe = subst(tvar)
      val eff = subst(evar)
      TypedAst.Exp.ApplyOp(symUse, es, tpe, eff, loc)

    case KindedAst.Exp.ApplySig(symUse, exps, targ, targs, itvar, tvar, evar, loc) =>
      val es = exps.map(visitExp)
      val ta = subst(targ)
      val tas = targs.map(subst.apply)
      TypedAst.Exp.ApplySig(symUse, es, ta, tas, subst(itvar), subst(tvar), subst(evar), loc)

    case KindedAst.Exp.Lambda(fparam, exp, _, loc) =>
      val p = visitFormalParam(fparam, subst)
      val e = visitExp(exp)
      val t = Type.mkArrowWithEffect(p.tpe, e.eff, e.tpe, loc)
      TypedAst.Exp.Lambda(p, e, t, loc)

    case KindedAst.Exp.Unary(sop, exp, tvar, loc) =>
      val e = visitExp(exp)
      val eff = e.eff
      TypedAst.Exp.Unary(sop, e, subst(tvar), eff, loc)

    case KindedAst.Exp.Binary(sop, exp1, exp2, tvar, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val eff = Type.mkUnion(e1.eff, e2.eff, loc)
      TypedAst.Exp.Binary(sop, e1, e2, subst(tvar), eff, loc)

    case KindedAst.Exp.IfThenElse(exp1, exp2, exp3, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val e3 = visitExp(exp3)
      val tpe = e2.tpe
      val eff = Type.mkUnion(e1.eff, e2.eff, e3.eff, loc)
      TypedAst.Exp.IfThenElse(e1, e2, e3, tpe, eff, loc)

    case KindedAst.Exp.Stm(exp1, exp2, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val tpe = e2.tpe
      val eff = Type.mkUnion(e1.eff, e2.eff, loc)
      TypedAst.Exp.Stm(e1, e2, tpe, eff, loc)

    case KindedAst.Exp.Discard(exp, loc) =>
      val e = visitExp(exp)
      TypedAst.Exp.Discard(e, e.eff, loc)

    case KindedAst.Exp.Let(sym, exp1, exp2, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val bnd = TypedAst.Binder(sym, e1.tpe)
      val tpe = e2.tpe
      val eff = Type.mkUnion(e1.eff, e2.eff, loc)
      TypedAst.Exp.Let(bnd, e1, e2, tpe, eff, loc)

    case KindedAst.Exp.LocalDef(sym, fparams, exp1, exp2, loc) =>
      val fps = fparams.map(visitFormalParam(_, subst))
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val tpe = e2.tpe
      val eff = e2.eff
      val boundType = Type.mkUncurriedArrowWithEffect(fps.map(_.tpe), e1.tpe, e1.eff, SourceLocation.Unknown)
      val bnd = TypedAst.Binder(sym, boundType)
      TypedAst.Exp.LocalDef(bnd, fps, e1, e2, tpe, eff, loc)

    case KindedAst.Exp.Region(sym, regSym, exp, tvar, evar, loc) =>
      // Use the appropriate branch for the scope.
      val e = visitExp(exp)(subst.branches.getOrElse(regSym, SubstitutionTree.empty))
      val tpe = subst(tvar)
      val eff = subst(evar)
      val bnd = TypedAst.Binder(sym, eff)
      TypedAst.Exp.Region(bnd, regSym, e, tpe, eff, loc)

    case KindedAst.Exp.Match(matchExp, rules, loc) =>
      val e1 = visitExp(matchExp)
      val rs = rules map {
        case KindedAst.MatchRule(pat, guard, exp, ruleLoc) =>
          val p = visitPattern(pat)
          val g = guard.map(visitExp(_))
          val b = visitExp(exp)
          TypedAst.MatchRule(p, g, b, ruleLoc)
      }
      val tpe = rs.head.exp.tpe
      val eff = rs.foldLeft(e1.eff) {
        case (acc, TypedAst.MatchRule(_, g, b, _)) => Type.mkUnion(g.map(_.eff).toList ::: List(b.eff, acc), loc)
      }
      TypedAst.Exp.Match(e1, rs, tpe, eff, loc)

    case KindedAst.Exp.TypeMatch(matchExp, rules, loc) =>
      val e1 = visitExp(matchExp)
      val rs = rules map {
        case KindedAst.TypeMatchRule(sym, tpe0, exp, ruleLoc) =>
          val t = subst(tpe0)
          val b = visitExp(exp)
          val bnd = TypedAst.Binder(sym, t)
          TypedAst.TypeMatchRule(bnd, t, b, ruleLoc)
      }
      val tpe = rs.head.exp.tpe
      val eff = rs.foldLeft(e1.eff) {
        case (acc, TypedAst.TypeMatchRule(_, _, b, _)) => Type.mkUnion(b.eff, acc, loc)
      }
      TypedAst.Exp.TypeMatch(e1, rs, tpe, eff, loc)

    case KindedAst.Exp.RestrictableChoose(star, exp, rules, tvar, loc) =>
      val e = visitExp(exp)
      val rs = rules.map {
        case KindedAst.RestrictableChooseRule(pat0, body0) =>
          val pat = pat0 match {
            case KindedAst.RestrictableChoosePattern.Tag(symUse, pats, tagTvar, tagLoc) =>
              val ps = pats.map {
                case KindedAst.RestrictableChoosePattern.Wild(wildTvar, wildLoc) => TypedAst.RestrictableChoosePattern.Wild(subst(wildTvar), wildLoc)
                case KindedAst.RestrictableChoosePattern.Var(sym, varTvar, varLoc) => TypedAst.RestrictableChoosePattern.Var(TypedAst.Binder(sym, subst(varTvar)), subst(varTvar), varLoc)
                case KindedAst.RestrictableChoosePattern.Error(errTvar, errLoc) => TypedAst.RestrictableChoosePattern.Error(subst(errTvar), errLoc)
              }
              TypedAst.RestrictableChoosePattern.Tag(symUse, ps, subst(tagTvar), tagLoc)
            case KindedAst.RestrictableChoosePattern.Error(errTvar, errLoc) => TypedAst.RestrictableChoosePattern.Error(subst(errTvar), errLoc)
          }
          val body = visitExp(body0)
          TypedAst.RestrictableChooseRule(pat, body)
      }
      val eff = Type.mkUnion(rs.map(_.exp.eff), loc)
      TypedAst.Exp.RestrictableChoose(star, e, rs, subst(tvar), eff, loc)

    case KindedAst.Exp.ExtMatch(exp, rules, loc) =>
      val e = visitExp(exp)
      val rs = rules.map(visitExtMatchRule)
      val tpe = rs.head.exp.tpe // Note: We are guaranteed to have at least one rule.
      val eff = Type.mkUnion(e.eff :: rs.map(_.exp.eff), loc)
      TypedAst.Exp.ExtMatch(e, rs, tpe, eff, loc)

    case KindedAst.Exp.Tag(symUse, exps, tvar, loc) =>
      val es = exps.map(visitExp)
      val eff = Type.mkUnion(es.map(_.eff), loc)
      TypedAst.Exp.Tag(symUse, es, subst(tvar), eff, loc)

    case KindedAst.Exp.RestrictableTag(symUse, exps, _, tvar, evar, loc) =>
      val es = exps.map(visitExp)
      TypedAst.Exp.RestrictableTag(symUse, es, subst(tvar), subst(evar), loc)

    case KindedAst.Exp.ExtTag(label, exps, tvar, loc) =>
      val es = exps.map(visitExp)
      val tpe = subst(tvar)
      val eff = Type.mkUnion(es.map(_.eff), loc)
      TypedAst.Exp.ExtTag(label, es, tpe, eff, loc)

    case KindedAst.Exp.Tuple(elms, loc) =>
      val es = elms.map(visitExp(_))
      val tpe = Type.mkTuple(es.map(_.tpe), loc)
      val eff = Type.mkUnion(es.map(_.eff), loc)
      TypedAst.Exp.Tuple(es, tpe, eff, loc)

    case KindedAst.Exp.RecordSelect(exp, field, tvar, loc) =>
      val e = visitExp(exp)
      val eff = e.eff
      TypedAst.Exp.RecordSelect(e, field, subst(tvar), eff, loc)

    case KindedAst.Exp.RecordExtend(field, value, rest, tvar, loc) =>
      val v = visitExp(value)
      val r = visitExp(rest)
      val eff = Type.mkUnion(v.eff, r.eff, loc)
      TypedAst.Exp.RecordExtend(field, v, r, subst(tvar), eff, loc)

    case KindedAst.Exp.RecordRestrict(field, rest, tvar, loc) =>
      val r = visitExp(rest)
      val eff = r.eff
      TypedAst.Exp.RecordRestrict(field, r, subst(tvar), eff, loc)

    case KindedAst.Exp.ArrayLit(exps, exp, tvar, evar, loc) =>
      val es = exps.map(visitExp(_))
      val e = visitExp(exp)
      val tpe = subst(tvar)
      val eff = subst(evar)
      TypedAst.Exp.ArrayLit(es, e, tpe, eff, loc)

    case KindedAst.Exp.ArrayNew(exp1, exp2, exp3, tvar, evar, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val e3 = visitExp(exp3)
      val tpe = subst(tvar)
      val eff = subst(evar)
      TypedAst.Exp.ArrayNew(e1, e2, e3, tpe, eff, loc)

    case KindedAst.Exp.ArrayLoad(exp1, exp2, tvar, evar, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val tpe = subst(tvar)
      val eff = subst(evar)
      TypedAst.Exp.ArrayLoad(e1, e2, tpe, eff, loc)

    case KindedAst.Exp.ArrayStore(exp1, exp2, exp3, evar, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val e3 = visitExp(exp3)
      val eff = subst(evar)
      TypedAst.Exp.ArrayStore(e1, e2, e3, eff, loc)

    case KindedAst.Exp.ArrayLength(exp, evar, loc) =>
      val e = visitExp(exp)
      val eff = subst(evar)
      TypedAst.Exp.ArrayLength(e, eff, loc)

    case KindedAst.Exp.StructNew(sym, fields0, region0, tvar, evar, loc) =>
      val region = visitExp(region0)
      val fields = fields0.map { case (k, v) => (k, visitExp(v)) }
      val tpe = subst(tvar)
      val eff = subst(evar)
      TypedAst.Exp.StructNew(sym, fields, region, tpe, eff, loc)

    case KindedAst.Exp.StructGet(e0, symUse, tvar, evar, loc) =>
      val e = visitExp(e0)
      val tpe = subst(tvar)
      val eff = subst(evar)
      TypedAst.Exp.StructGet(e, symUse, tpe, eff, loc)

    case KindedAst.Exp.StructPut(exp1, symUse, exp2, tvar, evar, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val tpe = subst(tvar)
      val eff = subst(evar)
      TypedAst.Exp.StructPut(e1, symUse, e2, tpe, eff, loc)

    case KindedAst.Exp.VectorLit(exps, tvar, evar, loc) =>
      val es = exps.map(visitExp(_))
      val tpe = subst(tvar)
      val eff = subst(evar)
      TypedAst.Exp.VectorLit(es, tpe, eff, loc)

    case KindedAst.Exp.VectorLoad(exp1, exp2, tvar, evar, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val tpe = subst(tvar)
      val eff = subst(evar)
      TypedAst.Exp.VectorLoad(e1, e2, tpe, eff, loc)

    case KindedAst.Exp.VectorLength(exp, loc) =>
      val e = visitExp(exp)
      TypedAst.Exp.VectorLength(e, loc)

    case KindedAst.Exp.Ascribe(exp, expectedType, expectedEff, tvar, loc) =>
      val e = visitExp(exp)
      val eff = e.eff
      TypedAst.Exp.Ascribe(e, expectedType, expectedEff, subst(tvar), eff, loc)

    case KindedAst.Exp.InstanceOf(exp, clazz, loc) =>
      val e1 = visitExp(exp)
      TypedAst.Exp.InstanceOf(e1, clazz, loc)

    case KindedAst.Exp.CheckedCast(cast, exp, tvar, evar, loc) =>
      cast match {
        case CheckedCastType.TypeCast =>
          val e = visitExp(exp)
          val tpe = subst(tvar)
          TypedAst.Exp.CheckedCast(cast, e, tpe, e.eff, loc)
        case CheckedCastType.EffectCast =>
          val e = visitExp(exp)
          val eff = Type.mkUnion(e.eff, subst(evar), loc)
          TypedAst.Exp.CheckedCast(cast, e, e.tpe, eff, loc)
      }

    case KindedAst.Exp.UncheckedCast(exp, declaredType0, declaredEff0, tvar, loc) =>
      val e = visitExp(exp)
      val declaredType = declaredType0.map(tpe => subst(tpe))
      val declaredEff = declaredEff0.map(eff => subst(eff))
      val tpe = subst(tvar)
      val eff = declaredEff0.getOrElse(e.eff)
      TypedAst.Exp.UncheckedCast(e, declaredType, declaredEff, tpe, eff, loc)

    case KindedAst.Exp.Unsafe(exp, eff0, loc) =>
      val e = visitExp(exp)
      val eff = Type.mkDifference(e.eff, eff0, loc)
      TypedAst.Exp.Unsafe(e, eff0, e.tpe, eff, loc)

    case KindedAst.Exp.Without(exp, symUse, loc) =>
      val e = visitExp(exp)
      val tpe = e.tpe
      val eff = e.eff
      TypedAst.Exp.Without(e, symUse, tpe, eff, loc)

    case KindedAst.Exp.TryCatch(exp, rules, loc) =>
      val e = visitExp(exp)
      val rs = rules map {
        case KindedAst.CatchRule(sym, clazz, body, ruleLoc) =>
          val b = visitExp(body)
          val bnd = TypedAst.Binder(sym, Type.mkNative(clazz, SourceLocation.Unknown))
          TypedAst.CatchRule(bnd, clazz, b, ruleLoc)
      }
      val tpe = rs.head.exp.tpe
      val eff = Type.mkUnion(e.eff :: rs.map(_.exp.eff), loc)
      TypedAst.Exp.TryCatch(e, rs, tpe, eff, loc)

    case KindedAst.Exp.Throw(exp, tvar, evar, loc) =>
      val e = visitExp(exp)
      val tpe = subst(tvar)
      val eff = subst(evar)
      TypedAst.Exp.Throw(e, tpe, eff, loc)

    case KindedAst.Exp.Handler(effectSymUse, rules, tvar, evar1, evar2, loc) =>
      val rs = rules map {
        case KindedAst.HandlerRule(opSymUse, fparams, hexp, _, ruleLoc) =>
          val fps = fparams.map(visitFormalParam(_, subst))
          val he = visitExp(hexp)
          TypedAst.HandlerRule(opSymUse, fps, he, ruleLoc)
      }
      val bodyTpe = subst(tvar)
      val bodyEff = subst(evar1)
      val handledEff = subst(evar2)
      val tpe = Type.mkArrowWithEffect(Type.mkArrowWithEffect(Type.Unit, bodyEff, bodyTpe, loc.asSynthetic), handledEff, bodyTpe, loc.asSynthetic)
      TypedAst.Exp.Handler(effectSymUse, rs, bodyTpe, bodyEff, handledEff, tpe, loc)

    case KindedAst.Exp.RunWith(exp1, exp2, tvar, evar, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      TypedAst.Exp.RunWith(e1, e2, subst(tvar), Type.mkUnion(subst(evar), e2.eff, loc.asSynthetic), loc)

    case KindedAst.Exp.InvokeConstructor(clazz, exps, jvar, evar, loc) =>
      val es0 = exps.map(visitExp)
      val constructorTpe = subst(jvar)
      val tpe = Type.getFlixType(clazz)
      val eff = subst(evar)
      constructorTpe match {
        case Type.Cst(TypeConstructor.JvmConstructor(constructor), _) =>
          val es = getArgumentsWithVarArgs(constructor, es0, loc)
          TypedAst.Exp.InvokeConstructor(constructor, es, tpe, eff, loc)
        case _ =>
          TypedAst.Exp.Error(TypeError.UnresolvedConstructor(loc), tpe, eff)
      }

    case KindedAst.Exp.InvokeMethod(exp, _, exps, jvar, tvar, evar, loc) =>
      val e = visitExp(exp)
      val es0 = exps.map(visitExp)
      val returnTpe = subst(tvar)
      val methodTpe = subst(jvar)
      val eff = subst(evar)
      methodTpe match {
        case Type.Cst(TypeConstructor.JvmMethod(method), methLoc) =>
          val es = getArgumentsWithVarArgs(method, es0, methLoc)
          TypedAst.Exp.InvokeMethod(method, e, es, returnTpe, eff, methLoc)
        case _ =>
          TypedAst.Exp.Error(TypeError.UnresolvedMethod(loc), methodTpe, eff)
      }

    case KindedAst.Exp.InvokeStaticMethod(_, _, exps, jvar, tvar, evar, loc) =>
      val es0 = exps.map(visitExp)
      val methodTpe = subst(jvar)
      val returnTpe = subst(tvar)
      val eff = subst(evar)
      methodTpe match {
        case Type.Cst(TypeConstructor.JvmMethod(method), methLoc) =>
          val es = getArgumentsWithVarArgs(method, es0, methLoc)
          TypedAst.Exp.InvokeStaticMethod(method, es, returnTpe, eff, methLoc)
        case _ =>
          TypedAst.Exp.Error(TypeError.UnresolvedStaticMethod(loc), methodTpe, eff)
      }

    case KindedAst.Exp.GetField(exp, _, jvar, tvar, evar, loc) =>
      val e = visitExp(exp)
      val fieldType = subst(tvar)
      val jvarType = subst(jvar)
      val eff = subst(evar)
      jvarType match {
        case Type.Cst(TypeConstructor.JvmField(field), fieldLoc) =>
          TypedAst.Exp.GetField(field, e, fieldType, eff, fieldLoc)
        case _ =>
          TypedAst.Exp.Error(TypeError.UnresolvedField(loc), jvarType, eff)
      }

    case KindedAst.Exp.PutField(field, _, exp1, exp2, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val tpe = Type.Unit
      val eff = Type.mkUnion(e1.eff, e2.eff, Type.IO, loc)
      TypedAst.Exp.PutField(field, e1, e2, tpe, eff, loc)

    case KindedAst.Exp.GetStaticField(field, loc) =>
      val tpe = getFlixType(field.getType)
      val eff = Type.IO
      TypedAst.Exp.GetStaticField(field, tpe, eff, loc)

    case KindedAst.Exp.PutStaticField(field, exp, loc) =>
      val e = visitExp(exp)
      val tpe = Type.Unit
      val eff = Type.mkUnion(e.eff, Type.IO, loc)
      TypedAst.Exp.PutStaticField(field, e, tpe, eff, loc)

    case KindedAst.Exp.NewObject(name, clazz, methods, loc) =>
      val tpe = getFlixType(clazz)
      val eff = Type.IO
      val ms = methods map visitJvmMethod
      TypedAst.Exp.NewObject(name, clazz, tpe, eff, ms, loc)

    case KindedAst.Exp.NewChannel(exp, tvar, loc) =>
      val e = visitExp(exp)
      val eff = Type.mkUnion(e.eff, Type.Chan, loc)
      TypedAst.Exp.NewChannel(e, subst(tvar), eff, loc)

    case KindedAst.Exp.GetChannel(exp, tvar, evar, loc) =>
      val e = visitExp(exp)
      TypedAst.Exp.GetChannel(e, subst(tvar), subst(evar), loc)

    case KindedAst.Exp.PutChannel(exp1, exp2, evar, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val tpe = Type.mkUnit(loc)
      TypedAst.Exp.PutChannel(e1, e2, tpe, subst(evar), loc)

    case KindedAst.Exp.SelectChannel(rules, default, tvar, evar, loc) =>
      val rs = rules map {
        case KindedAst.SelectChannelRule(sym, chan, exp, ruleLoc) =>
          val c = visitExp(chan)
          val b = visitExp(exp)
          val bnd = TypedAst.Binder(sym, c.tpe)
          TypedAst.SelectChannelRule(bnd, c, b, ruleLoc)
      }
      val d = default.map(visitExp(_))
      TypedAst.Exp.SelectChannel(rs, d, subst(tvar), subst(evar), loc)

    case KindedAst.Exp.Spawn(exp1, exp2, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val tpe = Type.Unit
      val eff = Type.mkUnion(e1.eff, e2.eff, loc)
      TypedAst.Exp.Spawn(e1, e2, tpe, eff, loc)

    case KindedAst.Exp.ParYield(frags, exp, loc) =>
      val e = visitExp(exp)
      val fs = frags map {
        case KindedAst.ParYieldFragment(pat, e0, l0) =>
          val p = visitPattern(pat)
          val e1 = visitExp(e0)
          TypedAst.ParYieldFragment(p, e1, l0)
      }
      val tpe = e.tpe
      val eff = fs.foldLeft(e.eff) {
        case (acc, TypedAst.ParYieldFragment(_, e1, _)) => Type.mkUnion(acc, e1.eff, loc)
      }
      TypedAst.Exp.ParYield(fs, e, tpe, eff, loc)

    case KindedAst.Exp.Lazy(exp, loc) =>
      val e = visitExp(exp)
      val tpe = Type.mkLazy(e.tpe, loc)
      TypedAst.Exp.Lazy(e, tpe, loc)

    case KindedAst.Exp.Force(exp, tvar, loc) =>
      val e = visitExp(exp)
      val tpe = subst(tvar)
      val eff = e.eff
      TypedAst.Exp.Force(e, tpe, eff, loc)

    case KindedAst.Exp.FixpointConstraintSet(cs0, tvar, loc) =>
      val cs = cs0.map(visitConstraint)
      TypedAst.Exp.FixpointConstraintSet(cs, subst(tvar), loc)

    case KindedAst.Exp.FixpointLambda(pparams, exp, tvar, loc) =>
      val ps = pparams.map(visitPredicateParam)
      val e = visitExp(exp)
      val tpe = subst(tvar)
      val eff = e.eff
      TypedAst.Exp.FixpointLambda(ps, e, tpe, eff, loc)

    case KindedAst.Exp.FixpointMerge(exp1, exp2, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val tpe = e1.tpe
      val eff = Type.mkUnion(e1.eff, e2.eff, loc)
      TypedAst.Exp.FixpointMerge(e1, e2, tpe, eff, loc)

    case KindedAst.Exp.FixpointQueryWithProvenance(exps, select, withh, tvar, loc) =>
      val es = exps.map(visitExp)
      val s = visitHeadPredicate(select)
      val eff = Type.mkUnion(es.map(_.eff), loc)
      TypedAst.Exp.FixpointQueryWithProvenance(es, s, withh, subst(tvar), eff, loc)

    case KindedAst.Exp.FixpointQueryWithSelect(exps, queryExp, selects, from, where, pred, tvar, loc) =>
      val es = exps.map(visitExp)
      val qe = visitExp(queryExp)
      val ss = selects.map(visitExp)
      val f = from.map(visitBodyPredicate)
      val w = where.map(visitExp)
      val effs = es.map(_.eff) ::: ss.map(_.eff) ::: w.map(_.eff)
      val eff = Type.mkUnion(effs, loc)
      TypedAst.Exp.FixpointQueryWithSelect(es, qe, ss, f, w, pred, subst(tvar), eff, loc)

    case KindedAst.Exp.FixpointSolveWithProject(exps, optPreds, mode, tvar, loc) =>
      val es = exps.map(visitExp)
      val tpe = subst(tvar)
      val eff = Type.mkUnion(es.map(_.eff), loc)
      TypedAst.Exp.FixpointSolveWithProject(es, optPreds, mode, tpe, eff, loc)

    case KindedAst.Exp.FixpointInjectInto(exps, predsAndArities, tvar, evar, loc) =>
      val es = exps.map(visitExp)
      TypedAst.Exp.FixpointInjectInto(es, predsAndArities, subst(tvar), subst(evar), loc)

    case KindedAst.Exp.Error(m, tvar, evar) =>
      val tpe = subst(tvar)
      val eff = subst(evar)
      TypedAst.Exp.Error(m, tpe, eff)
  }

  /**
    * Returns the given arguments `es` possibly with an empty VarArgs array added as the last argument.
    */
  private def getArgumentsWithVarArgs(exc: Executable, es: List[TypedAst.Exp], loc: SourceLocation): List[TypedAst.Exp] = {
    val declaredArity = exc.getParameterCount
    val actualArity = es.length
    // Check if (a) an argument is missing and (b) the constructor/method is VarArgs.
    if (actualArity == declaredArity - 1 && exc.isVarArgs) {
      // Case 1: Argument missing. Introduce a new empty vector argument.
      val varArgsType = Type.mkNative(exc.getParameterTypes.last.getComponentType, loc)
      val varArgs = TypedAst.Exp.VectorLit(Nil, Type.mkVector(varArgsType, loc), Type.Pure, loc)
      es ::: varArgs :: Nil
    } else {
      // Case 2: No argument missing. Return the arguments as-is.
      es
    }
  }

  /**
    * Applies the substitution to the given constraint.
    */
  private def visitConstraint(c0: KindedAst.Constraint)(implicit subst: SubstitutionTree): TypedAst.Constraint = {
    val KindedAst.Constraint(cparams0, head0, body0, loc) = c0

    val head = visitHeadPredicate(head0)
    val body = body0.map(b => visitBodyPredicate(b))

    val cparams = cparams0.map {
      case KindedAst.ConstraintParam(sym, l) =>
        val tpe = subst(sym.tvar)
        val bnd = TypedAst.Binder(sym, tpe)
        TypedAst.ConstraintParam(bnd, tpe, l)
    }

    TypedAst.Constraint(cparams, head, body, loc)
  }

  /**
    * Reconstructs types in the given predicate param.
    */
  private def visitPredicateParam(pparam: KindedAst.PredicateParam)(implicit subst: SubstitutionTree): TypedAst.PredicateParam =
    TypedAst.PredicateParam(pparam.pred, subst(pparam.tpe), pparam.loc)

  /**
    * Reconstructs types in the given JVM method.
    */
  private def visitJvmMethod(method: KindedAst.JvmMethod)(implicit subst: SubstitutionTree): TypedAst.JvmMethod = {
    method match {
      case KindedAst.JvmMethod(ident, fparams0, exp0, tpe, eff, loc) =>
        val fparams = fparams0.map(visitFormalParam(_, subst))
        val exp = visitExp(exp0)
        TypedAst.JvmMethod(ident, fparams, exp, tpe, eff, loc)
    }
  }

  /**
    * Reconstructs types in the given ext-match rule.
    */
  private def visitExtMatchRule(rule: KindedAst.ExtMatchRule)(implicit subst: SubstitutionTree): TypedAst.ExtMatchRule = rule match {
    case KindedAst.ExtMatchRule(pat, exp, loc) =>
      val p = visitExtPat(pat)
      val e = visitExp(exp)
      TypedAst.ExtMatchRule(p, e, loc)
  }

  /**
    * Reconstructs types in the given pattern.
    */
  private def visitPattern(pat0: KindedAst.Pattern)(implicit subst: SubstitutionTree): TypedAst.Pattern = pat0 match {
    case KindedAst.Pattern.Wild(tvar, loc) => TypedAst.Pattern.Wild(subst(tvar), loc)
    case KindedAst.Pattern.Var(sym, tvar, loc) => TypedAst.Pattern.Var(TypedAst.Binder(sym, subst(tvar)), subst(tvar), loc)
    case KindedAst.Pattern.Cst(cst, loc) => TypedAst.Pattern.Cst(cst, Type.constantType(cst), loc)

    case KindedAst.Pattern.Tag(symUse, pats, tvar, loc) => TypedAst.Pattern.Tag(symUse, pats.map(visitPattern), subst(tvar), loc)

    case KindedAst.Pattern.Tuple(elms, loc) =>
      val es = elms.map(visitPattern)
      val tpe = Type.mkTuple(es.map(_.tpe), loc)
      TypedAst.Pattern.Tuple(es, tpe, loc)

    case KindedAst.Pattern.Record(pats, pat, tvar, loc) =>
      val ps = pats.map {
        case KindedAst.Pattern.Record.RecordLabelPattern(field, pat1, tvar1, loc1) =>
          TypedAst.Pattern.Record.RecordLabelPattern(field, visitPattern(pat1), subst(tvar1), loc1)
      }
      val p = visitPattern(pat)
      TypedAst.Pattern.Record(ps, p, subst(tvar), loc)

    case KindedAst.Pattern.Error(tvar, loc) =>
      TypedAst.Pattern.Error(subst(tvar), loc)
  }

  /**
    * Reconstructs types in the given ext pattern.
    */
  private def visitExtPat(pat0: KindedAst.ExtPattern)(implicit subst: SubstitutionTree): TypedAst.ExtPattern = pat0 match {
    case KindedAst.ExtPattern.Default(_, loc) =>
      TypedAst.ExtPattern.Default(loc)

    case KindedAst.ExtPattern.Tag(label, pats, loc) =>
      val ps = pats.map(visitExtTagPat)
      TypedAst.ExtPattern.Tag(label, ps, loc)

    case KindedAst.ExtPattern.Error(_, loc) =>
      TypedAst.ExtPattern.Error(loc)
  }


  /**
    * Reconstructs types in the given ext tag pattern.
    */
  private def visitExtTagPat(pat0: KindedAst.ExtTagPattern)(implicit subst: SubstitutionTree): TypedAst.ExtTagPattern = pat0 match {
    case KindedAst.ExtTagPattern.Wild(tvar, loc) =>
      TypedAst.ExtTagPattern.Wild(subst(tvar), loc)

    case KindedAst.ExtTagPattern.Var(sym, tvar, loc) =>
      val tpe = subst(tvar)
      val bnd = TypedAst.Binder(sym, tpe)
      TypedAst.ExtTagPattern.Var(bnd, tpe, loc)

    case KindedAst.ExtTagPattern.Unit(loc) =>
      TypedAst.ExtTagPattern.Unit(Type.Unit, loc)

    case KindedAst.ExtTagPattern.Error(tvar, loc) =>
      TypedAst.ExtTagPattern.Error(subst(tvar), loc)
  }

  /**
    * Reconstructs types in the given head predicate.
    */
  private def visitHeadPredicate(head0: KindedAst.Predicate.Head)(implicit subst: SubstitutionTree): TypedAst.Predicate.Head = head0 match {
    case KindedAst.Predicate.Head.Atom(pred, den0, terms, tvar, loc) =>
      val ts = terms.map(t => visitExp(t))
      TypedAst.Predicate.Head.Atom(pred, den0, ts, subst(tvar), loc)
  }


  /**
    * Reconstructs types in the given body predicate.
    */
  private def visitBodyPredicate(body0: KindedAst.Predicate.Body)(implicit subst: SubstitutionTree): TypedAst.Predicate.Body = body0 match {
    case KindedAst.Predicate.Body.Atom(pred, den0, polarity, fixity, terms, tvar, loc) =>
      val ts = terms.map(t => visitPattern(t))
      TypedAst.Predicate.Body.Atom(pred, den0, polarity, fixity, ts, subst(tvar), loc)

    case KindedAst.Predicate.Body.Functional(syms, exp, loc) =>
      val e = visitExp(exp)
      val outBnds = syms.map(varSym => TypedAst.Binder(varSym, subst(varSym.tvar)))
      TypedAst.Predicate.Body.Functional(outBnds, e, loc)

    case KindedAst.Predicate.Body.Guard(exp, loc) =>
      val e = visitExp(exp)
      TypedAst.Predicate.Body.Guard(e, loc)

  }
}
