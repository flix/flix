/*
 * Copyright 2015-2023 Magnus Madsen, Matthew Lutze
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
import ca.uwaterloo.flix.language.ast.Ast.{CheckedCastType, Constant, LabelledPrecedenceGraph, Stratification}
import ca.uwaterloo.flix.language.ast.Type.getFlixType
import ca.uwaterloo.flix.language.ast.{Ast, ChangeSet, KindedAst, SourceLocation, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.errors.TypeError
import ca.uwaterloo.flix.language.phase.unification.Substitution
import ca.uwaterloo.flix.util.collection.ListMap
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps, Validation}

object TypeReconstruction {

  /**
    * Reconstructs types in the given def.
    */
  def visitDef(defn: KindedAst.Def, root: KindedAst.Root, subst: Substitution): TypedAst.Def = defn match {
    case KindedAst.Def(sym, spec0, exp0) =>
      val spec = visitSpec(spec0, root, subst)
      val exp = visitExp(exp0)(root, subst)
      TypedAst.Def(sym, spec, exp)
  }

  /**
    * Reconstructs types in the given sig.
    */
  def visitSig(sig: KindedAst.Sig, root: KindedAst.Root, subst: Substitution): TypedAst.Sig = sig match {
    case KindedAst.Sig(sym, spec0, exp0) =>
      val spec = visitSpec(spec0, root, subst)
      val exp = exp0.map(visitExp(_)(root, subst))
      TypedAst.Sig(sym, spec, exp)
  }

  /**
    * Reconstructs types in the given spec.
    */
  private def visitSpec(spec: KindedAst.Spec, root: KindedAst.Root, subst: Substitution): TypedAst.Spec = spec match {
    case KindedAst.Spec(doc, ann, mod, tparams0, fparams0, sc0, tpe0, eff0, tconstrs0, econstrs0, loc) =>
      val tparams = tparams0.map(visitTypeParam(_, root))
      val fparams = fparams0.map(visitFormalParam(_, root, subst))
      val tpe = subst(tpe0)
      val eff = subst(eff0)
      val tconstrs = tconstrs0.map(subst.apply)
      val econstrs = econstrs0.map(subst.apply)
      val sc = sc0 // TODO ASSOC-TYPES get rid of type visits here and elsewhere that only go over rigid tvars
      TypedAst.Spec(doc, ann, mod, tparams, fparams, sc, tpe, eff, tconstrs, econstrs, loc)
  }

  /**
    * Reconstructs types in the given tparams.
    */
  private def visitTypeParam(tparam: KindedAst.TypeParam, root: KindedAst.Root): TypedAst.TypeParam = tparam match {
    case KindedAst.TypeParam(name, sym, loc) => TypedAst.TypeParam(name, sym, loc)
  }

  /**
    * Reconstructs types in the given fparams.
    */
  private def visitFormalParam(fparam: KindedAst.FormalParam, root: KindedAst.Root, subst: Substitution): TypedAst.FormalParam = fparam match {
    case KindedAst.FormalParam(sym, mod, tpe0, src, loc) =>
      val tpe = subst(tpe0)
      TypedAst.FormalParam(sym, mod, tpe, src, loc)
  }

  /**
    * Reconstructs types in the given operation.
    */
  def visitOp(op: KindedAst.Op, root: KindedAst.Root)(implicit flix: Flix): TypedAst.Op = op match {
    case KindedAst.Op(sym, spec0) =>
      val spec = visitSpec(spec0, root, Substitution.empty)
      TypedAst.Op(sym, spec)
  }

  /**
    * Reconstructs types in the given expression.
    */
  private def visitExp(exp0: KindedAst.Expr)(implicit root: KindedAst.Root, subst: Substitution): TypedAst.Expr = exp0 match {
    case KindedAst.Expr.Var(sym, loc) =>
      TypedAst.Expr.Var(sym, subst(sym.tvar), loc)

    case KindedAst.Expr.Def(sym, tvar, loc) =>
      TypedAst.Expr.Def(sym, subst(tvar), loc)

    case KindedAst.Expr.Sig(sym, tvar, loc) =>
      TypedAst.Expr.Sig(sym, subst(tvar), loc)

    case KindedAst.Expr.Hole(sym, tpe, loc) =>
      TypedAst.Expr.Hole(sym, subst(tpe), loc)

    case KindedAst.Expr.HoleWithExp(exp, tvar, pvar, loc) =>
      val e = visitExp(exp)
      TypedAst.Expr.HoleWithExp(e, subst(tvar), subst(pvar), loc)

    case KindedAst.Expr.OpenAs(sym, exp, tvar, loc) =>
      val e = visitExp(exp)
      TypedAst.Expr.OpenAs(sym, e, subst(tvar), loc)

    case KindedAst.Expr.Use(sym, alias, exp, loc) =>
      val e = visitExp(exp)
      TypedAst.Expr.Use(sym, alias, e, loc)

    case KindedAst.Expr.Cst(Ast.Constant.Null, loc) =>
      TypedAst.Expr.Cst(Ast.Constant.Null, Type.Null, loc)

    case KindedAst.Expr.Cst(cst, loc) => TypedAst.Expr.Cst(cst, constantType(cst), loc)

    case KindedAst.Expr.Apply(exp, exps, tvar, pvar, loc) =>
      val e = visitExp(exp)
      val es = exps.map(visitExp(_))
      TypedAst.Expr.Apply(e, es, subst(tvar), subst(pvar), loc)

    case KindedAst.Expr.Lambda(fparam, exp, loc) =>
      val p = visitFormalParam(fparam, root, subst)
      val e = visitExp(exp)
      val t = Type.mkArrowWithEffect(p.tpe, e.eff, e.tpe, loc)
      TypedAst.Expr.Lambda(p, e, t, loc)

    case KindedAst.Expr.Unary(sop, exp, tvar, loc) =>
      val e = visitExp(exp)
      val eff = e.eff
      TypedAst.Expr.Unary(sop, e, subst(tvar), eff, loc)

    case KindedAst.Expr.Binary(sop, exp1, exp2, tvar, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val eff = Type.mkUnion(e1.eff, e2.eff, loc)
      TypedAst.Expr.Binary(sop, e1, e2, subst(tvar), eff, loc)

    case KindedAst.Expr.IfThenElse(exp1, exp2, exp3, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val e3 = visitExp(exp3)
      val tpe = e2.tpe
      val eff = Type.mkUnion(e1.eff, e2.eff, e3.eff, loc)
      TypedAst.Expr.IfThenElse(e1, e2, e3, tpe, eff, loc)

    case KindedAst.Expr.Stm(exp1, exp2, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val tpe = e2.tpe
      val eff = Type.mkUnion(e1.eff, e2.eff, loc)
      TypedAst.Expr.Stm(e1, e2, tpe, eff, loc)

    case KindedAst.Expr.Discard(exp, loc) =>
      val e = visitExp(exp)
      TypedAst.Expr.Discard(e, e.eff, loc)

    case KindedAst.Expr.Let(sym, mod, exp1, exp2, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val tpe = e2.tpe
      val eff = Type.mkUnion(e1.eff, e2.eff, loc)
      TypedAst.Expr.Let(sym, mod, e1, e2, tpe, eff, loc)

    case KindedAst.Expr.LetRec(sym, ann, mod, exp1, exp2, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val tpe = e2.tpe
      val eff = Type.mkUnion(e1.eff, e2.eff, loc)
      TypedAst.Expr.LetRec(sym, ann, mod, e1, e2, tpe, eff, loc)

    case KindedAst.Expr.Region(tpe, loc) =>
      TypedAst.Expr.Region(tpe, loc)

    case KindedAst.Expr.Scope(sym, regionVar, exp, pvar, loc) =>
      val e = visitExp(exp)
      val tpe = e.tpe
      val eff = subst(pvar)
      TypedAst.Expr.Scope(sym, regionVar, e, tpe, eff, loc)

    case KindedAst.Expr.ScopeExit(exp1, exp2, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val tpe = Type.Unit
      val eff = Type.Impure
      TypedAst.Expr.ScopeExit(e1, e2, tpe, eff, loc)

    case KindedAst.Expr.Match(matchExp, rules, loc) =>
      val e1 = visitExp(matchExp)
      val rs = rules map {
        case KindedAst.MatchRule(pat, guard, exp) =>
          val p = visitPattern(pat)
          val g = guard.map(visitExp(_))
          val b = visitExp(exp)
          TypedAst.MatchRule(p, g, b)
      }
      val tpe = rs.head.exp.tpe
      val eff = rs.foldLeft(e1.eff) {
        case (acc, TypedAst.MatchRule(_, g, b)) => Type.mkUnion(g.map(_.eff).toList ::: List(b.eff, acc), loc)
      }
      TypedAst.Expr.Match(e1, rs, tpe, eff, loc)

    case KindedAst.Expr.TypeMatch(matchExp, rules, loc) =>
      val e1 = visitExp(matchExp)
      val rs = rules map {
        case KindedAst.TypeMatchRule(sym, tpe0, exp) =>
          val t = subst(tpe0)
          val b = visitExp(exp)
          TypedAst.TypeMatchRule(sym, t, b)
      }
      val tpe = rs.head.exp.tpe
      val eff = rs.foldLeft(e1.eff) {
        case (acc, TypedAst.TypeMatchRule(_, _, b)) => Type.mkUnion(b.eff, acc, loc)
      }
      TypedAst.Expr.TypeMatch(e1, rs, tpe, eff, loc)

    case KindedAst.Expr.RestrictableChoose(star, exp, rules, tvar, loc) =>
      val e = visitExp(exp)
      val rs = rules.map {
        case KindedAst.RestrictableChooseRule(pat0, body0) =>
          val pat = pat0 match {
            case KindedAst.RestrictableChoosePattern.Tag(sym, pats, tvar, loc) =>
              val ps = pats.map {
                case KindedAst.RestrictableChoosePattern.Wild(tvar, loc) => TypedAst.RestrictableChoosePattern.Wild(subst(tvar), loc)
                case KindedAst.RestrictableChoosePattern.Var(sym, tvar, loc) => TypedAst.RestrictableChoosePattern.Var(sym, subst(tvar), loc)
              }
              TypedAst.RestrictableChoosePattern.Tag(sym, ps, subst(tvar), loc)
          }
          val body = visitExp(body0)
          TypedAst.RestrictableChooseRule(pat, body)
      }
      val eff = Type.mkUnion(rs.map(_.exp.eff), loc)
      TypedAst.Expr.RestrictableChoose(star, e, rs, subst(tvar), eff, loc)

    case KindedAst.Expr.Tag(sym, exp, tvar, loc) =>
      val e = visitExp(exp)
      val eff = e.eff
      TypedAst.Expr.Tag(sym, e, subst(tvar), eff, loc)

    case KindedAst.Expr.RestrictableTag(sym, exp, _, tvar, loc) =>
      val e = visitExp(exp)
      val eff = e.eff
      TypedAst.Expr.RestrictableTag(sym, e, subst(tvar), eff, loc)

    case KindedAst.Expr.Tuple(elms, loc) =>
      val es = elms.map(visitExp(_))
      val tpe = Type.mkTuple(es.map(_.tpe), loc)
      val eff = Type.mkUnion(es.map(_.eff), loc)
      TypedAst.Expr.Tuple(es, tpe, eff, loc)

    case KindedAst.Expr.RecordEmpty(loc) =>
      TypedAst.Expr.RecordEmpty(Type.mkRecord(Type.RecordRowEmpty, loc), loc)

    case KindedAst.Expr.RecordSelect(exp, field, tvar, loc) =>
      val e = visitExp(exp)
      val eff = e.eff
      TypedAst.Expr.RecordSelect(e, field, subst(tvar), eff, loc)

    case KindedAst.Expr.RecordExtend(field, value, rest, tvar, loc) =>
      val v = visitExp(value)
      val r = visitExp(rest)
      val eff = Type.mkUnion(v.eff, r.eff, loc)
      TypedAst.Expr.RecordExtend(field, v, r, subst(tvar), eff, loc)

    case KindedAst.Expr.RecordRestrict(field, rest, tvar, loc) =>
      val r = visitExp(rest)
      val eff = r.eff
      TypedAst.Expr.RecordRestrict(field, r, subst(tvar), eff, loc)

    case KindedAst.Expr.ArrayLit(exps, exp, tvar, pvar, loc) =>
      val es = exps.map(visitExp(_))
      val e = visitExp(exp)
      val tpe = subst(tvar)
      val eff = subst(pvar)
      TypedAst.Expr.ArrayLit(es, e, tpe, eff, loc)

    case KindedAst.Expr.ArrayNew(exp1, exp2, exp3, tvar, pvar, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val e3 = visitExp(exp3)
      val tpe = subst(tvar)
      val eff = subst(pvar)
      TypedAst.Expr.ArrayNew(e1, e2, e3, tpe, eff, loc)

    case KindedAst.Expr.ArrayLoad(exp1, exp2, tvar, pvar, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val tpe = subst(tvar)
      val eff = subst(pvar)
      TypedAst.Expr.ArrayLoad(e1, e2, tpe, eff, loc)

    case KindedAst.Expr.ArrayStore(exp1, exp2, exp3, pvar, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val e3 = visitExp(exp3)
      val eff = subst(pvar)
      TypedAst.Expr.ArrayStore(e1, e2, e3, eff, loc)

    case KindedAst.Expr.ArrayLength(exp, loc) =>
      val e = visitExp(exp)
      val eff = e.eff
      TypedAst.Expr.ArrayLength(e, eff, loc)

    case KindedAst.Expr.VectorLit(exps, tvar, pvar, loc) =>
      val es = exps.map(visitExp(_))
      val tpe = subst(tvar)
      val eff = subst(pvar)
      TypedAst.Expr.VectorLit(es, tpe, eff, loc)

    case KindedAst.Expr.VectorLoad(exp1, exp2, tvar, pvar, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val tpe = subst(tvar)
      val eff = subst(pvar)
      TypedAst.Expr.VectorLoad(e1, e2, tpe, eff, loc)

    case KindedAst.Expr.VectorLength(exp, loc) =>
      val e = visitExp(exp)
      val eff = e.eff
      TypedAst.Expr.VectorLength(e, loc)

    case KindedAst.Expr.Ref(exp1, exp2, tvar, pvar, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val tpe = subst(tvar)
      val eff = subst(pvar)
      TypedAst.Expr.Ref(e1, e2, tpe, eff, loc)

    case KindedAst.Expr.Deref(exp, tvar, pvar, loc) =>
      val e = visitExp(exp)
      val tpe = subst(tvar)
      val eff = subst(pvar)
      TypedAst.Expr.Deref(e, tpe, eff, loc)

    case KindedAst.Expr.Assign(exp1, exp2, pvar, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val tpe = Type.Unit
      val eff = subst(pvar)
      TypedAst.Expr.Assign(e1, e2, tpe, eff, loc)

    case KindedAst.Expr.Ascribe(exp, _, _, tvar, loc) =>
      val e = visitExp(exp)
      val eff = e.eff
      TypedAst.Expr.Ascribe(e, subst(tvar), eff, loc)

    case KindedAst.Expr.InstanceOf(exp, clazz, loc) =>
      val e1 = visitExp(exp)
      TypedAst.Expr.InstanceOf(e1, clazz, loc)

    case KindedAst.Expr.CheckedCast(cast, exp, tvar, pvar, loc) =>
      cast match {
        case CheckedCastType.TypeCast =>
          val e = visitExp(exp)
          val tpe = subst(tvar)
          TypedAst.Expr.CheckedCast(cast, e, tpe, e.eff, loc)
        case CheckedCastType.EffectCast =>
          val e = visitExp(exp)
          val eff = Type.mkUnion(e.eff, subst(pvar), loc)
          TypedAst.Expr.CheckedCast(cast, e, e.tpe, eff, loc)
      }

    case KindedAst.Expr.UncheckedCast(KindedAst.Expr.Cst(Ast.Constant.Null, _), _, _, tvar, loc) =>
      val t = subst(tvar)
      TypedAst.Expr.Cst(Ast.Constant.Null, t, loc)

    case KindedAst.Expr.UncheckedCast(exp, declaredType, declaredEff, tvar, loc) =>
      val e = visitExp(exp)
      val dt = declaredType.map(tpe => subst(tpe))
      val dp = declaredEff.map(eff => subst(eff))
      val tpe = subst(tvar)
      val eff = declaredEff.getOrElse(e.eff)
      TypedAst.Expr.UncheckedCast(e, dt, dp, tpe, eff, loc)

    case KindedAst.Expr.UncheckedMaskingCast(exp, loc) =>
      // We explicitly mark a `Mask` expression as Impure.
      val e = visitExp(exp)
      val tpe = e.tpe
      val eff = Type.Impure
      TypedAst.Expr.UncheckedMaskingCast(e, tpe, eff, loc)

    case KindedAst.Expr.Without(exp, effUse, loc) =>
      val e = visitExp(exp)
      val tpe = e.tpe
      val eff = e.eff
      TypedAst.Expr.Without(e, effUse, tpe, eff, loc)

    case KindedAst.Expr.TryCatch(exp, rules, loc) =>
      val e = visitExp(exp)
      val rs = rules map {
        case KindedAst.CatchRule(sym, clazz, body) =>
          val b = visitExp(body)
          TypedAst.CatchRule(sym, clazz, b)
      }
      val tpe = rs.head.exp.tpe
      val eff = Type.mkUnion(e.eff :: rs.map(_.exp.eff), loc)
      TypedAst.Expr.TryCatch(e, rs, tpe, eff, loc)

    case KindedAst.Expr.TryWith(exp, effUse, rules, tvar, loc) =>
      val e = visitExp(exp)
      val rs = rules map {
        case KindedAst.HandlerRule(op, fparams, hexp, htvar) =>
          val fps = fparams.map(visitFormalParam(_, root, subst))
          val he = visitExp(hexp)
          TypedAst.HandlerRule(op, fps, he)
      }
      val tpe = subst(tvar)
      val eff = Type.mkUnion(e.eff :: rs.map(_.exp.eff), loc)
      TypedAst.Expr.TryWith(e, effUse, rs, tpe, eff, loc)

    case KindedAst.Expr.Do(op, exps, tvar, loc) =>
      val es = exps.map(visitExp(_))
      val tpe = subst(tvar)
      val eff1 = Type.Cst(TypeConstructor.Effect(op.sym.eff), op.loc.asSynthetic)
      val eff = Type.mkUnion(eff1 :: es.map(_.eff), loc)
      TypedAst.Expr.Do(op, es, tpe, eff, loc)

    case KindedAst.Expr.Resume(exp, _, retTvar, loc) =>
      val e = visitExp(exp)
      val tpe = subst(retTvar)
      TypedAst.Expr.Resume(e, tpe, loc)

    case KindedAst.Expr.InvokeConstructor(constructor, args, loc) =>
      val as = args.map(visitExp(_))
      val tpe = getFlixType(constructor.getDeclaringClass)
      val eff = Type.Impure
      TypedAst.Expr.InvokeConstructor(constructor, as, tpe, eff, loc)

    case KindedAst.Expr.InvokeMethod(method, _, exp, args, loc) =>
      val e = visitExp(exp)
      val as = args.map(visitExp(_))
      val tpe = getFlixType(method.getReturnType)
      val eff = Type.Impure
      TypedAst.Expr.InvokeMethod(method, e, as, tpe, eff, loc)

    case KindedAst.Expr.InvokeStaticMethod(method, args, loc) =>
      val as = args.map(visitExp(_))
      val tpe = getFlixType(method.getReturnType)
      val eff = Type.Impure
      TypedAst.Expr.InvokeStaticMethod(method, as, tpe, eff, loc)

    case KindedAst.Expr.GetField(field, _, exp, loc) =>
      val e = visitExp(exp)
      val tpe = getFlixType(field.getType)
      val eff = Type.Impure
      TypedAst.Expr.GetField(field, e, tpe, eff, loc)

    case KindedAst.Expr.PutField(field, _, exp1, exp2, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val tpe = Type.Unit
      val eff = Type.Impure
      TypedAst.Expr.PutField(field, e1, e2, tpe, eff, loc)

    case KindedAst.Expr.GetStaticField(field, loc) =>
      val tpe = getFlixType(field.getType)
      val eff = Type.Impure
      TypedAst.Expr.GetStaticField(field, tpe, eff, loc)

    case KindedAst.Expr.PutStaticField(field, exp, loc) =>
      val e = visitExp(exp)
      val tpe = Type.Unit
      val eff = Type.Impure
      TypedAst.Expr.PutStaticField(field, e, tpe, eff, loc)

    case KindedAst.Expr.NewObject(name, clazz, methods, loc) =>
      val tpe = getFlixType(clazz)
      val eff = Type.Impure
      val ms = methods map visitJvmMethod
      TypedAst.Expr.NewObject(name, clazz, tpe, eff, ms, loc)

    case KindedAst.Expr.NewChannel(exp1, exp2, tvar, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val eff = Type.Impure
      TypedAst.Expr.NewChannel(e1, e2, subst(tvar), eff, loc)

    case KindedAst.Expr.GetChannel(exp, tvar, loc) =>
      val e = visitExp(exp)
      val eff = Type.Impure
      TypedAst.Expr.GetChannel(e, subst(tvar), eff, loc)

    case KindedAst.Expr.PutChannel(exp1, exp2, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val tpe = Type.mkUnit(loc)
      val eff = Type.Impure
      TypedAst.Expr.PutChannel(e1, e2, tpe, eff, loc)

    case KindedAst.Expr.SelectChannel(rules, default, tvar, loc) =>
      val rs = rules map {
        case KindedAst.SelectChannelRule(sym, chan, exp) =>
          val c = visitExp(chan)
          val b = visitExp(exp)
          TypedAst.SelectChannelRule(sym, c, b)
      }
      val d = default.map(visitExp(_))
      val eff = Type.Impure
      TypedAst.Expr.SelectChannel(rs, d, subst(tvar), eff, loc)

    case KindedAst.Expr.Spawn(exp1, exp2, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val tpe = Type.Unit
      val eff = Type.Impure
      TypedAst.Expr.Spawn(e1, e2, tpe, eff, loc)

    case KindedAst.Expr.ParYield(frags, exp, loc) =>
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
      TypedAst.Expr.ParYield(fs, e, tpe, eff, loc)

    case KindedAst.Expr.Lazy(exp, loc) =>
      val e = visitExp(exp)
      val tpe = Type.mkLazy(e.tpe, loc)
      TypedAst.Expr.Lazy(e, tpe, loc)

    case KindedAst.Expr.Force(exp, tvar, loc) =>
      val e = visitExp(exp)
      val tpe = subst(tvar)
      val eff = e.eff
      TypedAst.Expr.Force(e, tpe, eff, loc)

    case KindedAst.Expr.FixpointConstraintSet(cs0, tvar, loc) =>
      val cs = cs0.map(visitConstraint)
      TypedAst.Expr.FixpointConstraintSet(cs, subst(tvar), loc)

    case KindedAst.Expr.FixpointLambda(pparams, exp, tvar, loc) =>
      val ps = pparams.map(visitPredicateParam)
      val e = visitExp(exp)
      val tpe = subst(tvar)
      val eff = e.eff
      TypedAst.Expr.FixpointLambda(ps, e, tpe, eff, loc)

    case KindedAst.Expr.FixpointMerge(exp1, exp2, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val tpe = e1.tpe
      val eff = Type.mkUnion(e1.eff, e2.eff, loc)
      TypedAst.Expr.FixpointMerge(e1, e2, tpe, eff, loc)

    case KindedAst.Expr.FixpointSolve(exp, loc) =>
      val e = visitExp(exp)
      val tpe = e.tpe
      val eff = e.eff
      TypedAst.Expr.FixpointSolve(e, tpe, eff, loc)

    case KindedAst.Expr.FixpointFilter(pred, exp, tvar, loc) =>
      val e = visitExp(exp)
      val eff = e.eff
      TypedAst.Expr.FixpointFilter(pred, e, subst(tvar), eff, loc)

    case KindedAst.Expr.FixpointInject(exp, pred, tvar, loc) =>
      val e = visitExp(exp)
      val eff = e.eff
      TypedAst.Expr.FixpointInject(e, pred, subst(tvar), eff, loc)

    case KindedAst.Expr.FixpointProject(pred, exp1, exp2, tvar, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val tpe = subst(tvar)
      val eff = Type.mkUnion(e1.eff, e2.eff, loc)

      // Note: This transformation should happen in the Weeder but it is here because
      // `#{#Result(..)` | _} cannot be unified with `#{A(..)}` (a closed row).
      // See Weeder for more details.
      val mergeExp = TypedAst.Expr.FixpointMerge(e1, e2, e1.tpe, eff, loc)
      val solveExp = TypedAst.Expr.FixpointSolve(mergeExp, e1.tpe, eff, loc)
      TypedAst.Expr.FixpointProject(pred, solveExp, tpe, eff, loc)

    case KindedAst.Expr.Error(m, tvar, pvar) =>
      val tpe = subst(tvar)
      val eff = subst(pvar)
      TypedAst.Expr.Error(m, tpe, eff)
  }

  /**
    * Applies the substitution to the given constraint.
    */
  private def visitConstraint(c0: KindedAst.Constraint)(implicit root: KindedAst.Root, subst: Substitution): TypedAst.Constraint = {
    val KindedAst.Constraint(cparams0, head0, body0, loc) = c0

    val head = visitHeadPredicate(head0)
    val body = body0.map(b => visitBodyPredicate(b))

    val cparams = cparams0.map {
      case KindedAst.ConstraintParam(sym, l) =>
        TypedAst.ConstraintParam(sym, subst(sym.tvar), l)
    }

    TypedAst.Constraint(cparams, head, body, loc)
  }

  /**
    * Reconstructs types in the given predicate param.
    */
  private def visitPredicateParam(pparam: KindedAst.PredicateParam)(implicit root: KindedAst.Root, subst: Substitution): TypedAst.PredicateParam =
    TypedAst.PredicateParam(pparam.pred, subst(pparam.tpe), pparam.loc)

  /**
    * Reconstructs types in the given JVM method.
    */
  private def visitJvmMethod(method: KindedAst.JvmMethod)(implicit root: KindedAst.Root, subst: Substitution): TypedAst.JvmMethod = {
    method match {
      case KindedAst.JvmMethod(ident, fparams0, exp0, tpe, eff, loc) =>
        val fparams = fparams0.map(visitFormalParam(_, root, subst))
        val exp = visitExp(exp0)
        TypedAst.JvmMethod(ident, fparams, exp, tpe, eff, loc)
    }
  }

  /**
    * Reconstructs types in the given pattern.
    */
  private def visitPattern(pat0: KindedAst.Pattern)(implicit root: KindedAst.Root, subst: Substitution): TypedAst.Pattern = pat0 match {
    case KindedAst.Pattern.Wild(tvar, loc) => TypedAst.Pattern.Wild(subst(tvar), loc)
    case KindedAst.Pattern.Var(sym, tvar, loc) => TypedAst.Pattern.Var(sym, subst(tvar), loc)
    case KindedAst.Pattern.Cst(cst, loc) => TypedAst.Pattern.Cst(cst, constantType(cst), loc)

    case KindedAst.Pattern.Tag(sym, pat, tvar, loc) => TypedAst.Pattern.Tag(sym, visitPattern(pat), subst(tvar), loc)

    case KindedAst.Pattern.Tuple(elms, loc) =>
      val es = elms.map(visitPattern)
      val tpe = Type.mkTuple(es.map(_.tpe), loc)
      TypedAst.Pattern.Tuple(es, tpe, loc)

    case KindedAst.Pattern.Record(pats, pat, tvar, loc) =>
      val ps = pats.map {
        case KindedAst.Pattern.Record.RecordLabelPattern(field, tvar1, pat1, loc1) =>
          TypedAst.Pattern.Record.RecordLabelPattern(field, subst(tvar1), visitPattern(pat1), loc1)
      }
      val p = visitPattern(pat)
      TypedAst.Pattern.Record(ps, p, subst(tvar), loc)

    case KindedAst.Pattern.RecordEmpty(loc) =>
      TypedAst.Pattern.RecordEmpty(Type.mkRecord(Type.RecordRowEmpty, loc), loc)

    case KindedAst.Pattern.Error(tvar, loc) =>
      TypedAst.Pattern.Error(subst(tvar), loc)
  }


  /**
    * Reconstructs types in the given head predicate.
    */
  private def visitHeadPredicate(head0: KindedAst.Predicate.Head)(implicit root: KindedAst.Root, subst: Substitution): TypedAst.Predicate.Head = head0 match {
    case KindedAst.Predicate.Head.Atom(pred, den0, terms, tvar, loc) =>
      val ts = terms.map(t => visitExp(t))
      TypedAst.Predicate.Head.Atom(pred, den0, ts, subst(tvar), loc)
  }


  /**
    * Reconstructs types in the given body predicate.
    */
  private def visitBodyPredicate(body0: KindedAst.Predicate.Body)(implicit root: KindedAst.Root, subst: Substitution): TypedAst.Predicate.Body = body0 match {
    case KindedAst.Predicate.Body.Atom(pred, den0, polarity, fixity, terms, tvar, loc) =>
      val ts = terms.map(t => visitPattern(t))
      TypedAst.Predicate.Body.Atom(pred, den0, polarity, fixity, ts, subst(tvar), loc)

    case KindedAst.Predicate.Body.Functional(outVars, exp, loc) =>
      val e = visitExp(exp)
      TypedAst.Predicate.Body.Functional(outVars, e, loc)

    case KindedAst.Predicate.Body.Guard(exp, loc) =>
      val e = visitExp(exp)
      TypedAst.Predicate.Body.Guard(e, loc)

  }

  /**
    * Returns the type of the given constant.
    */
  private def constantType(cst: Ast.Constant): Type = cst match {
    case Constant.Unit => Type.Unit
    case Constant.Null => Type.Null
    case Constant.Bool(_) => Type.Bool
    case Constant.Char(_) => Type.Char
    case Constant.Float32(_) => Type.Float32
    case Constant.Float64(_) => Type.Float64
    case Constant.BigDecimal(_) => Type.BigDecimal
    case Constant.Int8(_) => Type.Int8
    case Constant.Int16(_) => Type.Int16
    case Constant.Int32(_) => Type.Int32
    case Constant.Int64(_) => Type.Int64
    case Constant.BigInt(_) => Type.BigInt
    case Constant.Str(_) => Type.Str
    case Constant.Regex(_) => Type.Regex
  }
}
