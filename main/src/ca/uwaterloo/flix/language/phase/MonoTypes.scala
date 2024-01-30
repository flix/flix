/*
 * Copyright 2023 Jonathan Lindegaard Starup
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
import ca.uwaterloo.flix.language.ast.LoweredAst.{Expr, Pattern}
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.util.collection.MapOps
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps}

/**
  * This phase does two things:
  * - Erase enums, such that `Option[t]` becomes `Option`
  * - Removes all type aliases in types
  */
object MonoTypes {

  /**
    * Performs monomorphization of enums on the given AST `root` and removes alias types.
    */
  def run(root: LoweredAst.Root)(implicit flix: Flix): LoweredAst.Root = flix.phase("MonoTypes") {
    // Assumptions:
    // - All typeclass information have been transformed into defs - this
    //   phase only looks at types and expressions in defs.
    // - All the following types have been removed:
    //   - Type variables
    //   - Associated types
    // - In schemas these are unused
    //   - tconstrs
    //   - econstrs

    val defs = ParOps.parMapValues(root.defs)(visitDef)
    root.copy(defs = defs, enums = Map.empty)
  }

  /**
    * Returns a [[LoweredAst.Def]] with specialized enums and without aliases in its types.
    */
  private def visitDef(defn: LoweredAst.Def): LoweredAst.Def = defn match {
    case LoweredAst.Def(sym, spec, exp) =>
      val s = visitSpec(spec)
      val e = visitExp(exp)
      LoweredAst.Def(sym, s, e)
  }

  /**
    * Returns a [[LoweredAst.Spec]] with specialized enums and without aliases in its types.
    */
  private def visitSpec(spec: LoweredAst.Spec): LoweredAst.Spec = spec match {
    case LoweredAst.Spec(doc, ann, mod, tparams, fparams, declaredScheme, retTpe, eff, tconstrs, loc) =>
      val fs = fparams.map(visitFormalParam)
      val ds = visitScheme(declaredScheme)
      val rt = visitType(retTpe)
      val p = visitType(eff)
      LoweredAst.Spec(doc, ann, mod, tparams, fs, ds, rt, p, tconstrs, loc)
  }

  /**
    * Returns an expression with specialized enums and without aliases in its types
    */
  private def visitExp(exp: LoweredAst.Expr): LoweredAst.Expr = exp match {
    case Expr.Cst(cst, tpe, loc) =>
      val t = visitType(tpe)
      Expr.Cst(cst, t, loc)

    case Expr.Var(sym, tpe, loc) =>
      val t = visitType(tpe)
      Expr.Var(sym, t, loc)

    case Expr.Def(sym, tpe, loc) =>
      val t = visitType(tpe)
      Expr.Def(sym, t, loc)

    case Expr.Sig(sym, tpe, loc) =>
      val t = visitType(tpe)
      Expr.Sig(sym, t, loc)

    case Expr.Lambda(fparam, exp, tpe, loc) =>
      val fs = visitFormalParam(fparam)
      val e = visitExp(exp)
      val t = visitType(tpe)
      Expr.Lambda(fs, e, t, loc)

    case Expr.Apply(exp, exps, tpe, eff, loc) =>
      val e = visitExp(exp)
      val es = exps.map(visitExp)
      val t = visitType(tpe)
      val p = visitType(eff)
      Expr.Apply(e, es, t, p, loc)

    case Expr.ApplyAtomic(op, exps, tpe, eff, loc) =>
      val es = exps.map(visitExp)
      val t = visitType(tpe)
      val p = visitType(eff)
      Expr.ApplyAtomic(op, es, t, p, loc)

    case Expr.Let(sym, mod, exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      val p = visitType(eff)
      Expr.Let(sym, mod, e1, e2, t, p, loc)

    case Expr.LetRec(sym, mod, exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      val p = visitType(eff)
      Expr.LetRec(sym, mod, e1, e2, t, p, loc)

    case Expr.Scope(sym, regionVar, exp, tpe, eff, loc) =>
      // The region variable has been rendered redundant by Monomorph.
      // It has replaced the region with pure/impure and the variable could
      // conceptually be removed.
      val e = visitExp(exp)
      val t = visitType(tpe)
      val p = visitType(eff)
      Expr.Scope(sym, regionVar, e, t, p, loc)

    case Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val e3 = visitExp(exp3)
      val t = visitType(tpe)
      val p = visitType(eff)
      Expr.IfThenElse(e1, e2, e3, t, p, loc)

    case Expr.Stm(exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      val p = visitType(eff)
      Expr.Stm(e1, e2, t, p, loc)

    case Expr.Discard(exp, eff, loc) =>
      val e = visitExp(exp)
      val p = visitType(eff)
      Expr.Discard(e, p, loc)

    case Expr.Match(exp, rules, tpe, eff, loc) =>
      val e = visitExp(exp)
      val rs = rules.map {
        case LoweredAst.MatchRule(pat, guard, exp) =>
          val p = visitPat(pat)
          val g = guard.map(visitExp)
          val e = visitExp(exp)
          LoweredAst.MatchRule(p, g, e)
      }
      val t = visitType(tpe)
      val p = visitType(eff)
      Expr.Match(e, rs, t, p, loc)

    case Expr.TypeMatch(exp, rules, tpe, eff, loc) =>
      val e = visitExp(exp)
      val rs = rules.map {
        case LoweredAst.TypeMatchRule(sym, tpe, exp) =>
          val t = visitType(tpe)
          val re = visitExp(exp)
          LoweredAst.TypeMatchRule(sym, t, re)
      }
      val t = visitType(tpe)
      val p = visitType(eff)
      Expr.TypeMatch(e, rs, t, p, loc)

    case Expr.VectorLit(exps, tpe, eff, loc) =>
      val es = exps.map(visitExp)
      val t = visitType(tpe)
      val p = visitType(eff)
      Expr.VectorLit(es, t, p, loc)

    case Expr.VectorLoad(exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      val p = visitType(eff)
      Expr.VectorLoad(e1, e2, t, p, loc)

    case Expr.VectorLength(exp, loc) =>
      val e = visitExp(exp)
      Expr.VectorLength(e, loc)

    case Expr.Ascribe(exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      val p = visitType(eff)
      Expr.Ascribe(e, t, p, loc)

    case Expr.Cast(exp, declaredType, declaredEff, tpe, eff, loc) =>
      val e = visitExp(exp)
      val dt = declaredType.map(visitType)
      val dp = declaredEff.map(visitType)
      val t = visitType(tpe)
      val p = visitType(eff)
      Expr.Cast(e, dt, dp, t, p, loc)

    case Expr.TryCatch(exp, rules, tpe, eff, loc) =>
      val e = visitExp(exp)
      val rs = rules.map {
        case LoweredAst.CatchRule(sym, clazz, exp) =>
          val re = visitExp(exp)
          LoweredAst.CatchRule(sym, clazz, re)
      }
      val t = visitType(tpe)
      val p = visitType(eff)
      Expr.TryCatch(e, rs, t, p, loc)

    case Expr.TryWith(exp, effUse, rules, tpe, eff, loc) =>
      val e = visitExp(exp)
      val rs = rules.map {
        case LoweredAst.HandlerRule(op, fparams, exp) =>
          val fs = fparams.map(visitFormalParam)
          val he = visitExp(exp)
          LoweredAst.HandlerRule(op, fs, he)
      }
      val t = visitType(tpe)
      val p = visitType(eff)
      Expr.TryWith(e, effUse, rs, t, p, loc)

    case Expr.Do(op, exps, tpe, eff, loc) =>
      val es = exps.map(visitExp)
      val t = visitType(tpe)
      val p = visitType(eff)
      Expr.Do(op, es, t, p, loc)

    case Expr.NewObject(name, clazz, tpe, eff, methods, loc) =>
      val t = visitType(tpe)
      val p = visitType(eff)
      val ms = methods.map {
        case LoweredAst.JvmMethod(ident, fparams, exp, retTpe, eff, loc) =>
          val fs = fparams.map(visitFormalParam)
          val me = visitExp(exp)
          val mt = visitType(retTpe)
          val mp = visitType(eff)
          LoweredAst.JvmMethod(ident, fs, me, mt, mp, loc)
      }
      Expr.NewObject(name, clazz, t, p, ms, loc)
  }

  /**
    * Returns a pattern with specialized enums in its type and no aliases.
    */
  private def visitPat(pat: LoweredAst.Pattern): LoweredAst.Pattern = pat match {
    case Pattern.Wild(tpe, loc) =>
      val t = visitType(tpe)
      Pattern.Wild(t, loc)

    case Pattern.Var(sym, tpe, loc) =>
      val t = visitType(tpe)
      Pattern.Var(sym, t, loc)

    case Pattern.Cst(cst, tpe, loc) =>
      val t = visitType(tpe)
      Pattern.Cst(cst, t, loc)

    case Pattern.Tag(sym, tagPat, tpe, loc) =>
      val tp = visitPat(tagPat)
      val t = visitType(tpe)
      Pattern.Tag(sym, tp, t, loc)

    case Pattern.Tuple(elms, tpe, loc) =>
      val es = elms.map(visitPat)
      val t = visitType(tpe)
      Pattern.Tuple(es, t, loc)

    case Pattern.Record(pats, pat, tpe, loc) =>
      val ps = pats.map {
        case Pattern.Record.RecordLabelPattern(label, tpe1, pat1, loc1) =>
          Pattern.Record.RecordLabelPattern(label, visitType(tpe1), visitPat(pat1), loc1)
      }
      val p = visitPat(pat)
      val t = visitType(tpe)
      Pattern.Record(ps, p, t, loc)

    case Pattern.RecordEmpty(tpe, loc) =>
      Pattern.RecordEmpty(visitType(tpe), loc)
  }

  /**
    * Returns a type where 1) aliases are removed and 2) enums are non-parametric.
    */
  private def visitType(tpe: Type): Type = tpe match {
    case Type.Cst(TypeConstructor.Enum(sym, _), loc) =>
      // Remove type arguments from enums.
      Type.Cst(TypeConstructor.Enum(sym, Kind.Star), loc)
    case Type.Cst(tc, loc) =>
      Type.Cst(tc, loc)
    case Type.Apply(tpe1, tpe2, loc) =>
      Type.Apply(visitType(tpe1), visitType(tpe2), loc)
    case Type.Alias(_, _, tpe, _) =>
      // Remove Alias types.
      visitType(tpe)
    case Type.Var(sym, loc) =>
      // Assumed to have been removed earlier.
      throw InternalCompilerException(s"Unexpected type var: '$sym'", loc)
    case Type.AssocType(cst, _, _, loc) =>
      // Assumed to have been removed earlier.
      throw InternalCompilerException(s"Unexpected associated type: '${cst.sym}'", loc)
  }

  /**
    * Returns a formal param with specialized enums in its type and no aliases.
    */
  private def visitFormalParam(p: LoweredAst.FormalParam): LoweredAst.FormalParam = p match {
    case LoweredAst.FormalParam(sym, mod, tpe, src, loc) =>
      val t = visitType(tpe)
      LoweredAst.FormalParam(sym, mod, t, src, loc)
  }

  /**
    * Returns a scheme with specialized enums in its base and no aliases.
    */
  private def visitScheme(sc: Scheme): Scheme = sc match {
    case Scheme(quantifiers, tconstrs, econstrs, base) =>
      // Since the types are expected to be without variables,
      // all fields except base should be "unused"/empty
      val b = visitType(base)
      Scheme(quantifiers, tconstrs, econstrs, b)
  }

}
