/*
 * Copyright 2024 Anna Krogh, Patrick Lundvig, Christian Bonde
 * 2024 Jakob Schneider Villumsen
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
import ca.uwaterloo.flix.language.ast.OccurrenceAst1.{DefContext, Occur}
import ca.uwaterloo.flix.language.ast.{MonoAst, OccurrenceAst1}
import ca.uwaterloo.flix.language.dbg.AstPrinter.*
import ca.uwaterloo.flix.util.ParOps

/**
  * Iterative runs of the optimizer pipeline: OccurrenceAnalyzer -> Inliner.
  */
object Optimizer1 {

  /**
    * Returns an optimized version of the given AST `root`.
    */
  def run(root: MonoAst.Root)(implicit flix: Flix): MonoAst.Root = flix.phase("Optimizer1") {
    if (flix.options.xnooptimizer) {
      root
    } else {
      var result = MonoAstConverter.run(root)
      for (_ <- 1 to 3) {
        val afterOccurrenceAnalyzer = OccurrenceAnalyzer1.run(result)
        val afterInliner = Inliner1.run(afterOccurrenceAnalyzer)
        result = afterInliner
      }
      OccurrenceAstConverter.run(result)
    }
  }

  private object MonoAstConverter {
    def run(root: MonoAst.Root)(implicit flix: Flix): OccurrenceAst1.Root = {
      val defs = ParOps.parMapValues(root.defs)(visitDef)
      val effects = ParOps.parMapValues(root.effects)(visitEffect)
      val enums = ParOps.parMapValues(root.enums)(visitEnum)
      val structs = ParOps.parMapValues(root.structs)(visitStruct)
      OccurrenceAst1.Root(defs, enums, structs, effects, root.entryPoint, root.reachable, root.sources)
    }

    private def visitDef(def0: MonoAst.Def): OccurrenceAst1.Def = def0 match {
      case MonoAst.Def(sym, spec, exp, loc) =>
        val e = visitExp(exp)
        val sp = visitSpec(spec)
        val fps = spec.fparams.map(fp => (visitFormalParam(fp), Occur.DontInline))
        val defContext = DefContext(isDirectCall = false, Occur.DontInline, 0, isSelfRecursive = false)
        OccurrenceAst1.Def(sym, fps, sp, e, defContext, loc)
    }

    private def visitSpec(spec0: MonoAst.Spec): OccurrenceAst1.Spec = spec0 match {
      case MonoAst.Spec(doc, ann, mod, _, functionType, retTpe, eff) =>
        OccurrenceAst1.Spec(doc, ann, mod, functionType, retTpe, eff)
    }

    private def visitEnum(enum0: MonoAst.Enum): OccurrenceAst1.Enum = enum0 match {
      case MonoAst.Enum(doc, ann, mod, sym, tparams, cases, loc) =>
        val tps = tparams.map(visitTypeParam)
        val cs = cases.map { case (sym, caze) => sym -> visitEnumCase(caze) }
        OccurrenceAst1.Enum(doc, ann, mod, sym, tps, cs, loc)
    }

    private def visitEnumCase(case0: MonoAst.Case): OccurrenceAst1.Case = case0 match {
      case MonoAst.Case(sym, tpe, loc) => OccurrenceAst1.Case(sym, tpe, loc)
    }

    private def visitStruct(struct0: MonoAst.Struct): OccurrenceAst1.Struct = struct0 match {
      case MonoAst.Struct(doc, ann, mod, sym, tparams, fields, loc) =>
        val tps = tparams.map(visitTypeParam)
        val fs = fields.map(visitStructField)
        OccurrenceAst1.Struct(doc, ann, mod, sym, tps, fs, loc)
    }

    private def visitStructField(field0: MonoAst.StructField): OccurrenceAst1.StructField = field0 match {
      case MonoAst.StructField(sym, tpe, loc) =>
        OccurrenceAst1.StructField(sym, tpe, loc)
    }

    private def visitEffect(effect0: MonoAst.Effect): OccurrenceAst1.Effect = effect0 match {
      case MonoAst.Effect(doc0, ann0, mod0, sym0, ops0, loc0) =>
        val ops = ops0.map(visitEffectOp)
        OccurrenceAst1.Effect(doc0, ann0, mod0, sym0, ops, loc0)
    }

    private def visitEffectOp(op0: MonoAst.Op): OccurrenceAst1.Op = op0 match {
      case MonoAst.Op(sym0, spec0, loc) =>
        val spec = visitSpec(spec0)
        val fps = spec0.fparams.map(visitFormalParam)
        OccurrenceAst1.Op(sym0, fps, spec, loc)
    }

    private def visitTypeParam(tparam0: MonoAst.TypeParam): OccurrenceAst1.TypeParam = tparam0 match {
      case MonoAst.TypeParam(name, sym, loc) => OccurrenceAst1.TypeParam(name, sym, loc)
    }

    /**
      * Translates the given formal parameter `fparam` from [[MonoAst.FormalParam]] into a [[OccurrenceAst1.FormalParam]].
      */
    private def visitFormalParam(fparam0: MonoAst.FormalParam): OccurrenceAst1.FormalParam = fparam0 match {
      case MonoAst.FormalParam(sym, mod, tpe, src, loc) => OccurrenceAst1.FormalParam(sym, mod, tpe, src, loc)
    }

    private def visitExp(exp0: MonoAst.Expr): OccurrenceAst1.Expr = exp0 match {

      case MonoAst.Expr.Cst(cst, tpe, loc) =>
        OccurrenceAst1.Expr.Cst(cst, tpe, loc)

      case MonoAst.Expr.Var(sym, tpe, loc) =>
        OccurrenceAst1.Expr.Var(sym, tpe, loc)

      case MonoAst.Expr.Lambda(fparam, exp, tpe, loc) =>
        val fp = visitFormalParam(fparam)
        val e = visitExp(exp)
        OccurrenceAst1.Expr.Lambda((fp, Occur.DontInline), e, tpe, loc)

      case MonoAst.Expr.ApplyAtomic(op, exps, tpe, eff, loc) =>
        val es = exps.map(visitExp)
        OccurrenceAst1.Expr.ApplyAtomic(op, es, tpe, eff, loc)

      case MonoAst.Expr.ApplyClo(exp, exps, tpe, eff, loc) =>
        val es = exps.map(visitExp)
        val e = visitExp(exp)
        OccurrenceAst1.Expr.ApplyClo(e, es, tpe, eff, loc)

      case MonoAst.Expr.ApplyDef(sym, exps, itpe, tpe, eff, loc) =>
        val es = exps.map(visitExp)
        OccurrenceAst1.Expr.ApplyDef(sym, es, itpe, tpe, eff, loc)

      case MonoAst.Expr.ApplyLocalDef(sym, exps, tpe, eff, loc) =>
        val es = exps.map(visitExp)
        OccurrenceAst1.Expr.ApplyLocalDef(sym, es, tpe, eff, loc)

      case MonoAst.Expr.Let(sym, exp1, exp2, tpe, eff, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        OccurrenceAst1.Expr.Let(sym, e1, e2, tpe, eff, Occur.DontInline, loc)

      case MonoAst.Expr.LocalDef(sym, fparams, exp1, exp2, tpe, eff, loc) =>
        val fps = fparams.map(visitFormalParam)
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        OccurrenceAst1.Expr.LocalDef(sym, fps, e1, e2, tpe, eff, Occur.DontInline, loc)

      case MonoAst.Expr.Scope(sym, rvar, exp, tpe, eff, loc) =>
        val e = visitExp(exp)
        OccurrenceAst1.Expr.Scope(sym, rvar, e, tpe, eff, loc)

      case MonoAst.Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        val e3 = visitExp(exp3)
        OccurrenceAst1.Expr.IfThenElse(e1, e2, e3, tpe, eff, loc)

      case MonoAst.Expr.Stm(exp1, exp2, tpe, eff, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        OccurrenceAst1.Expr.Stm(e1, e2, tpe, eff, loc)

      case MonoAst.Expr.Discard(exp, eff, loc) =>
        val e = visitExp(exp)
        OccurrenceAst1.Expr.Discard(e, eff, loc)

      case MonoAst.Expr.Match(exp, rules, tpe, eff, loc) =>
        val e = visitExp(exp)
        val rs = rules.map {
          case MonoAst.MatchRule(pat, guard, exp1) =>
            val p = visitPattern(pat)
            val g = guard.map(visitExp)
            val e = visitExp(exp1)
            OccurrenceAst1.MatchRule(p, g, e)
        }
        OccurrenceAst1.Expr.Match(e, rs, tpe, eff, loc)

      case MonoAst.Expr.VectorLit(exps, tpe, eff, loc) =>
        val es = exps.map(visitExp)
        OccurrenceAst1.Expr.VectorLit(es, tpe, eff, loc)

      case MonoAst.Expr.VectorLoad(exp1, exp2, tpe, eff, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        OccurrenceAst1.Expr.VectorLoad(e1, e2, tpe, eff, loc)

      case MonoAst.Expr.VectorLength(exp, loc) =>
        val e = visitExp(exp)
        OccurrenceAst1.Expr.VectorLength(e, loc)

      case MonoAst.Expr.Ascribe(exp, tpe, eff, loc) =>
        val e = visitExp(exp)
        OccurrenceAst1.Expr.Ascribe(e, tpe, eff, loc)

      case MonoAst.Expr.Cast(exp, declaredType, declaredEff, tpe, eff, loc) =>
        val e = visitExp(exp)
        OccurrenceAst1.Expr.Cast(e, declaredType, declaredEff, tpe, eff, loc)

      case MonoAst.Expr.TryCatch(exp, rules, tpe, eff, loc) =>
        val e = visitExp(exp)
        val rs = rules.map {
          case MonoAst.CatchRule(sym, clazz, exp) =>
            val e = visitExp(exp)
            OccurrenceAst1.CatchRule(sym, clazz, e)
        }
        OccurrenceAst1.Expr.TryCatch(e, rs, tpe, eff, loc)

      case MonoAst.Expr.TryWith(exp, effUse, rules, tpe, eff, loc) =>
        val e = visitExp(exp)
        val rs = rules.map {
          case MonoAst.HandlerRule(op, fparams, exp) =>
            val fps = fparams.map(visitFormalParam)
            val e = visitExp(exp)
            OccurrenceAst1.HandlerRule(op, fps, e)
        }
        OccurrenceAst1.Expr.TryWith(e, effUse, rs, tpe, eff, loc)

      case MonoAst.Expr.Do(op, exps, tpe, eff, loc) =>
        val es = exps.map(visitExp)
        OccurrenceAst1.Expr.Do(op, es, tpe, eff, loc)

      case MonoAst.Expr.NewObject(name, clazz, tpe, eff, methods0, loc) =>
        val methods = methods0.map {
          case MonoAst.JvmMethod(ident, fparams, exp, retTpe, eff, loc) =>
            val fps = fparams.map(visitFormalParam)
            val e = visitExp(exp)
            OccurrenceAst1.JvmMethod(ident, fps, e, retTpe, eff, loc)
        }
        OccurrenceAst1.Expr.NewObject(name, clazz, tpe, eff, methods, loc)

    }


    private def visitPattern(pattern00: MonoAst.Pattern): OccurrenceAst1.Pattern = {

      def visit(pattern0: MonoAst.Pattern): OccurrenceAst1.Pattern = pattern0 match {
        case MonoAst.Pattern.Wild(tpe, loc) =>
          OccurrenceAst1.Pattern.Wild(tpe, loc)

        case MonoAst.Pattern.Var(sym, tpe, loc) =>
          OccurrenceAst1.Pattern.Var(sym, tpe, Occur.DontInline, loc)

        case MonoAst.Pattern.Cst(cst, tpe, loc) =>
          OccurrenceAst1.Pattern.Cst(cst, tpe, loc)

        case MonoAst.Pattern.Tag(sym, pat, tpe, loc) =>
          val p = visit(pat)
          OccurrenceAst1.Pattern.Tag(sym, p, tpe, loc)

        case MonoAst.Pattern.Tuple(pats, tpe, loc) =>
          val ps = pats.map(visit)
          OccurrenceAst1.Pattern.Tuple(ps, tpe, loc)

        case MonoAst.Pattern.Record(pats, pat, tpe, loc) =>
          val ps = pats.map(visitRecordLabelPattern)
          val p = visit(pat)
          OccurrenceAst1.Pattern.Record(ps, p, tpe, loc)

        case MonoAst.Pattern.RecordEmpty(tpe, loc) =>
          OccurrenceAst1.Pattern.RecordEmpty(tpe, loc)
      }

      def visitRecordLabelPattern(pattern0: MonoAst.Pattern.Record.RecordLabelPattern): OccurrenceAst1.Pattern.Record.RecordLabelPattern = pattern0 match {
        case MonoAst.Pattern.Record.RecordLabelPattern(label, pat, tpe, loc) =>
          val p = visit(pat)
          OccurrenceAst1.Pattern.Record.RecordLabelPattern(label, p, tpe, loc)
      }

      visit(pattern00)
    }
  }

  private object OccurrenceAstConverter {
    def run(root: OccurrenceAst1.Root)(implicit flix: Flix): MonoAst.Root = {
      val defs = ParOps.parMapValues(root.defs)(visitDef)
      val effects = ParOps.parMapValues(root.effects)(visitEffect)
      val enums = ParOps.parMapValues(root.enums)(visitEnum)
      val structs = ParOps.parMapValues(root.structs)(visitStruct)
      MonoAst.Root(defs, enums, structs, effects, root.entryPoint, root.reachable, root.sources)
    }

    private def visitDef(def0: OccurrenceAst1.Def): MonoAst.Def = def0 match {
      case OccurrenceAst1.Def(sym, fparams, spec, exp, _, loc) =>
        val e = visitExp(exp)
        val sp = visitSpec(spec, fparams.map { case (fp, _) => fp })
        MonoAst.Def(sym, sp, e, loc)
    }

    private def visitSpec(spec0: OccurrenceAst1.Spec, fparams0: List[OccurrenceAst1.FormalParam]): MonoAst.Spec = spec0 match {
      case OccurrenceAst1.Spec(doc, ann, mod, functionType, retTpe, eff) =>
        val fps = fparams0.map(visitFormalParam)
        MonoAst.Spec(doc, ann, mod, fps, functionType, retTpe, eff)
    }

    private def visitEnum(enum0: OccurrenceAst1.Enum): MonoAst.Enum = enum0 match {
      case OccurrenceAst1.Enum(doc, ann, mod, sym, tparams, cases, loc) =>
        val tps = tparams.map(visitTypeParam)
        val cs = cases.map { case (sym, caze) => sym -> visitEnumCase(caze) }
        MonoAst.Enum(doc, ann, mod, sym, tps, cs, loc)
    }

    private def visitEnumCase(case0: OccurrenceAst1.Case): MonoAst.Case = case0 match {
      case OccurrenceAst1.Case(sym, tpe, loc) => MonoAst.Case(sym, tpe, loc)
    }

    private def visitStruct(struct0: OccurrenceAst1.Struct): MonoAst.Struct = struct0 match {
      case OccurrenceAst1.Struct(doc, ann, mod, sym, tparams, fields, loc) =>
        val tps = tparams.map(visitTypeParam)
        val fs = fields.map(visitStructField)
        MonoAst.Struct(doc, ann, mod, sym, tps, fs, loc)
    }

    private def visitStructField(field0: OccurrenceAst1.StructField): MonoAst.StructField = field0 match {
      case OccurrenceAst1.StructField(sym, tpe, loc) =>
        MonoAst.StructField(sym, tpe, loc)
    }

    private def visitEffect(effect0: OccurrenceAst1.Effect): MonoAst.Effect = effect0 match {
      case OccurrenceAst1.Effect(doc0, ann0, mod0, sym0, ops0, loc0) =>
        val ops = ops0.map(visitEffectOp)
        MonoAst.Effect(doc0, ann0, mod0, sym0, ops, loc0)
    }

    private def visitEffectOp(op0: OccurrenceAst1.Op): MonoAst.Op = op0 match {
      case OccurrenceAst1.Op(sym0, fparams0, spec0, loc) =>
        val spec = visitSpec(spec0, fparams0)
        MonoAst.Op(sym0, spec, loc)
    }

    private def visitTypeParam(tparam0: OccurrenceAst1.TypeParam): MonoAst.TypeParam = tparam0 match {
      case OccurrenceAst1.TypeParam(name, sym, loc) => MonoAst.TypeParam(name, sym, loc)
    }

    /**
      * Translates the given formal parameter `fparam` from [[OccurrenceAst1.FormalParam]] into a [[MonoAst.FormalParam]].
      */
    private def visitFormalParam(fparam0: OccurrenceAst1.FormalParam): MonoAst.FormalParam = fparam0 match {
      case OccurrenceAst1.FormalParam(sym, mod, tpe, src, loc) => MonoAst.FormalParam(sym, mod, tpe, src, loc)
    }

    private def visitExp(exp0: OccurrenceAst1.Expr): MonoAst.Expr = exp0 match {

      case OccurrenceAst1.Expr.Cst(cst, tpe, loc) =>
        MonoAst.Expr.Cst(cst, tpe, loc)

      case OccurrenceAst1.Expr.Var(sym, tpe, loc) =>
        MonoAst.Expr.Var(sym, tpe, loc)

      case OccurrenceAst1.Expr.Lambda((fparam, _), exp, tpe, loc) =>
        val fp = visitFormalParam(fparam)
        val e = visitExp(exp)
        MonoAst.Expr.Lambda(fp, e, tpe, loc)

      case OccurrenceAst1.Expr.ApplyAtomic(op, exps, tpe, eff, loc) =>
        val es = exps.map(visitExp)
        MonoAst.Expr.ApplyAtomic(op, es, tpe, eff, loc)

      case OccurrenceAst1.Expr.ApplyClo(exp, exps, tpe, eff, loc) =>
        val es = exps.map(visitExp)
        val e = visitExp(exp)
        MonoAst.Expr.ApplyClo(e, es, tpe, eff, loc)

      case OccurrenceAst1.Expr.ApplyDef(sym, exps, itpe, tpe, eff, loc) =>
        val es = exps.map(visitExp)
        MonoAst.Expr.ApplyDef(sym, es, itpe, tpe, eff, loc)

      case OccurrenceAst1.Expr.ApplyLocalDef(sym, exps, tpe, eff, loc) =>
        val es = exps.map(visitExp)
        MonoAst.Expr.ApplyLocalDef(sym, es, tpe, eff, loc)

      case OccurrenceAst1.Expr.Let(sym, exp1, exp2, tpe, eff, _, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        MonoAst.Expr.Let(sym, e1, e2, tpe, eff, loc)

      case OccurrenceAst1.Expr.LocalDef(sym, fparams, exp1, exp2, tpe, eff, _, loc) =>
        val fps = fparams.map(visitFormalParam)
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        MonoAst.Expr.LocalDef(sym, fps, e1, e2, tpe, eff, loc)

      case OccurrenceAst1.Expr.Scope(sym, rvar, exp, tpe, eff, loc) =>
        val e = visitExp(exp)
        MonoAst.Expr.Scope(sym, rvar, e, tpe, eff, loc)

      case OccurrenceAst1.Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        val e3 = visitExp(exp3)
        MonoAst.Expr.IfThenElse(e1, e2, e3, tpe, eff, loc)

      case OccurrenceAst1.Expr.Stm(exp1, exp2, tpe, eff, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        MonoAst.Expr.Stm(e1, e2, tpe, eff, loc)

      case OccurrenceAst1.Expr.Discard(exp, eff, loc) =>
        val e = visitExp(exp)
        MonoAst.Expr.Discard(e, eff, loc)

      case OccurrenceAst1.Expr.Match(exp, rules, tpe, eff, loc) =>
        val e = visitExp(exp)
        val rs = rules.map {
          case OccurrenceAst1.MatchRule(pat, guard, exp1) =>
            val p = visitPattern(pat)
            val g = guard.map(visitExp)
            val e = visitExp(exp1)
            MonoAst.MatchRule(p, g, e)
        }
        MonoAst.Expr.Match(e, rs, tpe, eff, loc)

      case OccurrenceAst1.Expr.VectorLit(exps, tpe, eff, loc) =>
        val es = exps.map(visitExp)
        MonoAst.Expr.VectorLit(es, tpe, eff, loc)

      case OccurrenceAst1.Expr.VectorLoad(exp1, exp2, tpe, eff, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        MonoAst.Expr.VectorLoad(e1, e2, tpe, eff, loc)

      case OccurrenceAst1.Expr.VectorLength(exp, loc) =>
        val e = visitExp(exp)
        MonoAst.Expr.VectorLength(e, loc)

      case OccurrenceAst1.Expr.Ascribe(exp, tpe, eff, loc) =>
        val e = visitExp(exp)
        MonoAst.Expr.Ascribe(e, tpe, eff, loc)

      case OccurrenceAst1.Expr.Cast(exp, declaredType, declaredEff, tpe, eff, loc) =>
        val e = visitExp(exp)
        MonoAst.Expr.Cast(e, declaredType, declaredEff, tpe, eff, loc)

      case OccurrenceAst1.Expr.TryCatch(exp, rules, tpe, eff, loc) =>
        val e = visitExp(exp)
        val rs = rules.map {
          case OccurrenceAst1.CatchRule(sym, clazz, exp) =>
            val e = visitExp(exp)
            MonoAst.CatchRule(sym, clazz, e)
        }
        MonoAst.Expr.TryCatch(e, rs, tpe, eff, loc)

      case OccurrenceAst1.Expr.TryWith(exp, effUse, rules, tpe, eff, loc) =>
        val e = visitExp(exp)
        val rs = rules.map {
          case OccurrenceAst1.HandlerRule(op, fparams, exp) =>
            val fps = fparams.map(visitFormalParam)
            val e = visitExp(exp)
            MonoAst.HandlerRule(op, fps, e)
        }
        MonoAst.Expr.TryWith(e, effUse, rs, tpe, eff, loc)

      case OccurrenceAst1.Expr.Do(op, exps, tpe, eff, loc) =>
        val es = exps.map(visitExp)
        MonoAst.Expr.Do(op, es, tpe, eff, loc)

      case OccurrenceAst1.Expr.NewObject(name, clazz, tpe, eff, methods0, loc) =>
        val methods = methods0.map {
          case OccurrenceAst1.JvmMethod(ident, fparams, exp, retTpe, eff, loc) =>
            val fps = fparams.map(visitFormalParam)
            val e = visitExp(exp)
            MonoAst.JvmMethod(ident, fps, e, retTpe, eff, loc)
        }
        MonoAst.Expr.NewObject(name, clazz, tpe, eff, methods, loc)

    }


    private def visitPattern(pattern00: OccurrenceAst1.Pattern): MonoAst.Pattern = {

      def visit(pattern0: OccurrenceAst1.Pattern): MonoAst.Pattern = pattern0 match {
        case OccurrenceAst1.Pattern.Wild(tpe, loc) =>
          MonoAst.Pattern.Wild(tpe, loc)

        case OccurrenceAst1.Pattern.Var(sym, tpe, occur, loc) =>
          MonoAst.Pattern.Var(sym, tpe, loc)

        case OccurrenceAst1.Pattern.Cst(cst, tpe, loc) =>
          MonoAst.Pattern.Cst(cst, tpe, loc)

        case OccurrenceAst1.Pattern.Tag(sym, pat, tpe, loc) =>
          val p = visit(pat)
          MonoAst.Pattern.Tag(sym, p, tpe, loc)

        case OccurrenceAst1.Pattern.Tuple(pats, tpe, loc) =>
          val ps = pats.map(visit)
          MonoAst.Pattern.Tuple(ps, tpe, loc)

        case OccurrenceAst1.Pattern.Record(pats, pat, tpe, loc) =>
          val ps = pats.map(visitRecordLabelPattern)
          val p = visit(pat)
          MonoAst.Pattern.Record(ps, p, tpe, loc)

        case OccurrenceAst1.Pattern.RecordEmpty(tpe, loc) =>
          MonoAst.Pattern.RecordEmpty(tpe, loc)
      }

      def visitRecordLabelPattern(pattern0: OccurrenceAst1.Pattern.Record.RecordLabelPattern): MonoAst.Pattern.Record.RecordLabelPattern = pattern0 match {
        case OccurrenceAst1.Pattern.Record.RecordLabelPattern(label, pat, tpe, loc) =>
          val p = visit(pat)
          MonoAst.Pattern.Record.RecordLabelPattern(label, p, tpe, loc)
      }

      visit(pattern00)
    }

  }
}
