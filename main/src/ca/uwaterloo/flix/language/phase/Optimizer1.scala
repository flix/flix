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
import ca.uwaterloo.flix.language.ast.OccurrenceAst1.Occur.Dead
import ca.uwaterloo.flix.language.ast.shared.Constant
import ca.uwaterloo.flix.language.ast.{AtomicOp, MonoAst, OccurrenceAst1, Type}
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
    var result = root
    for (_ <- 1 to 3) {
      val afterOccurrenceAnalyzer = OccurrenceAnalyzer1.run(result)
      val afterInliner = Inliner1.run(afterOccurrenceAnalyzer)
      result = afterInliner
    }
    DeadCodeElim.run(result)
  }

  private object DeadCodeElim {
    def run(root: MonoAst.Root)(implicit flix: Flix): MonoAst.Root = {
      val occurrenceRoot = OccurrenceAnalyzer1.run(root)
      shake(occurrenceRoot)
    }

    private def shake(root: OccurrenceAst1.Root)(implicit flix: Flix): MonoAst.Root = {
      val defs = ParOps.parMapValues(root.defs)(visitDef)
      val effects = ParOps.parMapValues(root.effects)(visitEffect)
      val enums = ParOps.parMapValues(root.enums)(visitEnum)
      val structs = ParOps.parMapValues(root.structs)(visitStruct)
      MonoAst.Root(defs, enums, structs, effects, root.entryPoint, root.reachable, root.sources)
    }

    private def visitDef(def0: OccurrenceAst1.Def)(implicit flix: Flix): MonoAst.Def = def0 match {
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

    private def visitFormalParam(fparam0: OccurrenceAst1.FormalParam): MonoAst.FormalParam = fparam0 match {
      case OccurrenceAst1.FormalParam(sym, mod, tpe, src, loc) => MonoAst.FormalParam(sym, mod, tpe, src, loc)
    }

    private def visitExp(exp0: OccurrenceAst1.Expr)(implicit flix: Flix): MonoAst.Expr = exp0 match {

      case OccurrenceAst1.Expr.Cst(cst, tpe, loc) =>
        MonoAst.Expr.Cst(cst, tpe, loc)

      case OccurrenceAst1.Expr.Var(sym, tpe, loc) =>
        MonoAst.Expr.Var(sym, tpe, loc)

      case OccurrenceAst1.Expr.Lambda((fparam, _), exp, tpe, loc) =>
        val fps = visitFormalParam(fparam)
        val e = visitExp(exp)
        MonoAst.Expr.Lambda(fps, e, tpe, loc)

      case OccurrenceAst1.Expr.ApplyAtomic(op, exps, tpe, eff, loc) =>
        val es = exps.map(visitExp)
        op match {
          case AtomicOp.Untag(_) =>
            val List(e) = es
            // Inline expressions of the form Untag(Tag(e)) => e
            e match {
              case MonoAst.Expr.ApplyAtomic(AtomicOp.Tag(_), innerExps, _, _, _) => innerExps.head
              case _ => MonoAst.Expr.ApplyAtomic(op, es, tpe, eff, loc)
            }

          case _ => MonoAst.Expr.ApplyAtomic(op, es, tpe, eff, loc)
        }

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

      case OccurrenceAst1.Expr.Let(sym, exp1, exp2, tpe, eff, occur, loc) =>
        if (isDead(occur)) {
          if (isPure(exp1.eff)) {
            visitExp(exp2)
          } else {
            val e1 = visitExp(exp1)
            val e2 = visitExp(exp2)
            MonoAst.Expr.Stm(e1, e2, tpe, eff, loc)
          }
        } else {
          val e1 = visitExp(exp1)
          val e2 = visitExp(exp2)
          MonoAst.Expr.Let(sym, e1, e2, tpe, eff, loc)
        }

      case OccurrenceAst1.Expr.LocalDef(sym, fparams, exp1, exp2, tpe, eff, occur, loc) =>
        if (isDead(occur)) { // Probably never happens
          visitExp(exp2)
        } else {
          val fps = fparams.map(visitFormalParam)
          val e1 = visitExp(exp1)
          val e2 = visitExp(exp2)
          MonoAst.Expr.LocalDef(sym, fps, e1, e2, tpe, eff, loc)
        }

      case OccurrenceAst1.Expr.Scope(sym, rvar, exp, tpe, eff, loc) =>
        val e = visitExp(exp)
        MonoAst.Expr.Scope(sym, rvar, e, tpe, eff, loc)

      case OccurrenceAst1.Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        val e3 = visitExp(exp3)
        e1 match {
          case MonoAst.Expr.Cst(Constant.Bool(true), _, _) => e2
          case MonoAst.Expr.Cst(Constant.Bool(false), _, _) => e3
          case _ => MonoAst.Expr.IfThenElse(e1, e2, e3, tpe, eff, loc)
        }

      case OccurrenceAst1.Expr.Stm(exp1, exp2, tpe, eff, loc) =>
        if (isPure(exp1.eff)) {
          visitExp(exp2)
        } else {
          val e1 = visitExp(exp1)
          val e2 = visitExp(exp2)
          MonoAst.Expr.Stm(e1, e2, tpe, eff, loc)
        }

      case OccurrenceAst1.Expr.Discard(exp, eff, loc) =>
        val e = visitExp(exp)
        MonoAst.Expr.Discard(e, eff, loc)

      case OccurrenceAst1.Expr.Match(exp, rules, tpe, eff, loc) =>
        val e = visitExp(exp)
        val rs = rules.map {
          case OccurrenceAst1.MatchRule(pat, guard, exp1) =>
            val p = visitPattern(pat)
            val g = guard.map(visitExp)
            val e1 = visitExp(exp1)
            MonoAst.MatchRule(p, g, e1)
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
          case OccurrenceAst1.CatchRule(sym, clazz, exp1) =>
            val e1 = visitExp(exp1)
            MonoAst.CatchRule(sym, clazz, e1)
        }
        MonoAst.Expr.TryCatch(e, rs, tpe, eff, loc)

      case OccurrenceAst1.Expr.TryWith(exp, effUse, rules, tpe, eff, loc) =>
        val e = visitExp(exp)
        val rs = rules.map {
          case OccurrenceAst1.HandlerRule(op, fparams, exp1) =>
            val fps = fparams.map(visitFormalParam)
            val e1 = visitExp(exp1)
            MonoAst.HandlerRule(op, fps, e1)
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
          if (isDead(occur)) {
            MonoAst.Pattern.Wild(tpe, loc)
          } else {
            MonoAst.Pattern.Var(sym, tpe, loc)
          }

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

    private def isPure(eff0: Type): Boolean = {
      eff0.effects.isEmpty
    }

    private def isDead(occur: OccurrenceAst1.Occur): Boolean = occur match {
      case Dead => true
      case _ => false
    }
  }
}
