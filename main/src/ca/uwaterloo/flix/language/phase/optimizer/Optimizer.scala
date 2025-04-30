package ca.uwaterloo.flix.language.phase.optimizer

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{MonoAst, OccurrenceAst}
import ca.uwaterloo.flix.language.dbg.AstPrinter.DebugMonoAst
import ca.uwaterloo.flix.util.ParOps

object Optimizer {

  /**
    * Returns an optimized version of the given AST `root`.
    */
  def run(root: MonoAst.Root)(implicit flix: Flix): MonoAst.Root = flix.phase("Optimizer") {
    var result = ToOccurrenceAst.run(root)
    var delta = result.defs.keys.toSet
    for (_ <- 1 to 10) {
      val (occurrenceRoot, occurrenceChange) = OccurrenceAnalyzer.run(result, delta)
      val (inlinerRoot, inlinerChange) = Inliner.run(occurrenceRoot, occurrenceChange)
      result = inlinerRoot
      delta = inlinerChange
    }
    ToMonoAst.run(result)
  }

  private object ToOccurrenceAst {
    def run(root: MonoAst.Root)(implicit flix: Flix): OccurrenceAst.Root = {
      val defs = ParOps.parMapValues(root.defs)(visitDef)
      OccurrenceAst.Root(defs, root.enums, root.structs, root.effects, root.mainEntryPoint, root.entryPoints, root.sources)
    }

    private def visitDef(def0: MonoAst.Def): OccurrenceAst.Def = def0 match {
      case MonoAst.Def(sym, spec, exp, loc) =>
        val e = visitExp(exp)
        val fps = spec.fparams.map(visitFormalParam)
        val ctx = OccurrenceAst.DefContext(0, isDirectCall = false, isSelfRecursive = false)
        OccurrenceAst.Def(sym, fps, spec, e, ctx, loc)
    }

    private def visitExp(expr0: MonoAst.Expr): OccurrenceAst.Expr = expr0 match {
      case MonoAst.Expr.Cst(cst, tpe, loc) =>
        OccurrenceAst.Expr.Cst(cst, tpe, loc)

      case MonoAst.Expr.Var(sym, tpe, loc) =>
        OccurrenceAst.Expr.Var(sym, tpe, loc)

      case MonoAst.Expr.Lambda(fparam, exp, tpe, loc) =>
        val fp = visitFormalParam(fparam)
        val e = visitExp(exp)
        OccurrenceAst.Expr.Lambda(fp, e, tpe, loc)

      case MonoAst.Expr.ApplyAtomic(op, exps, tpe, eff, loc) =>
        val es = exps.map(visitExp)
        OccurrenceAst.Expr.ApplyAtomic(op, es, tpe, eff, loc)

      case MonoAst.Expr.ApplyClo(exp1, exp2, tpe, eff, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        OccurrenceAst.Expr.ApplyClo(e1, e2, tpe, eff, loc)

      case MonoAst.Expr.ApplyDef(sym, exps, itpe, tpe, eff, loc) =>
        val es = exps.map(visitExp)
        OccurrenceAst.Expr.ApplyDef(sym, es, itpe, tpe, eff, loc)

      case MonoAst.Expr.ApplyLocalDef(sym, exps, tpe, eff, loc) =>
        val es = exps.map(visitExp)
        OccurrenceAst.Expr.ApplyLocalDef(sym, es, tpe, eff, loc)

      case MonoAst.Expr.Let(sym, exp1, exp2, tpe, eff, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        val occur = OccurrenceAst.Occur.Dead
        OccurrenceAst.Expr.Let(sym, e1, e2, tpe, eff, occur, loc)

      case MonoAst.Expr.LocalDef(sym, fparams, exp1, exp2, tpe, eff, loc) =>
        val fps = fparams.map(visitFormalParam)
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        val occur = OccurrenceAst.Occur.Dead
        OccurrenceAst.Expr.LocalDef(sym, fps, e1, e2, tpe, eff, occur, loc)

      case MonoAst.Expr.Scope(sym, regSym, exp, tpe, eff, loc) =>
        val e = visitExp(exp)
        OccurrenceAst.Expr.Scope(sym, regSym, e, tpe, eff, loc)

      case MonoAst.Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        val e3 = visitExp(exp3)
        OccurrenceAst.Expr.IfThenElse(e1, e2, e3, tpe, eff, loc)

      case MonoAst.Expr.Stm(exp1, exp2, tpe, eff, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        OccurrenceAst.Expr.Stm(e1, e2, tpe, eff, loc)

      case MonoAst.Expr.Discard(exp, eff, loc) =>
        val e = visitExp(exp)
        OccurrenceAst.Expr.Discard(e, eff, loc)

      case MonoAst.Expr.Match(exp, rules, tpe, eff, loc) =>
        val e = visitExp(exp)
        val rs = rules.map {
          case MonoAst.MatchRule(pat, guard, exp1) =>
            val p = visitPattern(pat)
            val g = guard.map(visitExp)
            val e1 = visitExp(exp1)
            OccurrenceAst.MatchRule(p, g, e1)
        }
        OccurrenceAst.Expr.Match(e, rs, tpe, eff, loc)

      case MonoAst.Expr.VectorLit(exps, tpe, eff, loc) =>
        val es = exps.map(visitExp)
        OccurrenceAst.Expr.VectorLit(es, tpe, eff, loc)

      case MonoAst.Expr.VectorLoad(exp1, exp2, tpe, eff, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        OccurrenceAst.Expr.VectorLoad(e1, e2, tpe, eff, loc)

      case MonoAst.Expr.VectorLength(exp, loc) =>
        val e = visitExp(exp)
        OccurrenceAst.Expr.VectorLength(e, loc)

      case MonoAst.Expr.Ascribe(exp, tpe, eff, loc) =>
        val e = visitExp(exp)
        OccurrenceAst.Expr.Ascribe(e, tpe, eff, loc)

      case MonoAst.Expr.Cast(exp, declaredType, declaredEff, tpe, eff, loc) =>
        val e = visitExp(exp)
        OccurrenceAst.Expr.Cast(e, declaredType, declaredEff, tpe, eff, loc)

      case MonoAst.Expr.TryCatch(exp, rules, tpe, eff, loc) =>
        val e = visitExp(exp)
        val rs = rules.map {
          case MonoAst.CatchRule(sym, clazz, exp1) =>
            val e1 = visitExp(exp1)
            OccurrenceAst.CatchRule(sym, clazz, e1)
        }
        OccurrenceAst.Expr.TryCatch(e, rs, tpe, eff, loc)

      case MonoAst.Expr.RunWith(exp, effUse, rules, tpe, eff, loc) =>
        val e = visitExp(exp)
        val rs = rules.map {
          case MonoAst.HandlerRule(op, fparams, exp1) =>
            val e1 = visitExp(exp1)
            val fps = fparams.map(visitFormalParam)
            OccurrenceAst.HandlerRule(op, fps, e1)
        }
        OccurrenceAst.Expr.RunWith(e, effUse, rs, tpe, eff, loc)

      case MonoAst.Expr.Do(op, exps, tpe, eff, loc) =>
        val es = exps.map(visitExp)
        OccurrenceAst.Expr.Do(op, es, tpe, eff, loc)

      case MonoAst.Expr.NewObject(name, clazz, tpe, eff, methods, loc) =>
        val ms = methods.map {
          case MonoAst.JvmMethod(ident, fparams, exp1, retTpe, eff1, loc1) =>
            val fps = fparams.map(visitFormalParam)
            val e1 = visitExp(exp1)
            OccurrenceAst.JvmMethod(ident, fps, e1, retTpe, eff1, loc1)
        }
        OccurrenceAst.Expr.NewObject(name, clazz, tpe, eff, ms, loc)
    }

    private def visitPattern(pat0: MonoAst.Pattern): OccurrenceAst.Pattern = pat0 match {
      case MonoAst.Pattern.Wild(tpe, loc) =>
        OccurrenceAst.Pattern.Wild(tpe, loc)

      case MonoAst.Pattern.Var(sym, tpe, loc) =>
        OccurrenceAst.Pattern.Var(sym, tpe, OccurrenceAst.Occur.Dead, loc)

      case MonoAst.Pattern.Cst(cst, tpe, loc) =>
        OccurrenceAst.Pattern.Cst(cst, tpe, loc)

      case MonoAst.Pattern.Tag(sym, pats, tpe, loc) =>
        val ps = pats.map(visitPattern)
        OccurrenceAst.Pattern.Tag(sym, ps, tpe, loc)

      case MonoAst.Pattern.Tuple(pats, tpe, loc) =>
        val ps = pats.map(visitPattern)
        OccurrenceAst.Pattern.Tuple(ps, tpe, loc)

      case MonoAst.Pattern.Record(pats, pat, tpe, loc) =>
        val ps = pats.map {
          case MonoAst.Pattern.Record.RecordLabelPattern(label, pat1, tpe1, loc1) =>
            val p1 = visitPattern(pat1)
            OccurrenceAst.Pattern.Record.RecordLabelPattern(label, p1, tpe1, loc1)
        }
        val p = visitPattern(pat)
        OccurrenceAst.Pattern.Record(ps, p, tpe, loc)
    }

    private def visitFormalParam(fp0: MonoAst.FormalParam): OccurrenceAst.FormalParam = fp0 match {
      case MonoAst.FormalParam(sym, mod, tpe, src, loc) => OccurrenceAst.FormalParam(sym, mod, tpe, src, OccurrenceAst.Occur.Dead, loc)
    }
  }

  private object ToMonoAst {
    def run(root: OccurrenceAst.Root)(implicit flix: Flix): MonoAst.Root = {
      val defs = ParOps.parMapValues(root.defs)(visitDef)
      MonoAst.Root(defs, root.enums, root.structs, root.effects, root.mainEntryPoint, root.entryPoints, root.sources)
    }

    private def visitDef(def0: OccurrenceAst.Def): MonoAst.Def = def0 match {
      case OccurrenceAst.Def(sym, _, spec, exp, _, loc) =>
        val e = visitExp(exp)
        MonoAst.Def(sym, spec, e, loc)
    }

    private def visitExp(expr0: OccurrenceAst.Expr): MonoAst.Expr = expr0 match {
      case OccurrenceAst.Expr.Cst(cst, tpe, loc) =>
        MonoAst.Expr.Cst(cst, tpe, loc)

      case OccurrenceAst.Expr.Var(sym, tpe, loc) =>
        MonoAst.Expr.Var(sym, tpe, loc)

      case OccurrenceAst.Expr.Lambda(fparam, exp, tpe, loc) =>
        val fp = visitFormalParam(fparam)
        val e = visitExp(exp)
        MonoAst.Expr.Lambda(fp, e, tpe, loc)

      case OccurrenceAst.Expr.ApplyAtomic(op, exps, tpe, eff, loc) =>
        val es = exps.map(visitExp)
        MonoAst.Expr.ApplyAtomic(op, es, tpe, eff, loc)

      case OccurrenceAst.Expr.ApplyClo(exp1, exp2, tpe, eff, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        MonoAst.Expr.ApplyClo(e1, e2, tpe, eff, loc)

      case OccurrenceAst.Expr.ApplyDef(sym, exps, itpe, tpe, eff, loc) =>
        val es = exps.map(visitExp)
        MonoAst.Expr.ApplyDef(sym, es, itpe, tpe, eff, loc)

      case OccurrenceAst.Expr.ApplyLocalDef(sym, exps, tpe, eff, loc) =>
        val es = exps.map(visitExp)
        MonoAst.Expr.ApplyLocalDef(sym, es, tpe, eff, loc)

      case OccurrenceAst.Expr.Let(sym, exp1, exp2, tpe, eff, _, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        MonoAst.Expr.Let(sym, e1, e2, tpe, eff, loc)

      case OccurrenceAst.Expr.LocalDef(sym, fparams, exp1, exp2, tpe, eff, _, loc) =>
        val fps = fparams.map(visitFormalParam)
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        MonoAst.Expr.LocalDef(sym, fps, e1, e2, tpe, eff, loc)

      case OccurrenceAst.Expr.Scope(sym, regSym, exp, tpe, eff, loc) =>
        val e = visitExp(exp)
        MonoAst.Expr.Scope(sym, regSym, e, tpe, eff, loc)

      case OccurrenceAst.Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        val e3 = visitExp(exp3)
        MonoAst.Expr.IfThenElse(e1, e2, e3, tpe, eff, loc)

      case OccurrenceAst.Expr.Stm(exp1, exp2, tpe, eff, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        MonoAst.Expr.Stm(e1, e2, tpe, eff, loc)

      case OccurrenceAst.Expr.Discard(exp, eff, loc) =>
        val e = visitExp(exp)
        MonoAst.Expr.Discard(e, eff, loc)

      case OccurrenceAst.Expr.Match(exp, rules, tpe, eff, loc) =>
        val e = visitExp(exp)
        val rs = rules.map {
          case OccurrenceAst.MatchRule(pat, guard, exp1) =>
            val p = visitPattern(pat)
            val g = guard.map(visitExp)
            val e1 = visitExp(exp1)
            MonoAst.MatchRule(p, g, e1)
        }
        MonoAst.Expr.Match(e, rs, tpe, eff, loc)

      case OccurrenceAst.Expr.VectorLit(exps, tpe, eff, loc) =>
        val es = exps.map(visitExp)
        MonoAst.Expr.VectorLit(es, tpe, eff, loc)

      case OccurrenceAst.Expr.VectorLoad(exp1, exp2, tpe, eff, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        MonoAst.Expr.VectorLoad(e1, e2, tpe, eff, loc)

      case OccurrenceAst.Expr.VectorLength(exp, loc) =>
        val e = visitExp(exp)
        MonoAst.Expr.VectorLength(e, loc)

      case OccurrenceAst.Expr.Ascribe(exp, tpe, eff, loc) =>
        val e = visitExp(exp)
        MonoAst.Expr.Ascribe(e, tpe, eff, loc)

      case OccurrenceAst.Expr.Cast(exp, declaredType, declaredEff, tpe, eff, loc) =>
        val e = visitExp(exp)
        MonoAst.Expr.Cast(e, declaredType, declaredEff, tpe, eff, loc)

      case OccurrenceAst.Expr.TryCatch(exp, rules, tpe, eff, loc) =>
        val e = visitExp(exp)
        val rs = rules.map {
          case OccurrenceAst.CatchRule(sym, clazz, exp1) =>
            val e1 = visitExp(exp1)
            MonoAst.CatchRule(sym, clazz, e1)
        }
        MonoAst.Expr.TryCatch(e, rs, tpe, eff, loc)

      case OccurrenceAst.Expr.RunWith(exp, effUse, rules, tpe, eff, loc) =>
        val e = visitExp(exp)
        val rs = rules.map {
          case OccurrenceAst.HandlerRule(op, fparams, exp1) =>
            val e1 = visitExp(exp1)
            val fps = fparams.map(visitFormalParam)
            MonoAst.HandlerRule(op, fps, e1)
        }
        MonoAst.Expr.RunWith(e, effUse, rs, tpe, eff, loc)

      case OccurrenceAst.Expr.Do(op, exps, tpe, eff, loc) =>
        val es = exps.map(visitExp)
        MonoAst.Expr.Do(op, es, tpe, eff, loc)

      case OccurrenceAst.Expr.NewObject(name, clazz, tpe, eff, methods, loc) =>
        val ms = methods.map {
          case OccurrenceAst.JvmMethod(ident, fparams, exp1, retTpe, eff1, loc1) =>
            val fps = fparams.map(visitFormalParam)
            val e1 = visitExp(exp1)
            MonoAst.JvmMethod(ident, fps, e1, retTpe, eff1, loc1)
        }
        MonoAst.Expr.NewObject(name, clazz, tpe, eff, ms, loc)
    }

    private def visitPattern(pat0: OccurrenceAst.Pattern): MonoAst.Pattern = pat0 match {
      case OccurrenceAst.Pattern.Wild(tpe, loc) =>
        MonoAst.Pattern.Wild(tpe, loc)

      case OccurrenceAst.Pattern.Var(sym, tpe, _, loc) =>
        MonoAst.Pattern.Var(sym, tpe, loc)

      case OccurrenceAst.Pattern.Cst(cst, tpe, loc) =>
        MonoAst.Pattern.Cst(cst, tpe, loc)

      case OccurrenceAst.Pattern.Tag(sym, pats, tpe, loc) =>
        val ps = pats.map(visitPattern)
        MonoAst.Pattern.Tag(sym, ps, tpe, loc)

      case OccurrenceAst.Pattern.Tuple(pats, tpe, loc) =>
        val ps = pats.map(visitPattern)
        MonoAst.Pattern.Tuple(ps, tpe, loc)

      case OccurrenceAst.Pattern.Record(pats, pat, tpe, loc) =>
        val ps = pats.map {
          case OccurrenceAst.Pattern.Record.RecordLabelPattern(label, pat1, tpe1, loc1) =>
            val p1 = visitPattern(pat1)
            MonoAst.Pattern.Record.RecordLabelPattern(label, p1, tpe1, loc1)
        }
        val p = visitPattern(pat)
        MonoAst.Pattern.Record(ps, p, tpe, loc)
    }

    private def visitFormalParam(fp0: OccurrenceAst.FormalParam): MonoAst.FormalParam = fp0 match {
      case OccurrenceAst.FormalParam(sym, mod, tpe, src, _, loc) => MonoAst.FormalParam(sym, mod, tpe, src, loc)
    }
  }
}
