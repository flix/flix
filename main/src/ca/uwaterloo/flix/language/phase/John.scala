package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.ast.LoweredNormalAst.{Expr, Pattern}
import ca.uwaterloo.flix.language.ast.NormalType.toType
import ca.uwaterloo.flix.language.ast.{LoweredAst, LoweredNormalAst, Scheme, Type}
import ca.uwaterloo.flix.util.collection.MapOps

object John {

  def convert(root: LoweredNormalAst.Root): LoweredAst.Root = root match {
    case LoweredNormalAst.Root(defs0, enums, effects0, entryPoint, reachable, sources, eqEnv) =>
      val defs = MapOps.mapValues(defs0)(visitDef)
      val effects = MapOps.mapValues(effects0)(visitEffect)
      LoweredAst.Root(Map.empty, Map.empty, Map.empty, defs, enums, effects, Map.empty, entryPoint, reachable, sources, Map.empty, eqEnv)
  }

  private def visitDef(defn: LoweredNormalAst.Def): LoweredAst.Def = defn match {
    case LoweredNormalAst.Def(sym, spec0, exp0) =>
      val exp = visitExp(exp0)
      val spec = visitSpec(spec0)
      LoweredAst.Def(sym, spec, exp)
  }

  private def visitSpec(spec: LoweredNormalAst.Spec): LoweredAst.Spec = spec match {
    case LoweredNormalAst.Spec(doc, ann, mod, fparams0, scheme, retTpe0, eff0, loc) =>
      val fparams = fparams0.map(visitFormalParam)
      val eff = toType(eff0)
      val retTpe = toType(retTpe0)
      LoweredAst.Spec(doc, ann, mod, Nil, fparams, scheme, retTpe, eff, Nil, loc)
  }

  private def visitEffect(eff: LoweredNormalAst.Effect): LoweredAst.Effect = eff match {
    case LoweredNormalAst.Effect(doc, ann, mod, sym, ops0, loc) =>
      val ops = ops0.map(visitOp)
      LoweredAst.Effect(doc, ann, mod, sym, ops, loc)
  }

  private def visitOp(op: LoweredNormalAst.Op): LoweredAst.Op = op match {
    case LoweredNormalAst.Op(sym, spec0) =>
      val spec = visitSpec(spec0)
      LoweredAst.Op(sym, spec)
  }

  def visitExp(exp: LoweredNormalAst.Expr): LoweredAst.Expr = exp match {
    case Expr.Cst(cst, tpe, loc) => LoweredAst.Expr.Cst(cst, toType(tpe), loc)
    case Expr.Var(sym, tpe, loc) => LoweredAst.Expr.Var(sym, toType(tpe), loc)
    case Expr.Def(sym, tpe, loc) => LoweredAst.Expr.Def(sym, toType(tpe), loc)
    case Expr.Sig(sym, tpe, loc) => LoweredAst.Expr.Sig(sym, toType(tpe), loc)
    case Expr.Lambda(fparam, exp, tpe, loc) => LoweredAst.Expr.Lambda(visitFormalParam(fparam), visitExp(exp), toType(tpe), loc)
    case Expr.Apply(exp, exps, tpe, eff, loc) => LoweredAst.Expr.Apply(visitExp(exp), exps.map(visitExp), toType(tpe), toType(eff), loc)
    case Expr.ApplyAtomic(op, exps, tpe, eff, loc) => LoweredAst.Expr.ApplyAtomic(op, exps.map(visitExp), toType(tpe), toType(eff), loc)
    case Expr.Let(sym, mod, exp1, exp2, tpe, eff, loc) => LoweredAst.Expr.Let(sym, mod, visitExp(exp1), visitExp(exp2), toType(tpe), toType(eff), loc)
    case Expr.LetRec(sym, mod, exp1, exp2, tpe, eff, loc) => LoweredAst.Expr.LetRec(sym, mod, visitExp(exp1), visitExp(exp2), toType(tpe), toType(eff), loc)
    case Expr.Scope(sym, regionVar, exp, tpe, eff, loc) => LoweredAst.Expr.Scope(sym, Type.Var(regionVar, loc), visitExp(exp), toType(tpe), toType(eff), loc)
    case Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) => LoweredAst.Expr.IfThenElse(visitExp(exp1), visitExp(exp2), visitExp(exp3), toType(tpe), toType(eff), loc)
    case Expr.Stm(exp1, exp2, tpe, eff, loc) => LoweredAst.Expr.Stm(visitExp(exp1), visitExp(exp2), toType(tpe), toType(eff), loc)
    case Expr.Discard(exp, eff, loc) => LoweredAst.Expr.Discard(visitExp(exp), toType(eff), loc)
    case Expr.Match(exp, rules, tpe, eff, loc) => LoweredAst.Expr.Match(visitExp(exp), rules.map(visitMatchRule), toType(tpe), toType(eff), loc)
    case Expr.VectorLit(exps, tpe, eff, loc) => LoweredAst.Expr.VectorLit(exps.map(visitExp), toType(tpe), toType(eff), loc)
    case Expr.VectorLoad(exp1, exp2, tpe, eff, loc) => LoweredAst.Expr.VectorLoad(visitExp(exp1), visitExp(exp2), toType(tpe), toType(eff), loc)
    case Expr.VectorLength(exp, loc) => LoweredAst.Expr.VectorLength(visitExp(exp), loc)
    case Expr.Ascribe(exp, tpe, eff, loc) => LoweredAst.Expr.Ascribe(visitExp(exp), toType(tpe), toType(eff), loc)
    case Expr.Cast(exp, tpe, eff, loc) => LoweredAst.Expr.Cast(visitExp(exp), None, None, toType(tpe), toType(eff), loc)
    case Expr.TryCatch(exp, rules, tpe, eff, loc) => LoweredAst.Expr.TryCatch(visitExp(exp), rules.map(visitCatchRule), toType(tpe), toType(eff), loc)
    case Expr.TryWith(exp, effUse, rules, tpe, eff, loc) => LoweredAst.Expr.TryWith(visitExp(exp), effUse, rules.map(visitHandlerRule), toType(tpe), toType(eff), loc)
    case Expr.Do(op, exps, tpe, eff, loc) => LoweredAst.Expr.Do(op, exps.map(visitExp), toType(tpe), toType(eff), loc)
    case Expr.NewObject(name, clazz, tpe, eff, methods, loc) => LoweredAst.Expr.NewObject(name, clazz, toType(tpe), toType(eff), methods.map(visitJvmMethod), loc)
  }

  def visitFormalParam(fparam: LoweredNormalAst.FormalParam): LoweredAst.FormalParam = fparam match {
    case LoweredNormalAst.FormalParam(sym, mod, tpe, src, loc) =>
      LoweredAst.FormalParam(sym, mod, toType(tpe), src, loc)
  }

  def visitMatchRule(rule: LoweredNormalAst.MatchRule): LoweredAst.MatchRule = rule match {
    case LoweredNormalAst.MatchRule(pat, guard, exp) =>
      LoweredAst.MatchRule(visitPattern(pat), guard.map(visitExp), visitExp(exp))
  }

  def visitPattern(pattern: LoweredNormalAst.Pattern): LoweredAst.Pattern = pattern match {
    case Pattern.Wild(tpe, loc) => LoweredAst.Pattern.Wild(toType(tpe), loc)
    case Pattern.Var(sym, tpe, loc) => LoweredAst.Pattern.Var(sym, toType(tpe), loc)
    case Pattern.Cst(cst, tpe, loc) => LoweredAst.Pattern.Cst(cst, toType(tpe), loc)
    case Pattern.Tag(sym, pat, tpe, loc) => LoweredAst.Pattern.Tag(sym, visitPattern(pat), toType(tpe), loc)
    case Pattern.Tuple(elms, tpe, loc) => LoweredAst.Pattern.Tuple(elms.map(visitPattern), toType(tpe), loc)
    case Pattern.Record(pats, pat, tpe, loc) => LoweredAst.Pattern.Record(pats.map(visitRecordLabelPattern), visitPattern(pat), toType(tpe), loc)
    case Pattern.RecordEmpty(tpe, loc) => LoweredAst.Pattern.RecordEmpty(toType(tpe), loc)
  }

  def visitRecordLabelPattern(pat: LoweredNormalAst.Pattern.Record.RecordLabelPattern): LoweredAst.Pattern.Record.RecordLabelPattern = pat match {
    case LoweredNormalAst.Pattern.Record.RecordLabelPattern(label, tpe, pat, loc) => LoweredAst.Pattern.Record.RecordLabelPattern(label, toType(tpe), visitPattern(pat), loc)

  }

  def visitCatchRule(rule: LoweredNormalAst.CatchRule): LoweredAst.CatchRule = rule match {
    case LoweredNormalAst.CatchRule(sym, clazz, exp) =>
      LoweredAst.CatchRule(sym, clazz, visitExp(exp))
  }

  def visitHandlerRule(rule: LoweredNormalAst.HandlerRule): LoweredAst.HandlerRule = rule match {
    case LoweredNormalAst.HandlerRule(op, fparams, exp) =>
      LoweredAst.HandlerRule(op, fparams.map(visitFormalParam), visitExp(exp))
  }

  def visitJvmMethod(method: LoweredNormalAst.JvmMethod): LoweredAst.JvmMethod = method match {
    case LoweredNormalAst.JvmMethod(ident, fparams, exp, retTpe, eff, loc) =>
      LoweredAst.JvmMethod(ident, fparams.map(visitFormalParam), visitExp(exp), toType(retTpe), toType(eff), loc)
  }

}
