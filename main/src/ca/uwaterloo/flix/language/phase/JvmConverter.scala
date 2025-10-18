/*
 * Copyright 2025 Jonathan Lindegaard Starup
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
import ca.uwaterloo.flix.language.ast.ReducedAst.Expr
import ca.uwaterloo.flix.language.ast.{JvmAst, ReducedAst}
import ca.uwaterloo.flix.language.dbg.AstPrinter.DebugNoOp
import ca.uwaterloo.flix.util.collection.MapOps

object JvmConverter {

  def run(root: ReducedAst.Root)(implicit flix: Flix): JvmAst.Root = flix.phase("HowLowCanYouGo") {
    JvmAst.Root(
      MapOps.mapValues(root.defs)(visitDef),
      MapOps.mapValues(root.enums)(visitEnum),
      MapOps.mapValues(root.structs)(visitStruct),
      MapOps.mapValues(root.effects)(visitEffect),
      root.types,
      root.anonClasses.map(visitAnonClass),
      root.mainEntryPoint,
      root.entryPoints,
      root.sources
    )
  }(DebugNoOp())

  private def visitDef(defn: ReducedAst.Def): JvmAst.Def = {
    JvmAst.Def(
      defn.ann,
      defn.mod,
      defn.sym,
      defn.cparams.map(visitFormalParam),
      defn.fparams.map(visitFormalParam),
      defn.lparams.map(visitLocalParam),
      defn.pcPoints,
      visitExpr(defn.expr),
      defn.tpe,
      visitUnboxedType(defn.unboxedType),
      defn.loc
    )
  }

  private def visitEnum(enm: ReducedAst.Enum): JvmAst.Enum = {
    JvmAst.Enum(
      enm.ann,
      enm.mod,
      enm.sym,
      enm.tparams.map(visitTypeParam),
      MapOps.mapValues(enm.cases)(visitCase),
      enm.loc
    )
  }

  private def visitStruct(struct: ReducedAst.Struct): JvmAst.Struct = {
    JvmAst.Struct(
      struct.ann,
      struct.mod,
      struct.sym,
      struct.tparams.map(visitTypeParam),
      struct.fields.map(visitStructField),
      struct.loc
    )
  }

  private def visitEffect(effect: ReducedAst.Effect): JvmAst.Effect = {
    JvmAst.Effect(
      effect.ann,
      effect.mod,
      effect.sym,
      effect.ops.map(visitOp),
      effect.loc
    )
  }

  private def visitAnonClass(anon: ReducedAst.AnonClass): JvmAst.AnonClass = {
    JvmAst.AnonClass(
      anon.name,
      anon.clazz,
      anon.tpe,
      anon.methods.map(visitJvmMethod),
      anon.loc
    )
  }

  private def visitUnboxedType(tpe: ReducedAst.UnboxedType): JvmAst.UnboxedType =
    JvmAst.UnboxedType(
      tpe.tpe
    )

  private def visitFormalParam(fp: ReducedAst.FormalParam): JvmAst.FormalParam =
    JvmAst.FormalParam(
      fp.sym,
      fp.tpe
    )

  private def visitLocalParam(lp: ReducedAst.LocalParam): JvmAst.LocalParam =
    JvmAst.LocalParam(
      lp.sym,
      lp.tpe
    )

  private def visitTypeParam(tp: ReducedAst.TypeParam): JvmAst.TypeParam =
    JvmAst.TypeParam(
      tp.name,
      tp.sym
    )

  private def visitStructField(field: ReducedAst.StructField): JvmAst.StructField =
    JvmAst.StructField(
      field.sym,
      field.tpe,
      field.loc
    )

  private def visitCase(caze: ReducedAst.Case): JvmAst.Case =
    JvmAst.Case(
      caze.sym,
      caze.tpes,
      caze.loc
    )

  private def visitOp(op: ReducedAst.Op): JvmAst.Op =
    JvmAst.Op(
      op.sym,
      op.ann,
      op.mod,
      op.fparams.map(visitFormalParam),
      op.tpe,
      op.purity,
      op.loc
    )

  private def visitJvmMethod(method: ReducedAst.JvmMethod): JvmAst.JvmMethod =
    JvmAst.JvmMethod(
      method.ident,
      method.fparams.map(visitFormalParam),
      visitExpr(method.exp),
      method.tpe,
      method.purity,
      method.loc
    )

  private def visitExpr(expr: ReducedAst.Expr): JvmAst.Expr = expr match {
    case Expr.Cst(cst, loc) =>
      JvmAst.Expr.Cst(cst, loc)
    case Expr.Var(sym, tpe, loc) =>
      JvmAst.Expr.Var(sym, tpe, loc)
    case Expr.ApplyAtomic(op, exps, tpe, purity, loc) =>
      val es = exps.map(visitExpr)
      JvmAst.Expr.ApplyAtomic(op, es, tpe, purity, loc)
    case Expr.ApplyClo(exp1, exp2, ct, tpe, purity, loc) =>
      val e1 = visitExpr(exp1)
      val e2 = visitExpr(exp2)
      JvmAst.Expr.ApplyClo(e1, e2, ct, tpe, purity, loc)
    case Expr.ApplyDef(sym, exps, ct, tpe, purity, loc) =>
      val es = exps.map(visitExpr)
      JvmAst.Expr.ApplyDef(sym, es, ct, tpe, purity, loc)
    case Expr.ApplyOp(sym, exps, tpe, purity, loc) =>
      val es = exps.map(visitExpr)
      JvmAst.Expr.ApplyOp(sym, es, tpe, purity, loc)
    case Expr.ApplySelfTail(sym, actuals, tpe, purity, loc) =>
      val as = actuals.map(visitExpr)
      JvmAst.Expr.ApplySelfTail(sym, as, tpe, purity, loc)
    case Expr.IfThenElse(exp1, exp2, exp3, tpe, purity, loc) =>
      val e1 = visitExpr(exp1)
      val e2 = visitExpr(exp2)
      val e3 = visitExpr(exp3)
      JvmAst.Expr.IfThenElse(e1, e2, e3, tpe, purity, loc)
    case Expr.Branch(exp, branches, tpe, purity, loc) =>
      val e = visitExpr(exp)
      val bs = MapOps.mapValues(branches)(visitExpr)
      JvmAst.Expr.Branch(e, bs, tpe, purity, loc)
    case Expr.JumpTo(sym, tpe, purity, loc) =>
      JvmAst.Expr.JumpTo(sym, tpe, purity, loc)
    case Expr.Let(sym, exp1, exp2, loc) =>
      val e1 = visitExpr(exp1)
      val e2 = visitExpr(exp2)
      JvmAst.Expr.Let(sym, e1, e2, loc)
    case Expr.Stmt(exp1, exp2, loc) =>
      val e1 = visitExpr(exp1)
      val e2 = visitExpr(exp2)
      JvmAst.Expr.Stmt(e1, e2, loc)
    case Expr.Region(sym, exp, tpe, purity, loc) =>
      val e = visitExpr(exp)
      JvmAst.Expr.Region(sym, e, tpe, purity, loc)
    case Expr.TryCatch(exp, rules, tpe, purity, loc) =>
      val e = visitExpr(exp)
      val rs = rules.map(visitCatchRule)
      JvmAst.Expr.TryCatch(e, rs, tpe, purity, loc)
    case Expr.RunWith(exp, effUse, rules, ct, tpe, purity, loc) =>
      val e = visitExpr(exp)
      val rs = rules.map(visitHandlerRule)
      JvmAst.Expr.RunWith(e, effUse, rs, ct, tpe, purity, loc)
    case Expr.NewObject(name, clazz, tpe, purity, methods, loc) =>
      val ms = methods.map(visitJvmMethod)
      JvmAst.Expr.NewObject(name, clazz, tpe, purity, ms, loc)
  }

  private def visitCatchRule(rule: ReducedAst.CatchRule): JvmAst.CatchRule =
    JvmAst.CatchRule(rule.sym, rule.clazz, visitExpr(rule.exp))

  private def visitHandlerRule(rule: ReducedAst.HandlerRule): JvmAst.HandlerRule =
    JvmAst.HandlerRule(
      rule.op,
      rule.fparams.map(visitFormalParam),
      visitExpr(rule.exp)
    )
}
