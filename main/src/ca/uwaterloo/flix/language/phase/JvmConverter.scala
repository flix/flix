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
import ca.uwaterloo.flix.language.ast.{JvmAst, ReducedAst}
import ca.uwaterloo.flix.language.dbg.AstPrinter.DebugNoOp
import ca.uwaterloo.flix.util.ParOps
import ca.uwaterloo.flix.util.collection.MapOps

object JvmConverter {

  def run(root: ReducedAst.Root)(implicit flix: Flix): JvmAst.Root = flix.phase("HowLowCanYouGo") {
    val defs = ParOps.parMapValues(root.defs)(visitDef)
    val enums = ParOps.parMapValues(root.enums)(visitEnum)
    val structs = ParOps.parMapValues(root.structs)(visitStruct)
    val effects = ParOps.parMapValues(root.effects)(visitEffect)
    val anonClasses = root.anonClasses.map(visitAnonClass)
    JvmAst.Root(
      defs,
      enums,
      structs,
      effects,
      root.types,
      anonClasses,
      root.mainEntryPoint,
      root.entryPoints,
      root.sources
    )
  }(DebugNoOp())

  private def visitDef(defn: ReducedAst.Def): JvmAst.Def = {
    val cparams = defn.cparams.map(visitFormalParam)
    val fparams = defn.fparams.map(visitFormalParam)
    val lparams = defn.lparams.map(visitLocalParam)
    val exp = visitExp(defn.expr)
    val unboxedType = visitUnboxedType(defn.unboxedType)
    JvmAst.Def(
      defn.ann,
      defn.mod,
      defn.sym,
      cparams,
      fparams,
      lparams,
      defn.pcPoints,
      exp,
      defn.tpe,
      unboxedType,
      defn.loc
    )
  }

  private def visitEnum(enm: ReducedAst.Enum): JvmAst.Enum = {
    val tparams = enm.tparams.map(visitTypeParam)
    val cases = MapOps.mapValues(enm.cases)(visitCase)
    JvmAst.Enum(
      enm.ann,
      enm.mod,
      enm.sym,
      tparams,
      cases,
      enm.loc
    )
  }

  private def visitStruct(struct: ReducedAst.Struct): JvmAst.Struct = {
    val tparams = struct.tparams.map(visitTypeParam)
    val fields = struct.fields.map(visitStructField)
    JvmAst.Struct(
      struct.ann,
      struct.mod,
      struct.sym,
      tparams,
      fields,
      struct.loc
    )
  }

  private def visitEffect(effect: ReducedAst.Effect): JvmAst.Effect = {
    val ops = effect.ops.map(visitOp)
    JvmAst.Effect(
      effect.ann,
      effect.mod,
      effect.sym,
      ops,
      effect.loc
    )
  }

  private def visitAnonClass(anon: ReducedAst.AnonClass): JvmAst.AnonClass = {
    val methods = anon.methods.map(visitJvmMethod)
    JvmAst.AnonClass(
      anon.name,
      anon.clazz,
      anon.tpe,
      methods,
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

  private def visitOp(op: ReducedAst.Op): JvmAst.Op = {
    val fparams = op.fparams.map(visitFormalParam)
    JvmAst.Op(
      op.sym,
      op.ann,
      op.mod,
      fparams,
      op.tpe,
      op.purity,
      op.loc
    )
  }

  private def visitJvmMethod(method: ReducedAst.JvmMethod): JvmAst.JvmMethod = {
    val fparams = method.fparams.map(visitFormalParam)
    val exp = visitExp(method.exp)
    JvmAst.JvmMethod(
      method.ident,
      fparams,
      exp,
      method.tpe,
      method.purity,
      method.loc
    )
  }

  private def visitExp(exp0: ReducedAst.Expr): JvmAst.Exp = exp0 match {
    case ReducedAst.Expr.Cst(cst, loc) =>
      JvmAst.Exp.Cst(cst, loc)
    case ReducedAst.Expr.Var(sym, tpe, loc) =>
      JvmAst.Exp.Var(sym, tpe, loc)
    case ReducedAst.Expr.ApplyAtomic(op, exps, tpe, purity, loc) =>
      val es = exps.map(visitExp)
      JvmAst.Exp.ApplyAtomic(op, es, tpe, purity, loc)
    case ReducedAst.Expr.ApplyClo(exp1, exp2, ct, tpe, purity, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      JvmAst.Exp.ApplyClo(e1, e2, ct, tpe, purity, loc)
    case ReducedAst.Expr.ApplyDef(sym, exps, ct, tpe, purity, loc) =>
      val es = exps.map(visitExp)
      JvmAst.Exp.ApplyDef(sym, es, ct, tpe, purity, loc)
    case ReducedAst.Expr.ApplyOp(sym, exps, tpe, purity, loc) =>
      val es = exps.map(visitExp)
      JvmAst.Exp.ApplyOp(sym, es, tpe, purity, loc)
    case ReducedAst.Expr.ApplySelfTail(sym, actuals, tpe, purity, loc) =>
      val as = actuals.map(visitExp)
      JvmAst.Exp.ApplySelfTail(sym, as, tpe, purity, loc)
    case ReducedAst.Expr.IfThenElse(exp1, exp2, exp3, tpe, purity, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val e3 = visitExp(exp3)
      JvmAst.Exp.IfThenElse(e1, e2, e3, tpe, purity, loc)
    case ReducedAst.Expr.Branch(exp, branches, tpe, purity, loc) =>
      val e = visitExp(exp)
      val bs = MapOps.mapValues(branches)(visitExp)
      JvmAst.Exp.Branch(e, bs, tpe, purity, loc)
    case ReducedAst.Expr.JumpTo(sym, tpe, purity, loc) =>
      JvmAst.Exp.JumpTo(sym, tpe, purity, loc)
    case ReducedAst.Expr.Let(sym, exp1, exp2, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      JvmAst.Exp.Let(sym, e1, e2, loc)
    case ReducedAst.Expr.Stmt(exp1, exp2, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      JvmAst.Exp.Stmt(e1, e2, loc)
    case ReducedAst.Expr.Region(sym, exp, tpe, purity, loc) =>
      val e = visitExp(exp)
      JvmAst.Exp.Region(sym, e, tpe, purity, loc)
    case ReducedAst.Expr.TryCatch(exp, rules, tpe, purity, loc) =>
      val e = visitExp(exp)
      val rs = rules.map(visitCatchRule)
      JvmAst.Exp.TryCatch(e, rs, tpe, purity, loc)
    case ReducedAst.Expr.RunWith(exp, effUse, rules, ct, tpe, purity, loc) =>
      val e = visitExp(exp)
      val rs = rules.map(visitHandlerRule)
      JvmAst.Exp.RunWith(e, effUse, rs, ct, tpe, purity, loc)
    case ReducedAst.Expr.NewObject(name, clazz, tpe, purity, methods, loc) =>
      val ms = methods.map(visitJvmMethod)
      JvmAst.Exp.NewObject(name, clazz, tpe, purity, ms, loc)
  }

  private def visitCatchRule(rule: ReducedAst.CatchRule): JvmAst.CatchRule =
    JvmAst.CatchRule(rule.sym, rule.clazz, visitExp(rule.exp))

  private def visitHandlerRule(rule: ReducedAst.HandlerRule): JvmAst.HandlerRule = {
    val fparams = rule.fparams.map(visitFormalParam)
    val exp = visitExp(rule.exp)
    JvmAst.HandlerRule(
      rule.op,
      fparams,
      exp
    )
  }
}
