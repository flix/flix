/*
 * Copyright 2023 Magnus Madsen
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
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.util.ParOps

import java.util.concurrent.ConcurrentLinkedQueue
import scala.jdk.CollectionConverters._

object Reducer {

  def run(root: LiftedAst.Root)(implicit flix: Flix): ReducedAst.Root = flix.phase("Reducer") {
    implicit val ctx: SharedContext = SharedContext(new ConcurrentLinkedQueue)

    val newDefs = ParOps.parMapValues(root.defs)(visitDef)
    val newEnums = ParOps.parMapValues(root.enums)(visitEnum)
    val newEffs = ParOps.parMapValues(root.effects)(visitEff)

    ReducedAst.Root(newDefs, newEnums, newEffs, ctx.anonClasses.asScala.toList, root.entryPoint, root.sources)
  }

  private def visitDef(d: LiftedAst.Def)(implicit ctx: SharedContext): ReducedAst.Def = d match {
    case LiftedAst.Def(ann, mod, sym, cparams, fparams, exp, tpe, purity, loc) =>
      val cs = cparams.map(visitFormalParam)
      val fs = fparams.map(visitFormalParam)
      val e = visitExpr(exp)
      val stmt = ReducedAst.Stmt.Ret(e, e.tpe, e.loc)
      ReducedAst.Def(ann, mod, sym, cs, fs, stmt, tpe, purity, loc)
  }

  private def visitEnum(d: LiftedAst.Enum): ReducedAst.Enum = d match {
    case LiftedAst.Enum(ann, mod, sym, cases0, tpe, loc) =>
      val cases = cases0.map {
        case (sym, caze) => sym -> visitCase(caze)
      }
      val t = tpe
      ReducedAst.Enum(ann, mod, sym, cases, tpe, loc)
  }

  private def visitEff(effect: LiftedAst.Effect): ReducedAst.Effect = effect match {
    case LiftedAst.Effect(ann, mod, sym, ops0, loc) =>
      val ops = ops0.map(visitEffectOp)
      ReducedAst.Effect(ann, mod, sym, ops, loc)
  }

  private def visitEffectOp(op: LiftedAst.Op): ReducedAst.Op = op match {
    case LiftedAst.Op(sym, ann, mod, fparams0, tpe, purity, loc) =>
      val fparams = fparams0.map(visitFormalParam)
      ReducedAst.Op(sym, ann, mod, fparams, tpe, purity, loc)
  }

  private def visitExpr(exp0: LiftedAst.Expr)(implicit ctx: SharedContext): ReducedAst.Expr = exp0 match {
    case LiftedAst.Expr.Cst(cst, tpe, loc) =>
      ReducedAst.Expr.Cst(cst, tpe, loc)

    case LiftedAst.Expr.Var(sym, tpe, loc) =>
      ReducedAst.Expr.Var(sym, tpe, loc)

    case LiftedAst.Expr.ApplyAtomic(op, exps, tpe, purity, loc) =>
      val es = exps.map(visitExpr)
      ReducedAst.Expr.ApplyAtomic(op, es, tpe, purity, loc)

    case LiftedAst.Expr.ApplyClo(exp, exps, ct, tpe, purity, loc) =>
      val e = visitExpr(exp)
      val es = exps.map(visitExpr)
      ReducedAst.Expr.ApplyClo(e, es, ct, tpe, purity, loc)

    case LiftedAst.Expr.ApplyDef(sym, exps, ct, tpe, purity, loc) =>
      val es = exps.map(visitExpr)
      ReducedAst.Expr.ApplyDef(sym, es, ct, tpe, purity, loc)

    case LiftedAst.Expr.ApplySelfTail(sym, formals, exps, tpe, purity, loc) =>
      val fs = formals.map(visitFormalParam)
      val es = exps.map(visitExpr)
      ReducedAst.Expr.ApplySelfTail(sym, fs, es, tpe, purity, loc)

    case LiftedAst.Expr.IfThenElse(exp1, exp2, exp3, tpe, purity, loc) =>
      val e1 = visitExpr(exp1)
      val e2 = visitExpr(exp2)
      val e3 = visitExpr(exp3)
      ReducedAst.Expr.IfThenElse(e1, e2, e3, tpe, purity, loc)

    case LiftedAst.Expr.Branch(exp, branches, tpe, purity, loc) =>
      val e = visitExpr(exp)
      val bs = branches map {
        case (label, body) => label -> visitExpr(body)
      }
      ReducedAst.Expr.Branch(e, bs, tpe, purity, loc)

    case LiftedAst.Expr.JumpTo(sym, tpe, purity, loc) =>
      ReducedAst.Expr.JumpTo(sym, tpe, purity, loc)

    case LiftedAst.Expr.Let(sym, exp1, exp2, tpe, purity, loc) =>
      val e1 = visitExpr(exp1)
      val e2 = visitExpr(exp2)
      ReducedAst.Expr.Let(sym, e1, e2, tpe, purity, loc)

    case LiftedAst.Expr.LetRec(varSym, index, defSym, exp1, exp2, tpe, purity, loc) =>
      val e1 = visitExpr(exp1)
      val e2 = visitExpr(exp2)
      ReducedAst.Expr.LetRec(varSym, index, defSym, e1, e2, tpe, purity, loc)

    case LiftedAst.Expr.Scope(sym, exp, tpe, purity, loc) =>
      val e = visitExpr(exp)
      ReducedAst.Expr.Scope(sym, e, tpe, purity, loc)

    case LiftedAst.Expr.TryCatch(exp, rules, tpe, purity, loc) =>
      val e = visitExpr(exp)
      val rs = rules map {
        case LiftedAst.CatchRule(sym, clazz, body) =>
          val b = visitExpr(body)
          ReducedAst.CatchRule(sym, clazz, b)
      }
      ReducedAst.Expr.TryCatch(e, rs, tpe, purity, loc)

    case LiftedAst.Expr.TryWith(exp, effUse, rules, tpe, purity, loc) =>
      val e = visitExpr(exp)
      val rs = rules.map {
        case LiftedAst.HandlerRule(op, fparams, exp) =>
          val fps = fparams.map(visitFormalParam)
          val e = visitExpr(exp)
          ReducedAst.HandlerRule(op, fps, e)
      }
      ReducedAst.Expr.TryWith(e, effUse, rs, tpe, purity, loc)

    case LiftedAst.Expr.Do(op, exps, tpe, purity, loc) =>
      val es = exps.map(visitExpr)
      ReducedAst.Expr.Do(op, es, tpe, purity, loc)

    case LiftedAst.Expr.Resume(exp, tpe, loc) =>
      val e = visitExpr(exp)
      ReducedAst.Expr.Resume(e, tpe, loc)

    case LiftedAst.Expr.NewObject(name, clazz, tpe, purity, methods, loc) =>
      val es = methods.map(m => visitExpr(m.clo))
      val specs = methods.map {
        case LiftedAst.JvmMethod(ident, fparams, _, retTpe, purity, loc) =>
          val f = fparams.map(visitFormalParam)
          ReducedAst.JvmMethod(ident, f, retTpe, purity, loc)
      }
      ctx.anonClasses.add(ReducedAst.AnonClass(name, clazz, tpe, specs, loc))

      ReducedAst.Expr.NewObject(name, clazz, tpe, purity, specs, es, loc)

  }

  private def visitCase(caze: LiftedAst.Case): ReducedAst.Case = caze match {
    case LiftedAst.Case(sym, tpe, loc) =>
      ReducedAst.Case(sym, tpe, loc)
  }

  private def visitFormalParam(fparam: LiftedAst.FormalParam): ReducedAst.FormalParam = fparam match {
    case LiftedAst.FormalParam(sym, mod, tpe, loc) =>
      ReducedAst.FormalParam(sym, mod, tpe, loc)
  }

  /**
    * A context shared across threads.
    *
    * We use a concurrent (non-blocking) linked queue to ensure thread-safety.
    */
  private case class SharedContext(anonClasses: ConcurrentLinkedQueue[ReducedAst.AnonClass])

}
