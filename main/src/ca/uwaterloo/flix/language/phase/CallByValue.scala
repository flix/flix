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
import ca.uwaterloo.flix.language.ast.{Ast, AtomicOp, ControlAst, LiftedAst, Purity, Type}

object CallByValue {

  def run(root: LiftedAst.Root)(implicit flix: Flix): ControlAst.Root = flix.phase("CallByValue") {

    val newDefs = root.defs.map {
      case (sym, d) => sym -> visitDef(d)
    }
    val newEnums = root.enums.map {
      case (sym, d) => sym -> visitEnum(d)
    }

    ControlAst.Root(newDefs, newEnums, root.entryPoint, root.sources)
  }

  private def visitDef(d: LiftedAst.Def): ControlAst.Def = d match {
    case LiftedAst.Def(ann, mod, sym, fparams0, exp0, tpe, loc) =>
      val fparams = fparams0.map(visitFormalParam)
      val exp = visitExp(exp0)
      ControlAst.Def(ann, mod, sym, fparams, exp, tpe, loc)
  }

  private def visitEnum(d: LiftedAst.Enum): ControlAst.Enum = d match {
    case LiftedAst.Enum(ann, mod, sym, cases0, tpeDeprecated, loc) =>
      val cases = cases0.map {
        case (sym, caze) => sym -> visitCase(caze)
      }
      ControlAst.Enum(ann, mod, sym, cases, tpeDeprecated, loc)
  }

  private def visitExp(exp0: LiftedAst.Expression): ControlAst.Expr = exp0 match {
    case LiftedAst.Expression.Cst(cst, tpe, loc) => ControlAst.Expr.Cst(cst, tpe, loc)

    case LiftedAst.Expression.Var(sym, tpe, loc) => ControlAst.Expr.Var(sym, tpe, loc)

    case LiftedAst.Expression.Closure(sym, exps, tpe, loc) =>
      val es = exps.map(visitExp)
      ControlAst.Expr.Closure(sym, es, tpe, loc)

    case LiftedAst.Expression.ApplyClo(exp, exps, tpe, purity, loc) =>
      val e = visitExp(exp)
      val es = exps.map(visitExp)
      ControlAst.Expr.ApplyClo(e, es, Ast.CallType.NonTailCall, tpe, purity, loc)

    case LiftedAst.Expression.ApplyDef(sym, exps, tpe, purity, loc) =>
      val es = exps.map(visitExp)
      ControlAst.Expr.ApplyDef(sym, es, Ast.CallType.NonTailCall, tpe, purity, loc)

    case LiftedAst.Expression.ApplyCloTail(exp, exps, tpe, purity, loc) =>
      val e = visitExp(exp)
      val es = exps.map(visitExp)
      ControlAst.Expr.ApplyClo(e, es, Ast.CallType.TailCall, tpe, purity, loc)

    case LiftedAst.Expression.ApplyDefTail(sym, exps, tpe, purity, loc) =>
      val es = exps.map(visitExp)
      ControlAst.Expr.ApplyDef(sym, es, Ast.CallType.TailCall, tpe, purity, loc)

    case LiftedAst.Expression.ApplySelfTail(sym, formals, exps, tpe, purity, loc) =>
      val fs = formals.map(visitFormalParam)
      val as = exps.map(visitExp)
      ControlAst.Expr.ApplySelfTail(sym, fs, as, tpe, purity, loc)

    case LiftedAst.Expression.Unary(sop, _, exp, tpe, purity, loc) =>
      val op = AtomicOp.Unary(sop)
      val e = visitExp(exp)
      ControlAst.Expr.ApplyAtomic(op, List(e), tpe, purity, loc)

    case LiftedAst.Expression.Binary(sop, _, exp1, exp2, tpe, purity, loc) =>
      val op = AtomicOp.Binary(sop)
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      ControlAst.Expr.ApplyAtomic(op, List(e1, e2), tpe, purity, loc)

    case LiftedAst.Expression.IfThenElse(exp1, exp2, exp3, tpe, purity, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val e3 = visitExp(exp3)
      ControlAst.Expr.IfThenElse(e1, e2, e3, tpe, purity, loc)

    case LiftedAst.Expression.Branch(exp, branches, tpe, purity, loc) =>
      val e = visitExp(exp)
      val bs = branches map {
        case (label, body) => label -> visitExp(body)
      }
      ControlAst.Expr.Branch(e, bs, tpe, purity, loc)

    case LiftedAst.Expression.JumpTo(sym, tpe, purity, loc) =>
      ControlAst.Expr.JumpTo(sym, tpe, purity, loc)

    case LiftedAst.Expression.Let(sym, exp1, exp2, tpe, purity, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      ControlAst.Expr.Let(sym, e1, e2, tpe, purity, loc)

    case LiftedAst.Expression.LetRec(varSym, index, defSym, exp1, exp2, tpe, purity, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      ControlAst.Expr.LetRec(varSym, index, defSym, e1, e2, tpe, purity, loc)

    case LiftedAst.Expression.Region(tpe, loc) =>
      ControlAst.Expr.Region(tpe, loc)

    case LiftedAst.Expression.Scope(sym, exp, tpe, purity, loc) =>
      val e = visitExp(exp)
      ControlAst.Expr.Scope(sym, e, tpe, purity, loc)

    case LiftedAst.Expression.ScopeExit(exp1, exp2, tpe, purity, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      ControlAst.Expr.ScopeExit(e1, e2, tpe, purity, loc)

    case LiftedAst.Expression.Is(sym, exp, purity, loc) =>
      val op = AtomicOp.Is(sym)
      val e = visitExp(exp)
      ControlAst.Expr.ApplyAtomic(op, List(e), Type.Bool, purity, loc)

    case LiftedAst.Expression.Tag(sym, exp, tpe, purity, loc) =>
      val op = AtomicOp.Tag(sym)
      val e = visitExp(exp)
      ControlAst.Expr.ApplyAtomic(op, List(e), tpe, purity, loc)

    case LiftedAst.Expression.Untag(sym, exp, tpe, purity, loc) =>
      val op = AtomicOp.Untag(sym)
      val e = visitExp(exp)
      ControlAst.Expr.ApplyAtomic(op, List(e), tpe, purity, loc)

    case LiftedAst.Expression.Index(exp, idx, tpe, purity, loc) =>
      val op = AtomicOp.Index(idx)
      val e = visitExp(exp)
      ControlAst.Expr.ApplyAtomic(op, List(e), tpe, purity, loc)

    case LiftedAst.Expression.Tuple(exps, tpe, purity, loc) =>
      val op = AtomicOp.Tuple
      val es = exps.map(visitExp)
      ControlAst.Expr.ApplyAtomic(op, es, tpe, purity, loc)

    case LiftedAst.Expression.RecordEmpty(tpe, loc) =>
      val op = AtomicOp.RecordEmpty
      ControlAst.Expr.ApplyAtomic(op, Nil, tpe, Purity.Pure, loc)

    case LiftedAst.Expression.RecordSelect(exp, field, tpe, purity, loc) =>
      val op = AtomicOp.RecordSelect(field)
      val e = visitExp(exp)
      ControlAst.Expr.ApplyAtomic(op, List(e), tpe, purity, loc)

    case LiftedAst.Expression.RecordExtend(field, exp1, exp2, tpe, purity, loc) =>
      val op = AtomicOp.RecordExtend(field)
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      ControlAst.Expr.ApplyAtomic(op, List(e1, e2), tpe, purity, loc)

    case LiftedAst.Expression.RecordRestrict(field, exp, tpe, purity, loc) =>
      val op = AtomicOp.RecordRestrict(field)
      val e = visitExp(exp)
      ControlAst.Expr.ApplyAtomic(op, List(e), tpe, purity, loc)

    case LiftedAst.Expression.ArrayLit(exps, tpe, loc) =>
      val op = AtomicOp.ArrayLit
      val es = exps.map(visitExp)
      ControlAst.Expr.ApplyAtomic(op, es, tpe, Purity.Impure, loc)

    case LiftedAst.Expression.ArrayNew(exp1, exp2, tpe, loc) =>
      val op = AtomicOp.ArrayNew
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      ControlAst.Expr.ApplyAtomic(op, List(e1, e2), tpe, Purity.Impure, loc) // TODO: Use effect from earlier phase.

    case LiftedAst.Expression.ArrayLoad(exp1, exp2, tpe, loc) =>
      val op = AtomicOp.ArrayLoad
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      ControlAst.Expr.ApplyAtomic(op, List(e1, e2), tpe, Purity.Impure, loc) // TODO: Use effect from earlier phase.

    case LiftedAst.Expression.ArrayStore(exp1, exp2, exp3, tpe, loc) =>
      val op = AtomicOp.ArrayStore
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val e3 = visitExp(exp3)
      ControlAst.Expr.ApplyAtomic(op, List(e1, e2, e3), tpe, Purity.Impure, loc) // TODO: Use effect from earlier phase.

    case LiftedAst.Expression.ArrayLength(exp, tpe, purity, loc) =>
      val op = AtomicOp.ArrayLength
      val e = visitExp(exp)
      ControlAst.Expr.ApplyAtomic(op, List(e), tpe, purity, loc) // TODO: Use effect from earlier phase.

    case LiftedAst.Expression.Ref(exp, tpe, loc) =>
      val op = AtomicOp.Ref
      val e = visitExp(exp)
      ControlAst.Expr.ApplyAtomic(op, List(e), tpe, Purity.Impure, loc)

    case LiftedAst.Expression.Deref(exp, tpe, loc) =>
      val op = AtomicOp.Deref
      val e = visitExp(exp)
      ControlAst.Expr.ApplyAtomic(op, List(e), tpe, Purity.Impure, loc)

    case LiftedAst.Expression.Assign(exp1, exp2, tpe, loc) =>
      val op = AtomicOp.Assign
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      ControlAst.Expr.ApplyAtomic(op, List(e1, e2), tpe, Purity.Impure, loc)

    case LiftedAst.Expression.InstanceOf(exp, clazz, loc) =>
      val op = AtomicOp.InstanceOf(clazz)
      val e = visitExp(exp)
      ControlAst.Expr.ApplyAtomic(op, List(e), Type.Bool, Purity.Pure, loc)

    case LiftedAst.Expression.Cast(exp, tpe, purity, loc) =>
      val op = AtomicOp.Cast
      val e = visitExp(exp)
      ControlAst.Expr.ApplyAtomic(op, List(e), tpe, purity, loc)

    case LiftedAst.Expression.TryCatch(exp, rules, tpe, purity, loc) =>
      val e = visitExp(exp)
      val rs = rules map {
        case LiftedAst.CatchRule(sym, clazz, body) =>
          val b = visitExp(body)
          ControlAst.CatchRule(sym, clazz, b)
      }
      ControlAst.Expr.TryCatch(e, rs, tpe, purity, loc)

    case LiftedAst.Expression.InvokeConstructor(constructor, exps, tpe, purity, loc) =>
      val op = AtomicOp.InvokeConstructor(constructor)
      val es = exps.map(visitExp)
      ControlAst.Expr.ApplyAtomic(op, es, tpe, purity, loc)

    case LiftedAst.Expression.InvokeMethod(method, exp, exps, tpe, purity, loc) =>
      val op = AtomicOp.InvokeMethod(method)
      val e = visitExp(exp)
      val es = exps.map(visitExp)
      ControlAst.Expr.ApplyAtomic(op, e :: es, tpe, purity, loc)

    case LiftedAst.Expression.InvokeStaticMethod(method, exps, tpe, purity, loc) =>
      val op = AtomicOp.InvokeStaticMethod(method)
      val es = exps.map(visitExp)
      ControlAst.Expr.ApplyAtomic(op, es, tpe, purity, loc)

    case LiftedAst.Expression.GetField(field, exp, tpe, purity, loc) =>
      val op = AtomicOp.GetField(field)
      val e = visitExp(exp)
      ControlAst.Expr.ApplyAtomic(op, List(e), tpe, purity, loc)

    case LiftedAst.Expression.PutField(field, exp1, exp2, tpe, purity, loc) =>
      val op = AtomicOp.PutField(field)
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      ControlAst.Expr.ApplyAtomic(op, List(e1, e2), tpe, purity, loc)

    case LiftedAst.Expression.GetStaticField(field, tpe, purity, loc) =>
      val op = AtomicOp.GetStaticField(field)
      ControlAst.Expr.ApplyAtomic(op, Nil, tpe, purity, loc)

    case LiftedAst.Expression.PutStaticField(field, exp, tpe, purity, loc) =>
      val op = AtomicOp.PutStaticField(field)
      val e = visitExp(exp)
      ControlAst.Expr.ApplyAtomic(op, List(e), tpe, purity, loc)

    case LiftedAst.Expression.NewObject(name, clazz, tpe, purity, methods, loc) =>
      val ms = methods.map(visitJvmMethod)
      ControlAst.Expr.NewObject(name, clazz, tpe, purity, ms, loc)

    case LiftedAst.Expression.Spawn(exp1, exp2, tpe, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      ControlAst.Expr.Spawn(e1, e2, tpe, loc)

    case LiftedAst.Expression.Lazy(exp, tpe, loc) =>
      val op = AtomicOp.Lazy
      val e = visitExp(exp)
      ControlAst.Expr.ApplyAtomic(op, List(e), tpe, Purity.Pure, loc)

    case LiftedAst.Expression.Force(exp, tpe, loc) =>
      val op = AtomicOp.Force
      val e = visitExp(exp)
      ControlAst.Expr.ApplyAtomic(op, List(e), tpe, Purity.Pure, loc)

    case LiftedAst.Expression.HoleError(sym, tpe, loc) =>
      val op = AtomicOp.HoleError(sym)
      ControlAst.Expr.ApplyAtomic(op, Nil, tpe, Purity.Pure, loc)

    case LiftedAst.Expression.MatchError(tpe, loc) =>
      val op = AtomicOp.MatchError
      ControlAst.Expr.ApplyAtomic(op, Nil, tpe, Purity.Pure, loc)
  }

  private def visitCase(caze: LiftedAst.Case): ControlAst.Case = caze match {
    case LiftedAst.Case(sym, tpeDeprecated, loc) => ControlAst.Case(sym, tpeDeprecated, loc)
  }

  private def visitFormalParam(fparam: LiftedAst.FormalParam): ControlAst.FormalParam = fparam match {
    case LiftedAst.FormalParam(sym, mod, tpe, loc) => ControlAst.FormalParam(sym, mod, tpe, loc)
  }

  private def visitJvmMethod(m: LiftedAst.JvmMethod): ControlAst.JvmMethod = m match {
    case LiftedAst.JvmMethod(ident, fparams, clo, retTpe, purity, loc) =>
      val c = visitExp(clo)
      val fs = fparams.map(visitFormalParam)
      ControlAst.JvmMethod(ident, fs, c, retTpe, purity, loc)
  }

}
