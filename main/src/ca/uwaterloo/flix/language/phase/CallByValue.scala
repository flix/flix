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
import ca.uwaterloo.flix.language.ast.{AtomicOp, ControlAst, LiftedAst, Purity}

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

  private def visitExp(exp0: LiftedAst.Expression): ControlAst.Expression = exp0 match {
    case LiftedAst.Expression.Cst(cst, tpe, loc) => ControlAst.Expression.Cst(cst, tpe, loc)

    case LiftedAst.Expression.Var(sym, tpe, loc) => ControlAst.Expression.Var(sym, tpe, loc)

    case LiftedAst.Expression.Closure(sym, exps, tpe, loc) =>
      val es = exps.map(visitExp)
      ControlAst.Expression.Closure(sym, es, tpe, loc)

    case LiftedAst.Expression.ApplyClo(exp, exps, tpe, purity, loc) =>
      val e = visitExp(exp)
      val es = exps.map(visitExp)
      ControlAst.Expression.ApplyClo(e, es, tpe, purity, loc)

    case LiftedAst.Expression.ApplyDef(sym, exps, tpe, purity, loc) =>
      val es = exps.map(visitExp)
      ControlAst.Expression.ApplyDef(sym, es, tpe, purity, loc)

    case LiftedAst.Expression.ApplyCloTail(exp, exps, tpe, purity, loc) =>
      val e = visitExp(exp)
      val es = exps.map(visitExp)
      ControlAst.Expression.ApplyCloTail(e, es, tpe, purity, loc)

    case LiftedAst.Expression.ApplyDefTail(sym, exps, tpe, purity, loc) =>
      val es = exps.map(visitExp)
      ControlAst.Expression.ApplyDefTail(sym, es, tpe, purity, loc)

    case LiftedAst.Expression.ApplySelfTail(sym, formals, exps, tpe, purity, loc) =>
      val fs = formals.map(visitFormalParam)
      val as = exps.map(visitExp)
      ControlAst.Expression.ApplySelfTail(sym, fs, as, tpe, purity, loc)

    case LiftedAst.Expression.Unary(sop, _, exp, tpe, purity, loc) =>
      val op = AtomicOp.Unary(sop)
      val e = visitExp(exp)
      ControlAst.Expression.ApplyAtomic(op, List(e), tpe, purity, loc)

    case LiftedAst.Expression.Binary(sop, _, exp1, exp2, tpe, purity, loc) =>
      val op = AtomicOp.Binary(sop)
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      ControlAst.Expression.ApplyAtomic(op, List(e1, e2), tpe, purity, loc)

    case LiftedAst.Expression.IfThenElse(exp1, exp2, exp3, tpe, purity, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val e3 = visitExp(exp3)
      ControlAst.Expression.IfThenElse(e1, e2, e3, tpe, purity, loc)

    case LiftedAst.Expression.Branch(exp, branches, tpe, purity, loc) =>
      val e = visitExp(exp)
      val bs = branches map {
        case (label, body) => label -> visitExp(body)
      }
      ControlAst.Expression.Branch(e, bs, tpe, purity, loc)

    case LiftedAst.Expression.JumpTo(sym, tpe, purity, loc) =>
      ControlAst.Expression.JumpTo(sym, tpe, purity, loc)

    case LiftedAst.Expression.Let(sym, exp1, exp2, tpe, purity, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      ControlAst.Expression.Let(sym, e1, e2, tpe, purity, loc)

    case LiftedAst.Expression.LetRec(varSym, index, defSym, exp1, exp2, tpe, purity, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      ControlAst.Expression.LetRec(varSym, index, defSym, e1, e2, tpe, purity, loc)

    case LiftedAst.Expression.Region(tpe, loc) =>
      ControlAst.Expression.Region(tpe, loc)

    case LiftedAst.Expression.Scope(sym, exp, tpe, purity, loc) =>
      val e = visitExp(exp)
      ControlAst.Expression.Scope(sym, e, tpe, purity, loc)

    case LiftedAst.Expression.ScopeExit(exp1, exp2, tpe, purity, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      ControlAst.Expression.ScopeExit(e1, e2, tpe, purity, loc)

    case LiftedAst.Expression.Is(sym, exp, purity, loc) =>
      val e = visitExp(exp)
      ControlAst.Expression.Is(sym, e, purity, loc)

    case LiftedAst.Expression.Tag(sym, exp, tpe, purity, loc) =>
      val e = visitExp(exp)
      ControlAst.Expression.Tag(sym, e, tpe, purity, loc)

    case LiftedAst.Expression.Untag(sym, exp, tpe, purity, loc) =>
      val e = visitExp(exp)
      ControlAst.Expression.Untag(sym, e, tpe, purity, loc)

    case LiftedAst.Expression.Index(exp, idx, tpe, purity, loc) =>
      val op = AtomicOp.Index(idx)
      val e = visitExp(exp)
      ControlAst.Expression.ApplyAtomic(op, List(e), tpe, purity, loc)

    case LiftedAst.Expression.Tuple(exps, tpe, purity, loc) =>
      val op = AtomicOp.Tuple
      val es = exps.map(visitExp)
      ControlAst.Expression.ApplyAtomic(op, es, tpe, purity, loc)

    case LiftedAst.Expression.RecordEmpty(tpe, loc) =>
      val op = AtomicOp.RecordEmpty
      ControlAst.Expression.ApplyAtomic(op, Nil, tpe, Purity.Pure, loc)

    case LiftedAst.Expression.RecordSelect(exp, field, tpe, purity, loc) =>
      val op = AtomicOp.RecordSelect(field)
      val e = visitExp(exp)
      ControlAst.Expression.ApplyAtomic(op, List(e), tpe, purity, loc)

    case LiftedAst.Expression.RecordExtend(field, exp1, exp2, tpe, purity, loc) =>
      val op = AtomicOp.RecordExtend(field)
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      ControlAst.Expression.ApplyAtomic(op, List(e1, e2), tpe, purity, loc)

    case LiftedAst.Expression.RecordRestrict(field, exp, tpe, purity, loc) =>
      val op = AtomicOp.RecordRestrict(field)
      val e = visitExp(exp)
      ControlAst.Expression.ApplyAtomic(op, List(e), tpe, purity, loc)

    case LiftedAst.Expression.ArrayLit(exps, tpe, loc) =>
      val es = exps.map(visitExp)
      ControlAst.Expression.ArrayLit(es, tpe, loc)

    case LiftedAst.Expression.ArrayNew(exp1, exp2, tpe, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      ControlAst.Expression.ArrayNew(e1, e2, tpe, loc)

    case LiftedAst.Expression.ArrayLoad(exp1, exp2, tpe, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      ControlAst.Expression.ArrayLoad(e1, e2, tpe, loc)

    case LiftedAst.Expression.ArrayStore(exp1, exp2, exp3, tpe, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val e3 = visitExp(exp3)
      ControlAst.Expression.ArrayStore(e1, e2, e3, tpe, loc)

    case LiftedAst.Expression.ArrayLength(exp, tpe, purity, loc) =>
      val e = visitExp(exp)
      ControlAst.Expression.ArrayLength(e, tpe, purity, loc)

    case LiftedAst.Expression.Ref(exp, tpe, loc) =>
      val e = visitExp(exp)
      ControlAst.Expression.Ref(e, tpe, loc)

    case LiftedAst.Expression.Deref(exp, tpe, loc) =>
      val e = visitExp(exp)
      ControlAst.Expression.Deref(e, tpe, loc)

    case LiftedAst.Expression.Assign(exp1, exp2, tpe, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      ControlAst.Expression.Assign(e1, e2, tpe, loc)

    case LiftedAst.Expression.InstanceOf(exp, clazz, loc) =>
      val e = visitExp(exp)
      ControlAst.Expression.InstanceOf(e, clazz, loc)

    case LiftedAst.Expression.Cast(exp, tpe, purity, loc) =>
      val e = visitExp(exp)
      ControlAst.Expression.Cast(e, tpe, purity, loc)

    case LiftedAst.Expression.TryCatch(exp, rules, tpe, purity, loc) =>
      val e = visitExp(exp)
      val rs = rules map {
        case LiftedAst.CatchRule(sym, clazz, body) =>
          val b = visitExp(body)
          ControlAst.CatchRule(sym, clazz, b)
      }
      ControlAst.Expression.TryCatch(e, rs, tpe, purity, loc)

    case LiftedAst.Expression.InvokeConstructor(constructor, exps, tpe, purity, loc) =>
      val op = AtomicOp.InvokeConstructor(constructor)
      val es = exps.map(visitExp)
      ControlAst.Expression.ApplyAtomic(op, es, tpe, purity, loc)

    case LiftedAst.Expression.InvokeMethod(method, exp, exps, tpe, purity, loc) =>
      val op = AtomicOp.InvokeMethod(method)
      val e = visitExp(exp)
      val es = exps.map(visitExp)
      ControlAst.Expression.ApplyAtomic(op, e :: es, tpe, purity, loc)

    case LiftedAst.Expression.InvokeStaticMethod(method, exps, tpe, purity, loc) =>
      val op = AtomicOp.InvokeStaticMethod(method)
      val es = exps.map(visitExp)
      ControlAst.Expression.ApplyAtomic(op, es, tpe, purity, loc)

    case LiftedAst.Expression.GetField(field, exp, tpe, purity, loc) =>
      val op = AtomicOp.GetField(field)
      val e = visitExp(exp)
      ControlAst.Expression.ApplyAtomic(op, List(e), tpe, purity, loc)

    case LiftedAst.Expression.PutField(field, exp1, exp2, tpe, purity, loc) =>
      val op = AtomicOp.PutField(field)
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      ControlAst.Expression.ApplyAtomic(op, List(e1, e2), tpe, purity, loc)

    case LiftedAst.Expression.GetStaticField(field, tpe, purity, loc) =>
      val op = AtomicOp.GetStaticField(field)
      ControlAst.Expression.ApplyAtomic(op, Nil, tpe, purity, loc)

    case LiftedAst.Expression.PutStaticField(field, exp, tpe, purity, loc) =>
      val op = AtomicOp.PutStaticField(field)
      val e = visitExp(exp)
      ControlAst.Expression.ApplyAtomic(op, List(e), tpe, purity, loc)

    case LiftedAst.Expression.NewObject(name, clazz, tpe, purity, methods, loc) =>
      val ms = methods.map(visitJvmMethod)
      ControlAst.Expression.NewObject(name, clazz, tpe, purity, ms, loc)

    case LiftedAst.Expression.Spawn(exp1, exp2, tpe, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      ControlAst.Expression.Spawn(e1, e2, tpe, loc)

    case LiftedAst.Expression.Lazy(exp, tpe, loc) =>
      val e = visitExp(exp)
      ControlAst.Expression.Lazy(e, tpe, loc)

    case LiftedAst.Expression.Force(exp, tpe, loc) =>
      val e = visitExp(exp)
      ControlAst.Expression.Force(e, tpe, loc)

    case LiftedAst.Expression.HoleError(sym, tpe, loc) =>
      ControlAst.Expression.HoleError(sym, tpe, loc)

    case LiftedAst.Expression.MatchError(tpe, loc) =>
      ControlAst.Expression.MatchError(tpe, loc)
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
