/*
 * Copyright 2020-2021 Jonathan Lindegaard Starup
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
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.ErasedAst.{IntrinsicOperator0, IntrinsicOperator1, IntrinsicOperator2}
import ca.uwaterloo.flix.language.ast.{ErasedAst, FinalAst}
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

object Eraser {

  def run(root: FinalAst.Root)(implicit flix: Flix): Validation[ErasedAst.Root, CompilationMessage] = flix.phase("Eraser") {
    val defs = root.defs.map { case (k, v) => k -> visitDef(v) }
    val enums = root.enums.map { case (k, v) => k -> visitEnum(v) }

    ErasedAst.Root(defs, enums, root.entryPoint, root.sources).toSuccess
  }

  /**
    * Translates the given enum `enum0` to the ErasedAst.
    */
  private def visitEnum(enum0: FinalAst.Enum): ErasedAst.Enum = {
    val cases = enum0.cases map { case (t, c) => t -> visitCase(c) }
    ErasedAst.Enum(enum0.ann, enum0.mod, enum0.sym, cases, enum0.tpeDeprecated, enum0.loc)
  }

  /**
    * Translates the given case `case0` to the ErasedAst.
    */
  private def visitCase(case0: FinalAst.Case): ErasedAst.Case = {
    ErasedAst.Case(case0.sym, case0.tpeDeprecated, case0.loc)
  }

  /**
    * Translates the given definition `def0` to the ErasedAst.
    */
  private def visitDef(def0: FinalAst.Def): ErasedAst.Def = {
    val formals = def0.formals.map(visitFormalParam)
    val exp = visitExp(def0.exp)
    ErasedAst.Def(def0.ann, def0.mod, def0.sym, formals, exp, def0.tpe, def0.loc)
  }

  /**
    * Translates the given formal param `p` to the ErasedAst.
    */
  private def visitFormalParam(p: FinalAst.FormalParam): ErasedAst.FormalParam =
    ErasedAst.FormalParam(p.sym, p.tpe)

  /**
    * Translates the given expression `exp0` to the ErasedAst.
    */
  private def visitExp(exp0: FinalAst.Expression): ErasedAst.Expression = exp0 match {
    case FinalAst.Expression.Cst(cst, tpe, loc) =>
      ErasedAst.Expression.Cst(cst, tpe, loc)

    case FinalAst.Expression.Var(sym, tpe, loc) =>
      ErasedAst.Expression.Var(sym, tpe, loc)

    case FinalAst.Expression.Closure(sym, closureArgs, tpe, loc) =>
      ErasedAst.Expression.Closure(sym, closureArgs.map(visitExp), tpe, loc)

    case FinalAst.Expression.ApplyClo(exp, args, tpe, loc) =>
      ErasedAst.Expression.ApplyClo(visitExp(exp), args.map(visitExp), tpe, loc)

    case FinalAst.Expression.ApplyDef(sym, args, tpe, loc) =>
      ErasedAst.Expression.ApplyDef(sym, args.map(visitExp), tpe, loc)

    case FinalAst.Expression.ApplyCloTail(exp, args, tpe, loc) =>
      ErasedAst.Expression.ApplyCloTail(visitExp(exp), args.map(visitExp), tpe, loc)

    case FinalAst.Expression.ApplyDefTail(sym, args, tpe, loc) =>
      ErasedAst.Expression.ApplyDefTail(sym, args.map(visitExp), tpe, loc)

    case FinalAst.Expression.ApplySelfTail(sym, formals0, actuals, tpe, loc) =>
      val formals = formals0.map {
        case FinalAst.FormalParam(formalSym, formalTpe) => ErasedAst.FormalParam(formalSym, formalTpe)
      }
      ErasedAst.Expression.ApplySelfTail(sym, formals, actuals.map(visitExp), tpe, loc)

    case FinalAst.Expression.Unary(sop, op, exp, tpe, loc) =>
      ErasedAst.Expression.Unary(sop, op, visitExp(exp), tpe, loc)

    case FinalAst.Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
      ErasedAst.Expression.Binary(sop, op, visitExp(exp1), visitExp(exp2), tpe, loc)

    case FinalAst.Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
      ErasedAst.Expression.IfThenElse(visitExp(exp1), visitExp(exp2), visitExp(exp3), tpe, loc)

    case FinalAst.Expression.Branch(exp, branches0, tpe, loc) =>
      val branches = branches0.map {
        case (branchLabel, branchExp) => (branchLabel, visitExp(branchExp))
      }
      ErasedAst.Expression.Branch(visitExp(exp), branches, tpe, loc)

    case FinalAst.Expression.JumpTo(sym, tpe, loc) =>
      ErasedAst.Expression.JumpTo(sym, tpe, loc)

    case FinalAst.Expression.Let(sym, exp1, exp2, tpe, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      ErasedAst.Expression.Let(sym, e1, e2, tpe, loc)

    case FinalAst.Expression.LetRec(varSym, index, defSym, exp1, exp2, tpe, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      ErasedAst.Expression.LetRec(varSym, index, defSym, e1, e2, tpe, loc)

    case FinalAst.Expression.Region(tpe, loc) =>
      ErasedAst.Expression.Region(tpe, loc)

    case FinalAst.Expression.Scope(sym, exp, tpe, loc) =>
      ErasedAst.Expression.Scope(sym, visitExp(exp), tpe, loc)

    case FinalAst.Expression.ScopeExit(exp1, exp2, tpe, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      ErasedAst.Expression.ScopeExit(e1, e2, tpe, loc)

    case FinalAst.Expression.Is(sym, exp, loc) =>
      ErasedAst.Expression.Is(sym, visitExp(exp), loc)

    case FinalAst.Expression.Tag(sym, exp, tpe, loc) =>
      val op = IntrinsicOperator1.Tag(sym)
      ErasedAst.Expression.Intrinsic1(op, visitExp(exp), tpe, loc)

    case FinalAst.Expression.Untag(sym, exp, tpe, loc) =>
      val op = IntrinsicOperator1.Untag(sym)
      ErasedAst.Expression.Intrinsic1(op, visitExp(exp), tpe, loc)

    case FinalAst.Expression.Index(base, idx, tpe, loc) =>
      val op = IntrinsicOperator1.Index(idx)
      ErasedAst.Expression.Intrinsic1(op, visitExp(base), tpe, loc)

    case FinalAst.Expression.Tuple(elms, tpe, loc) =>
      ErasedAst.Expression.Tuple(elms.map(visitExp), tpe, loc)

    case FinalAst.Expression.RecordEmpty(tpe, loc) =>
      val op = IntrinsicOperator0.RecordEmpty
      ErasedAst.Expression.Intrinsic0(op, tpe, loc)

    case FinalAst.Expression.RecordSelect(exp, field, tpe, loc) =>
      val op = IntrinsicOperator1.RecordSelect(field)
      ErasedAst.Expression.Intrinsic1(op, visitExp(exp), tpe, loc)

    case FinalAst.Expression.RecordExtend(field, exp1, exp2, tpe, loc) =>
      val op = IntrinsicOperator2.RecordExtend(field)
      ErasedAst.Expression.Intrinsic2(op, visitExp(exp1), visitExp(exp2), tpe, loc)

    case FinalAst.Expression.RecordRestrict(field, exp, tpe, loc) =>
      val op = IntrinsicOperator1.RecordRestrict(field)
      ErasedAst.Expression.Intrinsic1(op, visitExp(exp), tpe, loc)

    case FinalAst.Expression.ArrayLit(elms, tpe, loc) =>
      ErasedAst.Expression.ArrayLit(elms.map(visitExp), tpe, loc)

    case FinalAst.Expression.ArrayNew(exp1, exp2, tpe, loc) =>
      val op = IntrinsicOperator2.ArrayNew
      ErasedAst.Expression.Intrinsic2(op, visitExp(exp1), visitExp(exp2), tpe, loc)

    case FinalAst.Expression.ArrayLoad(exp1, exp2, tpe, loc) =>
      val op = ErasedAst.IntrinsicOperator2.ArrayLoad
      ErasedAst.Expression.Intrinsic2(op, visitExp(exp1), visitExp(exp2), tpe, loc)

    case FinalAst.Expression.ArrayStore(exp1, exp2, exp3, tpe, loc) =>
      val op = ErasedAst.IntrinsicOperator3.ArrayStore
      ErasedAst.Expression.Intrinsic3(op, visitExp(exp1), visitExp(exp2), visitExp(exp3), tpe, loc)

    case FinalAst.Expression.ArrayLength(exp, tpe, loc) =>
      val op = IntrinsicOperator1.ArrayLength
      ErasedAst.Expression.Intrinsic1(op, visitExp(exp), tpe, loc)

    case FinalAst.Expression.Ref(exp, tpe, loc) =>
      val op = IntrinsicOperator1.Ref
      ErasedAst.Expression.Intrinsic1(op, visitExp(exp), tpe, loc)

    case FinalAst.Expression.Deref(exp, tpe, loc) =>
      val op = IntrinsicOperator1.Deref
      ErasedAst.Expression.Intrinsic1(op, visitExp(exp), tpe, loc)

    case FinalAst.Expression.Assign(exp1, exp2, tpe, loc) =>
      val op = ErasedAst.IntrinsicOperator2.Assign
      ErasedAst.Expression.Intrinsic2(op, visitExp(exp1), visitExp(exp2), tpe, loc)

    case FinalAst.Expression.Cast(exp, tpe, loc) =>
      ErasedAst.Expression.Cast(visitExp(exp), tpe, loc)

    case FinalAst.Expression.TryCatch(exp, rules0, tpe, loc) =>
      val rules = rules0.map {
        case FinalAst.CatchRule(catchSym, catchClazz, catchExp) =>
          ErasedAst.CatchRule(catchSym, catchClazz, visitExp(catchExp))
      }
      ErasedAst.Expression.TryCatch(visitExp(exp), rules, tpe, loc)

    case FinalAst.Expression.InvokeConstructor(constructor, args, tpe, loc) =>
      ErasedAst.Expression.InvokeConstructor(constructor, args.map(visitExp), tpe, loc)

    case FinalAst.Expression.InvokeMethod(method, exp, args, tpe, loc) =>
      ErasedAst.Expression.InvokeMethod(method, visitExp(exp), args.map(visitExp), tpe, loc)

    case FinalAst.Expression.InvokeStaticMethod(method, args, tpe, loc) =>
      ErasedAst.Expression.InvokeStaticMethod(method, args.map(visitExp), tpe, loc)

    case FinalAst.Expression.GetField(field, exp, tpe, loc) =>
      val op = IntrinsicOperator1.GetField(field)
      ErasedAst.Expression.Intrinsic1(op, visitExp(exp), tpe, loc)

    case FinalAst.Expression.PutField(field, exp1, exp2, tpe, loc) =>
      val op = ErasedAst.IntrinsicOperator2.PutField(field)
      ErasedAst.Expression.Intrinsic2(op, visitExp(exp1), visitExp(exp2), tpe, loc)

    case FinalAst.Expression.GetStaticField(field, tpe, loc) =>
      val op = IntrinsicOperator0.GetStaticField(field)
      ErasedAst.Expression.Intrinsic0(op, tpe, loc)

    case FinalAst.Expression.PutStaticField(field, exp, tpe, loc) =>
      val op = IntrinsicOperator1.PutStaticField(field)
      ErasedAst.Expression.Intrinsic1(op, visitExp(exp), tpe, loc)

    case FinalAst.Expression.NewObject(name, clazz, tpe, methods0, loc) =>
      val methods = methods0.map {
        case FinalAst.JvmMethod(ident, fparams, clo, retTpe, loc) =>
          val f = fparams.map(visitFormalParam)
          ErasedAst.JvmMethod(ident, f, visitExp(clo), retTpe, loc)
      }
      ErasedAst.Expression.NewObject(name, clazz, tpe, methods, loc)

    case FinalAst.Expression.Spawn(exp1, exp2, tpe, loc) =>
      val op = IntrinsicOperator2.Spawn
      ErasedAst.Expression.Intrinsic2(op, visitExp(exp1), visitExp(exp2), tpe, loc)

    case FinalAst.Expression.Lazy(exp, tpe, loc) =>
      val op = IntrinsicOperator1.Lazy
      ErasedAst.Expression.Intrinsic1(op, visitExp(exp), tpe, loc)

    case FinalAst.Expression.Force(exp, tpe, loc) =>
      val op = IntrinsicOperator1.Force
      ErasedAst.Expression.Intrinsic1(op, visitExp(exp), tpe, loc)

    case FinalAst.Expression.HoleError(sym, tpe, loc) =>
      val op = IntrinsicOperator0.HoleError(sym)
      ErasedAst.Expression.Intrinsic0(op, tpe, loc)

    case FinalAst.Expression.MatchError(tpe, loc) =>
      val op = IntrinsicOperator0.MatchError
      ErasedAst.Expression.Intrinsic0(op, tpe, loc)
  }

}
