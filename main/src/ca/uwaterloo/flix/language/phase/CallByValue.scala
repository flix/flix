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
import ca.uwaterloo.flix.language.ast.LiftedAst
import ca.uwaterloo.flix.language.ast.ControlAst
import ca.uwaterloo.flix.language.ast.LiftedAst.

object CallByValue {

  def run(root: LiftedAst.Root)(implicit flix: Flix): LiftedAst.Root = flix.phase("CallByValue") {

    val newDefs = root.defs.map {
      case (sym, d) => sym -> visitDef(d)
    }
    val newEnums = ???
    ControlAst.Root(newDefs, newEnums, root.entryPoint, root.sources)

    return root
  }

  private def visitDef(d: LiftedAst.Def): ControlAst.Def = d match {
    case LiftedAst.Def(ann, mod, sym, fparams0, exp0, tpe, loc) =>
      val fparams = ???
      val exp = visitExp(exp0)
      ControlAst.Def(ann, mod, sym, fparams, exp, tpe, loc)
  }

  private def visitExp(exp0: LiftedAst.Expression): ControlAst.Expression = exp0 match {
    case LiftedAst.Expression.Cst(cst, tpe, loc) => ???
    case LiftedAst.Expression.Var(sym, tpe, loc) => ???
    case LiftedAst.Expression.Closure(sym, closureArgs, tpe, loc) => ???
    case LiftedAst.Expression.ApplyClo(exp, args, tpe, purity, loc) => ???
    case LiftedAst.Expression.ApplyDef(sym, args, tpe, purity, loc) => ???
    case LiftedAst.Expression.ApplyCloTail(exp, args, tpe, purity, loc) => ???
    case LiftedAst.Expression.ApplyDefTail(sym, args, tpe, purity, loc) => ???
    case LiftedAst.Expression.ApplySelfTail(sym, formals, actuals, tpe, purity, loc) => ???
    case LiftedAst.Expression.Unary(sop, op, exp, tpe, purity, loc) => ???
    case LiftedAst.Expression.Binary(sop, op, exp1, exp2, tpe, purity, loc) => ???
    case LiftedAst.Expression.IfThenElse(exp1, exp2, exp3, tpe, purity, loc) => ???
    case LiftedAst.Expression.Branch(exp, branches, tpe, purity, loc) => ???
    case LiftedAst.Expression.JumpTo(sym, tpe, purity, loc) => ???
    case LiftedAst.Expression.Let(sym, exp1, exp2, tpe, purity, loc) => ???
    case LiftedAst.Expression.LetRec(varSym, index, defSym, exp1, exp2, tpe, purity, loc) => ???
    case LiftedAst.Expression.Region(tpe, loc) => ???
    case LiftedAst.Expression.Scope(sym, exp, tpe, purity, loc) => ???
    case LiftedAst.Expression.ScopeExit(exp1, exp2, tpe, purity, loc) => ???
    case LiftedAst.Expression.Is(sym, exp, purity, loc) => ???
    case LiftedAst.Expression.Tag(sym, exp, tpe, purity, loc) => ???
    case LiftedAst.Expression.Untag(sym, exp, tpe, purity, loc) => ???
    case LiftedAst.Expression.Index(base, offset, tpe, purity, loc) => ???
    case LiftedAst.Expression.Tuple(elms, tpe, purity, loc) => ???
    case LiftedAst.Expression.RecordEmpty(tpe, loc) => ???
    case LiftedAst.Expression.RecordSelect(exp, field, tpe, purity, loc) => ???
    case LiftedAst.Expression.RecordExtend(field, value, rest, tpe, purity, loc) => ???
    case LiftedAst.Expression.RecordRestrict(field, rest, tpe, purity, loc) => ???
    case LiftedAst.Expression.ArrayLit(elms, tpe, loc) => ???
    case LiftedAst.Expression.ArrayNew(elm, len, tpe, loc) => ???
    case LiftedAst.Expression.ArrayLoad(base, index, tpe, loc) => ???
    case LiftedAst.Expression.ArrayStore(base, index, elm, tpe, loc) => ???
    case LiftedAst.Expression.ArrayLength(base, tpe, purity, loc) => ???
    case LiftedAst.Expression.Ref(exp, tpe, loc) => ???
    case LiftedAst.Expression.Deref(exp, tpe, loc) => ???
    case LiftedAst.Expression.Assign(exp1, exp2, tpe, loc) => ???
    case LiftedAst.Expression.InstanceOf(exp, clazz, loc) => ???
    case LiftedAst.Expression.Cast(exp, tpe, purity, loc) => ???
    case LiftedAst.Expression.TryCatch(exp, rules, tpe, purity, loc) => ???
    case LiftedAst.Expression.InvokeConstructor(constructor, args, tpe, purity, loc) => ???
    case LiftedAst.Expression.InvokeMethod(method, exp, args, tpe, purity, loc) => ???
    case LiftedAst.Expression.InvokeStaticMethod(method, args, tpe, purity, loc) => ???
    case LiftedAst.Expression.GetField(field, exp, tpe, purity, loc) => ???
    case LiftedAst.Expression.PutField(field, exp1, exp2, tpe, purity, loc) => ???
    case LiftedAst.Expression.GetStaticField(field, tpe, purity, loc) => ???
    case LiftedAst.Expression.PutStaticField(field, exp, tpe, purity, loc) => ???
    case LiftedAst.Expression.NewObject(name, clazz, tpe, purity, methods, loc) => ???
    case LiftedAst.Expression.Spawn(exp1, exp2, tpe, loc) => ???
    case LiftedAst.Expression.Lazy(exp, tpe, loc) => ???
    case LiftedAst.Expression.Force(exp, tpe, loc) => ???
    case LiftedAst.Expression.HoleError(sym, tpe, loc) => ???
    case LiftedAst.Expression.MatchError(tpe, loc) => ???
  }

}
