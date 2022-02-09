/*
 * Copyright 2018 Magnus Madsen
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
import ca.uwaterloo.flix.language.ast.LiftedAst.{Expression, Root}
import ca.uwaterloo.flix.language.ast.Symbol.LabelSym
import ca.uwaterloo.flix.language.ast.{LiftedAst, Symbol}
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.Validation

/**
  * The inliner replaces closures and functions by their code to improve performance.
  */
object Inliner {

  /**
    * Performs inlining on the given AST `root`.
    */
  def run(root: Root)(implicit flix: Flix): Validation[Root, CompilationMessage] = flix.phase("Inliner") {
    // TODO: Implement inliner.
    //visitExp()
    return root.toSuccess
  }


  def visitExp(exp: Expression, subst: Map[Symbol.VarSym, Int]): Expression = exp match {
    case Expression.Unit(loc) => exp
    case Expression.Null(tpe, loc) => exp
    case Expression.True(loc) => exp
    case Expression.False(loc) => exp
    case Expression.Char(lit, loc) => exp
    case Expression.Float32(lit, loc) => exp
    case Expression.Float64(lit, loc) => exp
    case Expression.Int8(lit, loc) => exp
    case Expression.Int16(lit, loc) => exp
    case Expression.Int32(lit, loc) => exp
    case Expression.Int64(lit, loc) => exp
    case Expression.BigInt(lit, loc) => exp
    case Expression.Str(lit, loc) => exp

    case Expression.Var(sym, tpe, loc) => subst.get(sym) match {
      case Some(value) => Expression.Int32(value,loc)
      case None => exp
    }
    case Expression.Closure(sym, freeVars, tpe, loc) => exp
    case Expression.ApplyClo(exp, args, tpe, loc) =>
      val e = visitExp(exp, subst)
      val a = args.map(e => visitExp(e, subst))
      Expression.ApplyClo(e, a, tpe, loc)
    case Expression.ApplyDef(sym, args, tpe, loc) =>
      val a = args.map(e => visitExp(e, subst))
      Expression.ApplyDef(sym, a, tpe, loc)
    case Expression.ApplyCloTail(exp, args, tpe, loc) =>
      val e = visitExp(exp, subst)
      val a = args.map(e => visitExp(e, subst))
      Expression.ApplyCloTail(e, a, tpe, loc)
    case Expression.ApplyDefTail(sym, args, tpe, loc) =>
      val a = args.map(e => visitExp(e, subst))
      Expression.ApplyDefTail(sym, a, tpe, loc)
    case Expression.ApplySelfTail(sym, formals, actuals, tpe, loc) =>
      val a = actuals.map(e => visitExp(e, subst))
      Expression.ApplySelfTail(sym, formals, a, tpe, loc)
    case Expression.Unary(sop, op, exp, tpe, loc) =>
      val e1 = visitExp(exp,subst)
      Expression.Unary(sop,op,e1,tpe,loc)
    case Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
      val e1 = visitExp(exp1, subst)
      val e2 = visitExp(exp2, subst)
      Expression.Binary(sop,op,e1,e2,tpe,loc)
    case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
      val e1 = visitExp(exp1, subst)
      val e2 = visitExp(exp2, subst)
      val e3 = visitExp(exp3, subst)
      Expression.IfThenElse(e1,e2,e3,tpe,loc)
    case Expression.Branch(exp, branches, tpe, loc) =>
      val e = visitExp(exp, subst)
      val bs = branches map {
        case (sym, br) => sym -> visitExp(br, subst)
      }
      Expression.Branch(e, bs, tpe, loc)

    case Expression.JumpTo(sym, tpe, loc) => exp

    //let x = let y = 5; y;

    // let x = 2+40;
    // x + 123;
    // x
    case Expression.Let(sym, exp1, exp2, tpe, loc) =>
      val e1 = visitExp(exp1, subst)
      val wantToInline: Boolean = e1 match {
      case LiftedAst.Expression.Int32(lit, _) if lit == 42 => true
      case _ => false
    }
      val subst1 = if (wantToInline) subst + (sym -> 42) else subst
      val e2 = visitExp(exp2, subst1)
      Expression.Let(sym,e1,e2,tpe, loc)
    case Expression.LetRec(varSym, index, defSym, exp1, exp2, tpe, loc) =>
      val e1 = visitExp(exp1, subst)
      val e2 = visitExp(exp2, subst)
      Expression.LetRec(varSym, index, defSym, e1, e2, tpe, loc)
    case Expression.Is(sym, tag, exp, loc) =>
      val e= visitExp(exp, subst)
      Expression.Is(sym, tag, e, loc)
    case Expression.Tag(sym, tag, exp, tpe, loc) => ???
    case Expression.Untag(sym, tag, exp, tpe, loc) => ???
    case Expression.Index(base, offset, tpe, loc) => ???
    case Expression.Tuple(elms, tpe, loc) => ???
    case Expression.RecordEmpty(tpe, loc) => ???
    case Expression.RecordSelect(exp, field, tpe, loc) => ???
    case Expression.RecordExtend(field, value, rest, tpe, loc) => ???
    case Expression.RecordRestrict(field, rest, tpe, loc) => ???
    case Expression.ArrayLit(elms, tpe, loc) => ???
    case Expression.ArrayNew(elm, len, tpe, loc) => ???
    case Expression.ArrayLoad(base, index, tpe, loc) => ???
    case Expression.ArrayStore(base, index, elm, tpe, loc) => ???
    case Expression.ArrayLength(base, tpe, loc) => ???
    case Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) => ???
    case Expression.Ref(exp, tpe, loc) => ???
    case Expression.Deref(exp, tpe, loc) => ???
    case Expression.Assign(exp1, exp2, tpe, loc) => ???
    case Expression.Cast(exp, tpe, loc) => ???
    case Expression.TryCatch(exp, rules, tpe, loc) => ???
    case Expression.InvokeConstructor(constructor, args, tpe, loc) => ???
    case Expression.InvokeMethod(method, exp, args, tpe, loc) => ???
    case Expression.InvokeStaticMethod(method, args, tpe, loc) => ???
    case Expression.GetField(field, exp, tpe, loc) => ???
    case Expression.PutField(field, exp1, exp2, tpe, loc) => ???
    case Expression.GetStaticField(field, tpe, loc) => ???
    case Expression.PutStaticField(field, exp, tpe, loc) => ???
    case Expression.NewChannel(exp, tpe, loc) => ???
    case Expression.GetChannel(exp, tpe, loc) => ???
    case Expression.PutChannel(exp1, exp2, tpe, loc) => ???
    case Expression.SelectChannel(rules, default, tpe, loc) => ???
    case Expression.Spawn(exp, tpe, loc) => ???
    case Expression.Lazy(exp, tpe, loc) => ???
    case Expression.Force(exp, tpe, loc) => ???
    case Expression.HoleError(sym, tpe, loc) => ???
    case Expression.MatchError(tpe, loc) => ???
  }
}
