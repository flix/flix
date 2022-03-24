/*
 * Copyright 2018 Magnus Madsen, Anna Krogh, Patrick Lundvig, Christian Bonde
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
import ca.uwaterloo.flix.language.ast.OccurrenceAst._
import ca.uwaterloo.flix.language.ast.{OccurrenceAst, Purity, Symbol}
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

/**
 * The inliner replaces closures and functions by their code to improve performance.
 */
object Inliner {

  /**
   * Performs inlining on the given AST `root`.
   */
  def run(root: OccurrenceAst.Root)(implicit flix: Flix): Validation[Root, CompilationMessage] = flix.subphase("Inliner") {
    // Visit every definition in the program.
    val defs = root.defs.map {
      case (sym, defn) => sym -> visitDef(defn)
    }

    // Reassemble the ast root.
    val result = root.copy(defs = defs)

    result.toSuccess
  }

  private def visitDef(def0: Def, defs: Map[Symbol.DefnSym, Def]): Def = {
    val exp = visitExp(def0.exp, Map.empty)
    if (def0.occurDef.isConstantNonSelfCall) {
      //TODO get name of the function being called in occurdef.
    }
    def0.copy(exp = visitExp(def0.exp, Map.empty))
  }

  /**
   * Performs inlining operations on the expression `exp0` of type OccurrenceAst.Expression.
   * Returns an expression of type Expression
   */
  private def visitExp(exp0: OccurrenceAst.Expression, subst0: Map[Symbol.VarSym, Expression]): Expression = exp0 match {
    case Expression.Unit(_) => exp0

    case Expression.Null(_, _) => exp0

    case Expression.True(_) => exp0

    case Expression.False(_) => exp0

    case Expression.Char(_, _) => exp0

    case Expression.Float32(_, _) => exp0

    case Expression.Float64(_, _) => exp0

    case Expression.Int8(_, _) => exp0

    case Expression.Int16(_, _) => exp0

    case Expression.Int32(_, _) => exp0

    case Expression.Int64(_, _) => exp0

    case Expression.BigInt(_, _) => exp0

    case Expression.Str(_, _) => exp0

    case Expression.Var(sym, _, _) => subst0.get(sym).fold(exp0)(visitExp(_, subst0))

    case Expression.Closure(_, _, _, _) => exp0

    case Expression.ApplyClo(exp, args, tpe, loc) =>
      val e = visitExp(exp, subst0)
      val as = args.map(visitExp(_, subst0))
      Expression.ApplyClo(e, as, tpe, loc)

    case Expression.ApplyDef(sym, args, tpe, loc) =>
      val as = args.map(visitExp(_, subst0))
      Expression.ApplyDef(sym, as, tpe, loc)

    case Expression.ApplyCloTail(exp, args, tpe, loc) =>
      val e = visitExp(exp, subst0)
      val as = args.map(visitExp(_, subst0))
      Expression.ApplyCloTail(e, as, tpe, loc)

    case Expression.ApplyDefTail(sym, args, tpe, loc) =>
      val as = args.map(visitExp(_, subst0))
      Expression.ApplyDefTail(sym, as, tpe, loc)

    case Expression.ApplySelfTail(sym, formals, actuals, tpe, loc) =>
      val as = actuals.map(visitExp(_, subst0))
      val fs = formals map {
        case OccurrenceAst.FormalParam(sym, mod, tpe, loc) => FormalParam(sym, mod, tpe, loc)
      }
      Expression.ApplySelfTail(sym, fs, as, tpe, loc)

    case Expression.Unary(sop, op, exp, tpe, loc) =>
      val e = visitExp(exp, subst0)
      Expression.Unary(sop, op, e, tpe, loc)

    case Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
      val e1 = visitExp(exp1, subst0)
      val e2 = visitExp(exp2, subst0)
      Expression.Binary(sop, op, e1, e2, tpe, loc)

    case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
      val e1 = visitExp(exp1, subst0)
      val e2 = visitExp(exp2, subst0)
      val e3 = visitExp(exp3, subst0)
      Expression.IfThenElse(e1, e2, e3, tpe, loc)

    case Expression.Branch(exp, branches, tpe, loc) =>
      val e = visitExp(exp, subst0)
      val bs = branches.map {
        case (sym, br) => sym -> visitExp(br, subst0)
      }
      Expression.Branch(e, bs, tpe, loc)

    case Expression.JumpTo(_, _, _) => exp0

    case Expression.Let(sym, exp1, exp2, occur, tpe, purity, loc) =>
      val wantToPreInline = (occur, purity) match {
        case (Occur.Once, Purity.Pure) => true
        case _ => false
      }
      if (wantToPreInline) {
        val subst1 = subst0 + (sym -> exp1)
        visitExp(exp2, subst1)
      } else {
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        Expression.Let(sym, e1, e2, occur, tpe, purity, loc)
      }

    case Expression.LetRec(varSym, index, defSym, exp1, exp2, tpe, loc) =>
      val e1 = visitExp(exp1, subst0)
      val e2 = visitExp(exp2, subst0)
      Expression.LetRec(varSym, index, defSym, e1, e2, tpe, loc)

    case Expression.Is(sym, tag, exp, loc) =>
      val e = visitExp(exp, subst0)
      Expression.Is(sym, tag, e, loc)

    case Expression.Tag(sym, tag, exp, tpe, loc) =>
      val e = visitExp(exp, subst0)
      Expression.Tag(sym, tag, e, tpe, loc)

    case Expression.Untag(sym, tag, exp, tpe, loc) =>
      val e = visitExp(exp, subst0)
      Expression.Untag(sym, tag, e, tpe, loc)

    case Expression.Index(base, offset, tpe, loc) =>
      val b = visitExp(base, subst0)
      Expression.Index(b, offset, tpe, loc)

    case Expression.Tuple(elms, tpe, loc) =>
      val es = elms.map(visitExp(_, subst0))
      Expression.Tuple(es, tpe, loc)

    case Expression.RecordEmpty(_, _) => exp0

    case Expression.RecordSelect(exp, field, tpe, loc) =>
      val e = visitExp(exp, subst0)
      Expression.RecordSelect(e, field, tpe, loc)

    case Expression.RecordExtend(field, value, rest, tpe, loc) =>
      val v = visitExp(value, subst0)
      val r = visitExp(rest, subst0)
      Expression.RecordExtend(field, v, r, tpe, loc)

    case Expression.RecordRestrict(field, rest, tpe, loc) =>
      val r = visitExp(rest, subst0)
      Expression.RecordRestrict(field, r, tpe, loc)

    case Expression.ArrayLit(elms, tpe, loc) =>
      val es = elms.map(visitExp(_, subst0))
      Expression.ArrayLit(es, tpe, loc)

    case Expression.ArrayNew(elm, len, tpe, loc) =>
      val e = visitExp(elm, subst0)
      val l = visitExp(len, subst0)
      Expression.ArrayNew(e, l, tpe, loc)

    case Expression.ArrayLoad(base, index, tpe, loc) =>
      val b = visitExp(base, subst0)
      val i = visitExp(index, subst0)
      Expression.ArrayLoad(b, i, tpe, loc)

    case Expression.ArrayStore(base, index, elm, tpe, loc) =>
      val b = visitExp(base, subst0)
      val i = visitExp(index, subst0)
      val e = visitExp(elm, subst0)
      Expression.ArrayStore(b, i, e, tpe, loc)

    case Expression.ArrayLength(base, tpe, loc) =>
      val b = visitExp(base, subst0)
      Expression.ArrayLength(b, tpe, loc)

    case Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) =>
      val b = visitExp(base, subst0)
      val i1 = visitExp(beginIndex, subst0)
      val i2 = visitExp(endIndex, subst0)
      Expression.ArraySlice(b, i1, i2, tpe, loc)

    case Expression.Ref(exp, tpe, loc) =>
      val e = visitExp(exp, subst0)
      Expression.Ref(e, tpe, loc)

    case Expression.Deref(exp, tpe, loc) =>
      val e = visitExp(exp, subst0)
      Expression.Deref(e, tpe, loc)

    case Expression.Assign(exp1, exp2, tpe, loc) =>
      val e1 = visitExp(exp1, subst0)
      val e2 = visitExp(exp2, subst0)
      Expression.Assign(e1, e2, tpe, loc)

    case Expression.Cast(exp, tpe, loc) =>
      val e = visitExp(exp, subst0)
      Expression.Cast(e, tpe, loc)

    case Expression.TryCatch(exp, rules, tpe, loc) =>
      val e = visitExp(exp, subst0)
      val rs = rules.map {
        case OccurrenceAst.CatchRule(sym, clazz, exp) =>
          val e = visitExp(exp, subst0)
          CatchRule(sym, clazz, e)
      }
      Expression.TryCatch(e, rs, tpe, loc)

    case Expression.InvokeConstructor(constructor, args, tpe, loc) =>
      val as = args.map(visitExp(_, subst0))
      Expression.InvokeConstructor(constructor, as, tpe, loc)

    case Expression.InvokeMethod(method, exp, args, tpe, loc) =>
      val e = visitExp(exp, subst0)
      val as = args.map(visitExp(_, subst0))
      Expression.InvokeMethod(method, e, as, tpe, loc)

    case Expression.InvokeStaticMethod(method, args, tpe, loc) =>
      val as = args.map(visitExp(_, subst0))
      Expression.InvokeStaticMethod(method, as, tpe, loc)

    case Expression.GetField(field, exp, tpe, loc) =>
      val e = visitExp(exp, subst0)
      Expression.GetField(field, e, tpe, loc)

    case Expression.PutField(field, exp1, exp2, tpe, loc) =>
      val e1 = visitExp(exp1, subst0)
      val e2 = visitExp(exp2, subst0)
      Expression.PutField(field, e1, e2, tpe, loc)

    case Expression.GetStaticField(_,_,_) => exp0

    case Expression.PutStaticField(field, exp, tpe, loc) =>
      val e = visitExp(exp, subst0)
      Expression.PutStaticField(field, e, tpe, loc)

    case Expression.NewChannel(exp, tpe, loc) =>
      val e = visitExp(exp, subst0)
      Expression.NewChannel(e, tpe, loc)

    case Expression.GetChannel(exp, tpe, loc) =>
      val e = visitExp(exp, subst0)
      Expression.GetChannel(e, tpe, loc)

    case Expression.PutChannel(exp1, exp2, tpe, loc) =>
      val e1 = visitExp(exp1, subst0)
      val e2 = visitExp(exp2, subst0)
      Expression.PutChannel(e1, e2, tpe, loc)

    case Expression.SelectChannel(rules, default, tpe, loc) =>
      val rs = rules.map {
        case OccurrenceAst.SelectChannelRule(sym, chan, exp) =>
          val c = visitExp(chan, subst0)
          val e = visitExp(exp, subst0)
          SelectChannelRule(sym, c, e)
      }
      val d = default.map(visitExp(_, subst0))
      Expression.SelectChannel(rs, d, tpe, loc)

    case Expression.Spawn(exp, tpe, loc) =>
      val e = visitExp(exp, subst0)
      Expression.Spawn(e, tpe, loc)

    case Expression.Lazy(exp, tpe, loc) =>
      val e = visitExp(exp, subst0)
      Expression.Lazy(e, tpe, loc)

    case Expression.Force(exp, tpe, loc) =>
      val e = visitExp(exp, subst0)
      Expression.Force(e, tpe, loc)

    case Expression.HoleError(_, _, _) => exp0

    case Expression.MatchError(_, _) => exp0
  }
}
