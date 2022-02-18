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
import ca.uwaterloo.flix.language.ast.OccurrenceAst.Expression
import ca.uwaterloo.flix.language.ast.{LiftedAst, OccurrenceAst, Symbol}
import ca.uwaterloo.flix.language.debug.PrettyPrinter
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.{Formatter, Validation}

/**
 * The inliner replaces closures and functions by their code to improve performance.
 */
object Inliner {

  /**
   * Performs inlining on the given AST `root`.
   */
  def run(root: OccurrenceAst.Root)(implicit flix: Flix): Validation[LiftedAst.Root, CompilationMessage] = flix.phase("Inliner") {
    // Visit every definition in the program.
    val defs = root.defs.map {
      case (sym, defn) =>
        val fparams = defn.fparams.map { case OccurrenceAst.FormalParam(sym, mod, tpe, loc) => LiftedAst.FormalParam(sym, mod, tpe, loc) }
        val e = visitExp(defn.exp, Map.empty)
        sym -> LiftedAst.Def(defn.ann, defn.mod, defn.sym, fparams, e, defn.tpe, defn.loc)
      }
    val enums = root.enums.map {
      case (sym, enum) =>
        val cases = enum.cases.map {
          case (tag, caze) =>
            tag -> LiftedAst.Case(caze.sym, tag, caze.tpeDeprecated, caze.loc)
        }
        sym -> LiftedAst.Enum(enum.mod, enum.sym, cases, enum.tpeDeprecated, enum.loc)
    }
    // Reassemble the ast root.
    val result = LiftedAst.Root(defs, enums, root.reachable, root.sources)

    // Print the ast if debugging is enabled.
    if (flix.options.debug) {
      println(PrettyPrinter.Lifted.fmtRoot(result, Formatter.AnsiTerminalFormatter))
    }

    result.toSuccess
  }


  /**
   * Performs inlining operations on the expression `exp0` of type OccurrenceAst.Expression.
   * Returns an expression of type LiftedAst.Expression
   */
  private def visitExp(exp0: OccurrenceAst.Expression, subst0: Map[Symbol.VarSym, Int]): LiftedAst.Expression = exp0 match {
    case Expression.Unit(loc) => LiftedAst.Expression.Unit(loc)

    case Expression.Null(tpe, loc) => LiftedAst.Expression.Null(tpe, loc)

    case Expression.True(loc) => LiftedAst.Expression.True(loc)

    case Expression.False(loc) => LiftedAst.Expression.False(loc)

    case Expression.Char(lit, loc) => LiftedAst.Expression.Char(lit, loc)

    case Expression.Float32(lit, loc) => LiftedAst.Expression.Float32(lit, loc)

    case Expression.Float64(lit, loc) => LiftedAst.Expression.Float64(lit, loc)

    case Expression.Int8(lit, loc) => LiftedAst.Expression.Int8(lit, loc)

    case Expression.Int16(lit, loc) => LiftedAst.Expression.Int16(lit, loc)

    case Expression.Int32(lit, loc) => LiftedAst.Expression.Int32(lit, loc)

    case Expression.Int64(lit, loc) => LiftedAst.Expression.Int64(lit, loc)

    case Expression.BigInt(lit, loc) => LiftedAst.Expression.BigInt(lit, loc)

    case Expression.Str(lit, loc) => LiftedAst.Expression.Str(lit, loc)

    case Expression.Var(sym, tpe, loc) => subst0.get(sym) match {
      case Some(lit) => LiftedAst.Expression.Int32(lit, loc)
      case None => LiftedAst.Expression.Var(sym, tpe, loc)
    }

    case Expression.Closure(sym, freeVars, tpe, loc) =>
      val fv = freeVars.map {
      case OccurrenceAst.FreeVar(sym, tpe) => LiftedAst.FreeVar(sym, tpe)
    }
      LiftedAst.Expression.Closure(sym, fv, tpe, loc)

    case Expression.ApplyClo(exp, args, tpe, loc) =>
      val e = visitExp(exp, subst0)
      val as = args.map(visitExp(_, subst0))
      LiftedAst.Expression.ApplyClo(e, as, tpe, loc)

    case Expression.ApplyDef(sym, args, tpe, loc) =>
      val as = args.map(visitExp(_, subst0))
      LiftedAst.Expression.ApplyDef(sym, as, tpe, loc)

    case Expression.ApplyCloTail(exp, args, tpe, loc) =>
      val e = visitExp(exp, subst0)
      val as = args.map(visitExp(_, subst0))
      LiftedAst.Expression.ApplyCloTail(e, as, tpe, loc)

    case Expression.ApplyDefTail(sym, args, tpe, loc) =>
      val as = args.map(visitExp(_, subst0))
      LiftedAst.Expression.ApplyDefTail(sym, as, tpe, loc)

    case Expression.ApplySelfTail(sym, formals, actuals, tpe, loc) =>
      val as = actuals.map(visitExp(_, subst0))
      val fs = formals map {
        case OccurrenceAst.FormalParam(sym, mod, tpe, loc) => LiftedAst.FormalParam(sym, mod, tpe, loc)
      }
      LiftedAst.Expression.ApplySelfTail(sym, fs, as, tpe, loc)

    case Expression.Unary(sop, op, exp, tpe, loc) =>
      val e = visitExp(exp, subst0)
      LiftedAst.Expression.Unary(sop, op, e, tpe, loc)

    case Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
      val e1 = visitExp(exp1, subst0)
      val e2 = visitExp(exp2, subst0)
      LiftedAst.Expression.Binary(sop, op, e1, e2, tpe, loc)

    case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
      val e1 = visitExp(exp1, subst0)
      val e2 = visitExp(exp2, subst0)
      val e3 = visitExp(exp3, subst0)
      LiftedAst.Expression.IfThenElse(e1, e2, e3, tpe, loc)

    case Expression.Branch(exp, branches, tpe, loc) =>
      val e = visitExp(exp, subst0)
      val bs = branches.map {
        case (sym, br) => sym -> visitExp(br, subst0)
      }
      LiftedAst.Expression.Branch(e, bs, tpe, loc)

    case Expression.JumpTo(sym, tpe, loc) => LiftedAst.Expression.JumpTo(sym, tpe, loc)

    case Expression.Let(sym, exp1, exp2, occur, tpe, loc) =>
      val e1 = visitExp(exp1, subst0)
      val wantToInline: Boolean = e1 match {
        case LiftedAst.Expression.Int32(lit, _) if lit == 42 => true
        case _ => false
      }
      val subst1 = if (wantToInline) subst0 + (sym -> 42) else subst0
      val e2 = visitExp(exp2, subst1)
      LiftedAst.Expression.Let(sym, e1, e2, tpe, loc)

    case Expression.LetRec(varSym, index, defSym, exp1, exp2, tpe, loc) =>
      val e1 = visitExp(exp1, subst0)
      val e2 = visitExp(exp2, subst0)
      LiftedAst.Expression.LetRec(varSym, index, defSym, e1, e2, tpe, loc)

    case Expression.Is(sym, tag, exp, loc) =>
      val e = visitExp(exp, subst0)
      LiftedAst.Expression.Is(sym, tag, e, loc)

    case Expression.Tag(sym, tag, exp, tpe, loc) =>
      val e = visitExp(exp, subst0)
      LiftedAst.Expression.Tag(sym, tag, e, tpe, loc)

    case Expression.Untag(sym, tag, exp, tpe, loc) =>
      val e = visitExp(exp, subst0)
      LiftedAst.Expression.Untag(sym, tag, e, tpe, loc)

    case Expression.Index(base, offset, tpe, loc) =>
      val b = visitExp(base, subst0)
      LiftedAst.Expression.Index(b, offset, tpe, loc)

    case Expression.Tuple(elms, tpe, loc) =>
      val es = elms.map(visitExp(_, subst0))
      LiftedAst.Expression.Tuple(es, tpe, loc)

    case Expression.RecordEmpty(tpe, loc) => LiftedAst.Expression.RecordEmpty(tpe, loc)

    case Expression.RecordSelect(exp, field, tpe, loc) =>
      val e = visitExp(exp, subst0)
      LiftedAst.Expression.RecordSelect(e, field, tpe, loc)

    case Expression.RecordExtend(field, value, rest, tpe, loc) =>
      val v = visitExp(value, subst0)
      val r = visitExp(rest, subst0)
      LiftedAst.Expression.RecordExtend(field, v, r, tpe, loc)

    case Expression.RecordRestrict(field, rest, tpe, loc) =>
      val r = visitExp(rest, subst0)
      LiftedAst.Expression.RecordRestrict(field, r, tpe, loc)

    case Expression.ArrayLit(elms, tpe, loc) =>
      val es = elms.map(visitExp(_, subst0))
      LiftedAst.Expression.ArrayLit(es, tpe, loc)

    case Expression.ArrayNew(elm, len, tpe, loc) =>
      val e = visitExp(elm, subst0)
      val l = visitExp(len, subst0)
      LiftedAst.Expression.ArrayNew(e, l, tpe, loc)

    case Expression.ArrayLoad(base, index, tpe, loc) =>
      val b = visitExp(base, subst0)
      val i = visitExp(index, subst0)
      LiftedAst.Expression.ArrayLoad(b, i, tpe, loc)

    case Expression.ArrayStore(base, index, elm, tpe, loc) =>
      val b = visitExp(base, subst0)
      val i = visitExp(index, subst0)
      val e = visitExp(elm, subst0)
      LiftedAst.Expression.ArrayStore(b, i, e, tpe, loc)

    case Expression.ArrayLength(base, tpe, loc) =>
      val b = visitExp(base, subst0)
      LiftedAst.Expression.ArrayLength(b, tpe, loc)

    case Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) =>
      val b = visitExp(base, subst0)
      val i1 = visitExp(beginIndex, subst0)
      val i2 = visitExp(endIndex, subst0)
      LiftedAst.Expression.ArraySlice(b, i1, i2, tpe, loc)

    case Expression.Ref(exp, tpe, loc) =>
      val e = visitExp(exp, subst0)
      LiftedAst.Expression.Ref(e, tpe, loc)

    case Expression.Deref(exp, tpe, loc) =>
      val e = visitExp(exp, subst0)
      LiftedAst.Expression.Deref(e, tpe, loc)

    case Expression.Assign(exp1, exp2, tpe, loc) =>
      val e1 = visitExp(exp1, subst0)
      val e2 = visitExp(exp2, subst0)
      LiftedAst.Expression.Assign(e1, e2, tpe, loc)

    case Expression.Cast(exp, tpe, loc) =>
      val e = visitExp(exp, subst0)
      LiftedAst.Expression.Cast(e, tpe, loc)

    case Expression.TryCatch(exp, rules, tpe, loc) =>
      val e = visitExp(exp, subst0)
      val rs = rules.map {
        case OccurrenceAst.CatchRule(sym, clazz, exp) =>
          val e = visitExp(exp, subst0)
          LiftedAst.CatchRule(sym, clazz, e)
      }
      LiftedAst.Expression.TryCatch(e, rs, tpe, loc)

    case Expression.InvokeConstructor(constructor, args, tpe, loc) =>
      val as = args.map(visitExp(_, subst0))
      LiftedAst.Expression.InvokeConstructor(constructor, as, tpe, loc)

    case Expression.InvokeMethod(method, exp, args, tpe, loc) =>
      val e = visitExp(exp, subst0)
      val as = args.map(visitExp(_, subst0))
      LiftedAst.Expression.InvokeMethod(method, e, as, tpe, loc)

    case Expression.InvokeStaticMethod(method, args, tpe, loc) =>
      val as = args.map(visitExp(_, subst0))
      LiftedAst.Expression.InvokeStaticMethod(method, as, tpe, loc)

    case Expression.GetField(field, exp, tpe, loc) =>
      val e = visitExp(exp, subst0)
      LiftedAst.Expression.GetField(field, e, tpe, loc)

    case Expression.PutField(field, exp1, exp2, tpe, loc) =>
      val e1 = visitExp(exp1, subst0)
      val e2 = visitExp(exp2, subst0)
      LiftedAst.Expression.PutField(field, e1, e2, tpe, loc)

    case Expression.GetStaticField(field, tpe, loc) => LiftedAst.Expression.GetStaticField(field, tpe, loc)

    case Expression.PutStaticField(field, exp, tpe, loc) =>
      val e = visitExp(exp, subst0)
      LiftedAst.Expression.PutStaticField(field, e, tpe, loc)

    case Expression.NewChannel(exp, tpe, loc) =>
      val e = visitExp(exp, subst0)
      LiftedAst.Expression.NewChannel(e, tpe, loc)

    case Expression.GetChannel(exp, tpe, loc) =>
      val e = visitExp(exp, subst0)
      LiftedAst.Expression.GetChannel(e, tpe, loc)

    case Expression.PutChannel(exp1, exp2, tpe, loc) =>
      val e1 = visitExp(exp1, subst0)
      val e2 = visitExp(exp2, subst0)
      LiftedAst.Expression.PutChannel(e1, e2, tpe, loc)

    case Expression.SelectChannel(rules, default, tpe, loc) =>
      val rs = rules.map {
        case OccurrenceAst.SelectChannelRule(sym, chan, exp) =>
          val c = visitExp(chan, subst0)
          val e = visitExp(exp, subst0)
          LiftedAst.SelectChannelRule(sym, c, e)
      }
      val d = default.map(visitExp(_, subst0))
      LiftedAst.Expression.SelectChannel(rs, d, tpe, loc)

    case Expression.Spawn(exp, tpe, loc) =>
      val e = visitExp(exp, subst0)
      LiftedAst.Expression.Spawn(e, tpe, loc)

    case Expression.Lazy(exp, tpe, loc) =>
      val e = visitExp(exp, subst0)
      LiftedAst.Expression.Lazy(e, tpe, loc)

    case Expression.Force(exp, tpe, loc) =>
      val e = visitExp(exp, subst0)
      LiftedAst.Expression.Force(e, tpe, loc)

    case Expression.HoleError(sym, tpe, loc) => LiftedAst.Expression.HoleError(sym, tpe, loc)

    case Expression.MatchError(tpe, loc) =>  LiftedAst.Expression.MatchError(tpe, loc)
  }
}
