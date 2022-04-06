/*
 * Copyright 2017 Magnus Madsen, Anna Krogh, Patrick Lundvig, Christian Bonde
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
import ca.uwaterloo.flix.language.ast.OccurrenceAst.{Expression, Occur}
import ca.uwaterloo.flix.language.ast.{LiftedAst, OccurrenceAst, Purity, Symbol}
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.Validation

/**
 * The Reducer phase performs intra-procedural optimizations.
 *
 * Specifically,
 *
 * - Elimination of dead branches (e.g. if (true) e1 else e2).
 * - Copy propagation (e.g. let z = w; let y = z; let x = y; x -> w)
 */
object Reducer {

  /**
   * Returns an optimized version of the given AST `root`.
   */
  def run(root: OccurrenceAst.Root)(implicit flix: Flix): Validation[LiftedAst.Root, CompilationMessage] = flix.subphase("Reducer") {

    // Visit every definition in the AST
    val defs = root.defs.map {
      case (sym, defn) => sym -> visitDef(defn)
    }

    val enums = root.enums.map {
      case (sym, enum) =>
        val cases = enum.cases.map {
          case (tag, caze) =>
            tag -> LiftedAst.Case(caze.sym, tag, caze.tpeDeprecated, caze.loc)
        }
        sym -> LiftedAst.Enum(enum.ann, enum.mod, enum.sym, cases, enum.tpeDeprecated, enum.loc)
    }

    // Reassemble the ast root.
    val result = LiftedAst.Root(defs, enums, root.entryPoint, root.reachable, root.sources)

    result.toSuccess
  }

  /**
   * Performs expression reduction on the given definition `def0`.
   * Converts definition from OccurrenceAst to LiftedAst.
   */
  private def visitDef(def0: OccurrenceAst.Def)(implicit flix: Flix): LiftedAst.Def = {
    val convertedExp = visitExp(def0.exp, Map.empty)
    val fparams = def0.fparams.map {
      case OccurrenceAst.FormalParam(sym, mod, tpe, loc) => LiftedAst.FormalParam(sym, mod, tpe, loc)
    }
    LiftedAst.Def(def0.ann, def0.mod, def0.sym, fparams, convertedExp, def0.tpe, def0.loc)
  }

  /**
   * Performs intra-procedural optimization on the given expression `exp0` and substitution map `env0`.
   */
  private def visitExp(exp0: Expression, env0: Map[Symbol.VarSym, Symbol.VarSym]): LiftedAst.Expression = exp0 match {
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

    case Expression.Var(sym, tpe, loc) =>
      // Lookup to see if the variable should be replaced by a copy.
      env0.get(sym) match {
        case None => LiftedAst.Expression.Var(sym, tpe, loc)
        case Some(srcSym) => LiftedAst.Expression.Var(srcSym, tpe, loc)
      }

    case Expression.Closure(sym, freeVars, tpe, loc) =>
      val fvs = freeVars.map {
        case OccurrenceAst.FreeVar(s, varType) => LiftedAst.FreeVar(env0.getOrElse(s, s), varType)
      }
      LiftedAst.Expression.Closure(sym, fvs, tpe, loc)

    case Expression.ApplyClo(exp, args, tpe, purity, loc) =>
      val e = visitExp(exp, env0)
      val as = args.map(visitExp(_, env0))
      LiftedAst.Expression.ApplyClo(e, as, tpe, purity, loc)

    case Expression.ApplyDef(sym, args, tpe, purity, loc) =>
      val as = args.map(visitExp(_, env0))
      LiftedAst.Expression.ApplyDef(sym, as, tpe, purity, loc)

    case Expression.ApplyCloTail(exp, args, tpe, purity, loc) =>
      val e = visitExp(exp, env0)
      val as = args.map(visitExp(_, env0))
      LiftedAst.Expression.ApplyCloTail(e, as, tpe, purity, loc)

    case Expression.ApplyDefTail(sym, args, tpe, purity, loc) =>
      val as = args.map(visitExp(_, env0))
      LiftedAst.Expression.ApplyDefTail(sym, as, tpe, purity, loc)

    case Expression.ApplySelfTail(sym, formals, actuals, tpe, purity, loc) =>
      val as = actuals.map(visitExp(_, env0))
      val fs = formals.map {
        case OccurrenceAst.FormalParam(sym, mod, tpe, loc) => LiftedAst.FormalParam(sym, mod, tpe, loc)
      }
      LiftedAst.Expression.ApplySelfTail(sym, fs, as, tpe, purity, loc)

    case Expression.Unary(sop, op, exp, tpe, purity, loc) =>
      val e = visitExp(exp, env0)
      LiftedAst.Expression.Unary(sop, op, e, tpe, purity, loc)

    case Expression.Binary(sop, op, exp1, exp2, tpe, purity, loc) =>
      val e1 = visitExp(exp1, env0)
      val e2 = visitExp(exp2, env0)
      LiftedAst.Expression.Binary(sop, op, e1, e2, tpe, purity, loc)

    case Expression.IfThenElse(exp1, exp2, exp3, tpe, purity, loc) =>
      // Eliminate dead branches, if possible.
      val e1 = visitExp(exp1, env0)
      val e2 = visitExp(exp2, env0)
      val e3 = visitExp(exp3, env0)
      e1 match {
        case LiftedAst.Expression.True(_) => e2
        case LiftedAst.Expression.False(_) => e3
        case _ => LiftedAst.Expression.IfThenElse(e1, e2, e3, tpe, purity, loc)
      }

    case Expression.Branch(exp, branches, tpe, purity, loc) =>
      val e = visitExp(exp, env0)
      val bs = branches.map {
        case (sym, br) => sym -> visitExp(br, env0)
      }
      LiftedAst.Expression.Branch(e, bs, tpe, purity, loc)

    case Expression.JumpTo(sym, tpe, purity, loc) =>
      LiftedAst.Expression.JumpTo(sym, tpe, purity, loc)

    case Expression.Let(sym, exp1, exp2, occur, tpe, purity, loc) =>
      // If `e1` is unused (dead) and has no side effects (pure), visit `e2` and remove `e1`
      if (occur == Occur.Dead && exp1.purity == Purity.Pure) {
        visitExp(exp2, env0)
      } else {
        // Visit the value expression.
        val e1 = visitExp(exp1, env0)

        // Check for copy propagation: let x = y; e ~~> e[x -> y]
        e1 match {
          case LiftedAst.Expression.Var(srcSym, _, _) =>
            // The srcSym might itself originate from some other symbol.
            val originalSym = env0.getOrElse(srcSym, srcSym)
            visitExp(exp2, env0 + (sym -> originalSym))
          case _ =>
            val e2 = visitExp(exp2, env0)
            LiftedAst.Expression.Let(sym, e1, e2, tpe, purity, loc)
        }
      }

    case Expression.LetRec(varSym, index, defSym, exp1, exp2, tpe, purity, loc) =>
      val e1 = visitExp(exp1, env0)
      val e2 = visitExp(exp2, env0)
      LiftedAst.Expression.LetRec(varSym, index, defSym, e1, e2, tpe, purity, loc)

    case Expression.Is(sym, tag, exp, purity, loc) =>
      val e = visitExp(exp, env0)
      LiftedAst.Expression.Is(sym, tag, e, purity, loc)

    case Expression.Tag(sym, tag, exp, tpe, purity, loc) =>
      val e = visitExp(exp, env0)
      LiftedAst.Expression.Tag(sym, tag, e, tpe, purity, loc)

    case Expression.Untag(sym, tag, exp, tpe, purity, loc) =>
      val e = visitExp(exp, env0)
      LiftedAst.Expression.Untag(sym, tag, e, tpe, purity, loc)

    case Expression.Index(base, offset, tpe, purity, loc) =>
      val b = visitExp(base, env0)
      LiftedAst.Expression.Index(b, offset, tpe, purity, loc)

    case Expression.Tuple(elms, tpe, purity, loc) =>
      val es = elms.map(visitExp(_, env0))
      LiftedAst.Expression.Tuple(es, tpe, purity, loc)

    case Expression.RecordEmpty(tpe, loc) =>
      LiftedAst.Expression.RecordEmpty(tpe, loc)

    case Expression.RecordSelect(exp, field, tpe, purity, loc) =>
      val e = visitExp(exp, env0)
      LiftedAst.Expression.RecordSelect(e, field, tpe, purity, loc)

    case Expression.RecordExtend(field, value, rest, tpe, purity, loc) =>
      val v = visitExp(value, env0)
      val r = visitExp(rest, env0)
      LiftedAst.Expression.RecordExtend(field, v, r, tpe, purity, loc)

    case Expression.RecordRestrict(field, rest, tpe, purity, loc) =>
      val r = visitExp(rest, env0)
      LiftedAst.Expression.RecordRestrict(field, r, tpe, purity, loc)

    case Expression.ArrayLit(elms, tpe, loc) =>
      val es = elms.map(visitExp(_, env0))
      LiftedAst.Expression.ArrayLit(es, tpe, loc)

    case Expression.ArrayNew(elm, len, tpe, loc) =>
      val e = visitExp(elm, env0)
      val ln = visitExp(len, env0)
      LiftedAst.Expression.ArrayNew(e, ln, tpe, loc)

    case Expression.ArrayLoad(base, index, tpe, loc) =>
      val b = visitExp(base, env0)
      val i = visitExp(index, env0)
      LiftedAst.Expression.ArrayLoad(b, i, tpe, loc)

    case Expression.ArrayStore(base, index, elm, tpe, loc) =>
      val b = visitExp(base, env0)
      val i = visitExp(index, env0)
      val e = visitExp(elm, env0)
      LiftedAst.Expression.ArrayStore(b, i, e, tpe, loc)

    case Expression.ArrayLength(base, tpe, _, loc) =>
      val b = visitExp(base, env0)
      val purity = b.purity
      LiftedAst.Expression.ArrayLength(b, tpe, purity, loc)

    case Expression.ArraySlice(base, startIndex, endIndex, tpe, loc) =>
      val b = visitExp(base, env0)
      val i1 = visitExp(startIndex, env0)
      val i2 = visitExp(endIndex, env0)
      LiftedAst.Expression.ArraySlice(b, i1, i2, tpe, loc)

    case Expression.Ref(exp, tpe, loc) =>
      val e = visitExp(exp, env0)
      LiftedAst.Expression.Ref(e, tpe, loc)

    case Expression.Deref(exp, tpe, loc) =>
      val e = visitExp(exp, env0)
      LiftedAst.Expression.Deref(e, tpe, loc)

    case Expression.Assign(exp1, exp2, tpe, loc) =>
      val e1 = visitExp(exp1, env0)
      val e2 = visitExp(exp2, env0)
      LiftedAst.Expression.Assign(e1, e2, tpe, loc)

    case Expression.Cast(exp, tpe, purity, loc) =>
      val e = visitExp(exp, env0)
      LiftedAst.Expression.Cast(e, tpe, purity, loc)

    case Expression.TryCatch(exp, rules, tpe, purity, loc) =>
      val e = visitExp(exp, env0)
      val rs = rules.map {
        case OccurrenceAst.CatchRule(sym, clazz, body) =>
          val b = visitExp(body, env0)
          LiftedAst.CatchRule(sym, clazz, b)
      }
      LiftedAst.Expression.TryCatch(e, rs, tpe, purity, loc)

    case Expression.InvokeConstructor(constructor, args, tpe, purity, loc) =>
      val as = args.map(visitExp(_, env0))
      LiftedAst.Expression.InvokeConstructor(constructor, as, tpe, purity, loc)

    case Expression.InvokeMethod(method, exp, args, tpe, purity, loc) =>
      val e = visitExp(exp, env0)
      val as = args.map(visitExp(_, env0))
      LiftedAst.Expression.InvokeMethod(method, e, as, tpe, purity, loc)

    case Expression.InvokeStaticMethod(method, args, tpe, purity, loc) =>
      val as = args.map(visitExp(_, env0))
      LiftedAst.Expression.InvokeStaticMethod(method, as, tpe, purity, loc)

    case Expression.GetField(field, exp, tpe, purity, loc) =>
      val e = visitExp(exp, env0)
      LiftedAst.Expression.GetField(field, e, tpe, purity, loc)

    case Expression.PutField(field, exp1, exp2, tpe, purity, loc) =>
      val e1 = visitExp(exp1, env0)
      val e2 = visitExp(exp2, env0)
      LiftedAst.Expression.PutField(field, e1, e2, tpe, purity, loc)

    case Expression.GetStaticField(field, tpe, purity, loc) =>
      LiftedAst.Expression.GetStaticField(field, tpe, purity, loc)

    case Expression.PutStaticField(field, exp, tpe, purity, loc) =>
      val e = visitExp(exp, env0)
      LiftedAst.Expression.PutStaticField(field, e, tpe, purity, loc)

    case Expression.NewChannel(exp, tpe, loc) =>
      val e = visitExp(exp, env0)
      LiftedAst.Expression.NewChannel(e, tpe, loc)

    case Expression.GetChannel(exp, tpe, loc) =>
      val e = visitExp(exp, env0)
      LiftedAst.Expression.GetChannel(e, tpe, loc)

    case Expression.PutChannel(exp1, exp2, tpe, loc) =>
      val e1 = visitExp(exp1, env0)
      val e2 = visitExp(exp2, env0)
      LiftedAst.Expression.PutChannel(e1, e2, tpe, loc)

    case Expression.SelectChannel(rules, default, tpe, loc) =>
      val rs = rules.map {
        case OccurrenceAst.SelectChannelRule(sym, chan, exp) =>
          val c = visitExp(chan, env0)
          val e = visitExp(exp, env0)
          LiftedAst.SelectChannelRule(sym, c, e)
      }

      val d = default.map(visitExp(_, env0))

      LiftedAst.Expression.SelectChannel(rs, d, tpe, loc)

    case Expression.Spawn(exp, tpe, loc) =>
      val e = visitExp(exp, env0)
      LiftedAst.Expression.Spawn(e, tpe, loc)

    case Expression.Lazy(exp, tpe, loc) =>
      val e = visitExp(exp, env0)
      LiftedAst.Expression.Lazy(e, tpe, loc)

    case Expression.Force(exp, tpe, loc) =>
      val e = visitExp(exp, env0)
      LiftedAst.Expression.Force(e, tpe, loc)

    case Expression.HoleError(sym, tpe, loc) => LiftedAst.Expression.HoleError(sym, tpe, loc)

    case Expression.MatchError(tpe, loc) => LiftedAst.Expression.MatchError(tpe, loc)
  }
}
