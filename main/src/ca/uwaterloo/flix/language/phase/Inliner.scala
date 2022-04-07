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
import ca.uwaterloo.flix.language.ast.OccurrenceAst.Occur.DontInline
import ca.uwaterloo.flix.language.ast.OccurrenceAst._
import ca.uwaterloo.flix.language.ast.{OccurrenceAst, Purity, Symbol}
import ca.uwaterloo.flix.language.phase.Optimizer.isTrivialExp
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
      case (sym, defn) => sym -> visitDef(defn)(root, flix)
    }

    // Reassemble the ast root.
    val result = root.copy(defs = defs)

    result.toSuccess
  }

  private def visitDef(def0: Def)(implicit root: Root, flix: Flix): Def = {
    def0.copy(exp = visitExp(def0.exp, Map.empty))
  }

  private def visitExp(exp0: OccurrenceAst.Expression, subst0: Map[Symbol.VarSym, Expression])(implicit root: Root, flix: Flix): Expression = exp0 match {
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

    case Expression.ApplyClo(exp, args, tpe, purity, loc) =>
      val e = visitExp(exp, subst0)
      val as = args.map(visitExp(_, subst0))
      Expression.ApplyClo(e, as, tpe, purity, loc)

    case Expression.ApplyDef(sym, args, tpe, purity, loc) =>
      val as = args.map(visitExp(_, subst0))
      val def1 = root.defs.apply(sym)
      // If `def1` is a single non-self call and its arguments are trivial, then inline the single non-self call, `e1`.
      if (def1.context.isNonSelfCall && purity == Purity.Pure) {
        val e1 = convertTailCall(def1.exp)
        bindFormals(e1, def1.fparams.map(_.sym), as, Map.empty)
      } else {
        Expression.ApplyDef(sym, as, tpe, purity, loc)
      }

    case Expression.ApplyCloTail(exp, args, tpe, purity, loc) =>
      val e = visitExp(exp, subst0)
      val as = args.map(visitExp(_, subst0))
      Expression.ApplyCloTail(e, as, tpe, purity, loc)

    case Expression.ApplyDefTail(sym, args, tpe, purity, loc) =>
      val as = args.map(visitExp(_, subst0))
      val def1 = root.defs.apply(sym)
      // If `def1` is a single non-self call and its arguments are trivial, then inline the single non-self call, `e1`.
      if (def1.context.isNonSelfCall && purity == Purity.Pure) {
        bindFormals(def1.exp, def1.fparams.map(_.sym), as, Map.empty)
      } else {
        Expression.ApplyDefTail(sym, as, tpe, purity, loc)
      }

    case Expression.ApplySelfTail(sym, formals, actuals, tpe, purity, loc) =>
      val as = actuals.map(visitExp(_, subst0))
      val fs = formals map {
        case OccurrenceAst.FormalParam(sym, mod, tpe, loc) => FormalParam(sym, mod, tpe, loc)
      }
      Expression.ApplySelfTail(sym, fs, as, tpe, purity, loc)

    case Expression.Unary(sop, op, exp, tpe, purity, loc) =>
      val e = visitExp(exp, subst0)
      Expression.Unary(sop, op, e, tpe, purity, loc)

    case Expression.Binary(sop, op, exp1, exp2, tpe, purity, loc) =>
      val e1 = visitExp(exp1, subst0)
      val e2 = visitExp(exp2, subst0)
      Expression.Binary(sop, op, e1, e2, tpe, purity, loc)

    case Expression.IfThenElse(exp1, exp2, exp3, tpe, purity, loc) =>
      val e1 = visitExp(exp1, subst0)
      val e2 = visitExp(exp2, subst0)
      val e3 = visitExp(exp3, subst0)
      Expression.IfThenElse(e1, e2, e3, tpe, purity, loc)

    case Expression.Branch(exp, branches, tpe, purity, loc) =>
      val e = visitExp(exp, subst0)
      val bs = branches.map {
        case (sym, br) => sym -> visitExp(br, subst0)
      }
      Expression.Branch(e, bs, tpe, purity, loc)

    case Expression.JumpTo(_, _, _, _) => exp0

    case Expression.Let(sym, exp1, exp2, occur, tpe, purity, loc) =>
      /// Case 1:
      /// If `exp1` occurs once and it is pure, then it is safe to inline without increasing neither the code size
      /// nor the execution time.
      val wantToPreInline = (occur, exp1.purity) match {
        case (Occur.Once, Purity.Pure) => true
        case _ => false
      }
      if (wantToPreInline) {
        val subst1 = subst0 + (sym -> exp1)
        visitExp(exp2, subst1)
      } else {
        /// Case 2:
        /// If `e1` is trivial and pure, then it is safe to inline without increasing execution time.
        val e1 = visitExp(exp1, subst0)
        val wantToPostInline = (occur, exp1.purity, isTrivialExp(e1)) match {
          case (Occur.DontInline, _, _) => false
          case (_, Purity.Pure, true) => true
          case _ => false
        }
        /// If `e1` is to be inlined:
        /// Add map `sym` to `e1` and return `e2` without constructing the let expression.
        if (wantToPostInline) {
          val subst1 = subst0 + (sym -> e1)
          visitExp(exp2, subst1)
        } else {
          /// Case 3:
          /// If none of the previous cases pass, `sym` is not inlined. Return a let expression with the visited expressions
          val e3 = visitExp(exp2, subst0)
          Expression.Let(sym, e1, e3, occur, tpe, purity, loc)
        }
      }

    case Expression.LetRec(varSym, index, defSym, exp1, exp2, tpe, purity, loc) =>
      val e1 = visitExp(exp1, subst0)
      val e2 = visitExp(exp2, subst0)
      Expression.LetRec(varSym, index, defSym, e1, e2, tpe, purity, loc)

    case Expression.Is(sym, tag, exp, purity, loc) =>
      val e = visitExp(exp, subst0)
      Expression.Is(sym, tag, e, purity, loc)

    case Expression.Tag(sym, tag, exp, tpe, purity, loc) =>
      val e = visitExp(exp, subst0)
      Expression.Tag(sym, tag, e, tpe, purity, loc)

    case Expression.Untag(sym, tag, exp, tpe, purity, loc) =>
      val e = visitExp(exp, subst0)
      Expression.Untag(sym, tag, e, tpe, purity, loc)

    case Expression.Index(base, offset, tpe, purity, loc) =>
      val b = visitExp(base, subst0)
      Expression.Index(b, offset, tpe, purity, loc)

    case Expression.Tuple(elms, tpe, purity, loc) =>
      val es = elms.map(visitExp(_, subst0))
      Expression.Tuple(es, tpe, purity, loc)

    case Expression.RecordEmpty(_, _) => exp0

    case Expression.RecordSelect(exp, field, tpe, purity, loc) =>
      val e = visitExp(exp, subst0)
      Expression.RecordSelect(e, field, tpe, purity, loc)

    case Expression.RecordExtend(field, value, rest, tpe, purity, loc) =>
      val v = visitExp(value, subst0)
      val r = visitExp(rest, subst0)
      Expression.RecordExtend(field, v, r, tpe, purity, loc)

    case Expression.RecordRestrict(field, rest, tpe, purity, loc) =>
      val r = visitExp(rest, subst0)
      Expression.RecordRestrict(field, r, tpe, purity, loc)

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

    case Expression.ArrayLength(base, tpe, _, loc) =>
      val b = visitExp(base, subst0)
      val purity = b.purity
      Expression.ArrayLength(b, tpe, purity, loc)

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

    case Expression.Cast(exp, tpe, purity, loc) =>
      val e = visitExp(exp, subst0)
      Expression.Cast(e, tpe, purity, loc)

    case Expression.TryCatch(exp, rules, tpe, purity, loc) =>
      val e = visitExp(exp, subst0)
      val rs = rules.map {
        case OccurrenceAst.CatchRule(sym, clazz, exp) =>
          val e = visitExp(exp, subst0)
          CatchRule(sym, clazz, e)
      }
      Expression.TryCatch(e, rs, tpe, purity, loc)

    case Expression.InvokeConstructor(constructor, args, tpe, purity, loc) =>
      val as = args.map(visitExp(_, subst0))
      Expression.InvokeConstructor(constructor, as, tpe, purity, loc)

    case Expression.InvokeMethod(method, exp, args, tpe, purity, loc) =>
      val e = visitExp(exp, subst0)
      val as = args.map(visitExp(_, subst0))
      Expression.InvokeMethod(method, e, as, tpe, purity, loc)

    case Expression.InvokeStaticMethod(method, args, tpe, purity, loc) =>
      val as = args.map(visitExp(_, subst0))
      Expression.InvokeStaticMethod(method, as, tpe, purity, loc)

    case Expression.GetField(field, exp, tpe, purity, loc) =>
      val e = visitExp(exp, subst0)
      Expression.GetField(field, e, tpe, purity, loc)

    case Expression.PutField(field, exp1, exp2, tpe, purity, loc) =>
      val e1 = visitExp(exp1, subst0)
      val e2 = visitExp(exp2, subst0)
      Expression.PutField(field, e1, e2, tpe, purity, loc)

    case Expression.GetStaticField(_, _, _, _) => exp0

    case Expression.PutStaticField(field, exp, tpe, purity, loc) =>
      val e = visitExp(exp, subst0)
      Expression.PutStaticField(field, e, tpe, purity, loc)

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

  /**
   * Recursively bind each argument in `args` to a let-expression with a fresh symbol
   * Add corresponding symbol from `symbols` to substitution map `env0`, mapping old symbols to fresh symbols.
   * Substitute variables in `exp0` via the filled substitution map `env0`
   */
  private def bindFormals(exp0: Expression, symbols: List[Symbol.VarSym], args: List[Expression], env0: Map[Symbol.VarSym, Symbol.VarSym])(implicit root: Root, flix: Flix): Expression = {
    (symbols, args) match {
      case ((sym :: nextSymbols), (e1 :: nextExpressions)) =>
        val freshVar = Symbol.freshVarSym(sym)
        val env1 = env0 + (sym -> freshVar)
        val nextLet = bindFormals(exp0, nextSymbols, nextExpressions, env1)
        val purity = if (isTrivialExp(e1)) Purity.Pure else Purity.Impure
        Expression.Let(freshVar, e1, nextLet, DontInline, exp0.tpe, purity, exp0.loc)
      case _ => substituteExp(exp0, env0)
    }
  }

  /**
   * Convert a given tailCall expression `exp0` to a non tail call
   */
  //TODO expand `convertTailCall` when more functions are being inlined
  private def convertTailCall(exp0: Expression): Expression = exp0 match {
    case Expression.ApplyCloTail(exp, args, tpe, purity, loc) => Expression.ApplyClo(exp, args, tpe, purity, loc)
    case Expression.ApplyDefTail(sym, args, tpe, purity, loc) => Expression.ApplyDef(sym, args, tpe, purity, loc)
    case _ => exp0
  }

  /**
   * Substitute variables in `exp0` for new fresh variables in `env0`
   */
  private def substituteExp(exp0: OccurrenceAst.Expression, env0: Map[Symbol.VarSym, Symbol.VarSym])(implicit root: Root, flix: Flix): Expression = exp0 match {
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

    case Expression.Var(sym, tpe, loc) => Expression.Var(env0.getOrElse(sym, sym), tpe, loc)

    case Expression.Closure(sym, freeVars, tpe, loc) =>
      val fvs = freeVars.map {
        case FreeVar(sym, tpe) => FreeVar(env0.getOrElse(sym, sym), tpe)
      }
      Expression.Closure(sym, fvs, tpe, loc)

    case Expression.ApplyClo(exp, args, tpe, purity, loc) =>
      val e = substituteExp(exp, env0)
      val as = args.map(substituteExp(_, env0))
      Expression.ApplyClo(e, as, tpe, purity, loc)

    case Expression.ApplyDef(sym, args, tpe, purity, loc) =>
      val as = args.map(substituteExp(_, env0))
      Expression.ApplyDef(sym, as, tpe, purity, loc)

    case Expression.ApplyCloTail(exp, args, tpe, purity, loc) =>
      val e = substituteExp(exp, env0)
      val as = args.map(substituteExp(_, env0))
      Expression.ApplyCloTail(e, as, tpe, purity, loc)

    case Expression.ApplyDefTail(sym, args, tpe, purity, loc) =>
      val as = args.map(substituteExp(_, env0))
      Expression.ApplyDefTail(sym, as, tpe, purity, loc)

    case Expression.ApplySelfTail(sym, formals, actuals, tpe, purity, loc) =>
      val as = actuals.map(substituteExp(_, env0))
      Expression.ApplySelfTail(sym, formals, as, tpe, purity, loc)

    case Expression.Unary(sop, op, exp, tpe, purity, loc) =>
      val e = substituteExp(exp, env0)
      Expression.Unary(sop, op, e, tpe, purity, loc)

    case Expression.Binary(sop, op, exp1, exp2, tpe, purity, loc) =>
      val e1 = substituteExp(exp1, env0)
      val e2 = substituteExp(exp2, env0)
      Expression.Binary(sop, op, e1, e2, tpe, purity, loc)

    case Expression.IfThenElse(exp1, exp2, exp3, tpe, purity, loc) =>
      val e1 = substituteExp(exp1, env0)
      val e2 = substituteExp(exp2, env0)
      val e3 = substituteExp(exp3, env0)
      Expression.IfThenElse(e1, e2, e3, tpe, purity, loc)

    case Expression.Branch(exp, branches, tpe, purity, loc) =>
      val e = substituteExp(exp, env0)
      val bs = branches.map {
        case (sym, br) => sym -> substituteExp(br, env0)
      }
      Expression.Branch(e, bs, tpe, purity, loc)

    case Expression.JumpTo(_, _, _, _) => exp0

    case Expression.Let(sym, exp1, exp2, occur, tpe, purity, loc) => {
      val freshVar = Symbol.freshVarSym(sym)
      val env1 = env0 + (sym -> freshVar)
      val e1 = substituteExp(exp1, env1)
      val e2 = substituteExp(exp2, env1)
      Expression.Let(freshVar, e1, e2, occur, tpe, purity, loc)
    }

    case Expression.LetRec(varSym, index, defSym, exp1, exp2, tpe, purity, loc) => {
      val freshVar = Symbol.freshVarSym(varSym)
      val env1 = env0 + (varSym -> freshVar)
      val e1 = substituteExp(exp1, env1)
      val e2 = substituteExp(exp2, env1)
      Expression.LetRec(freshVar, index, defSym, e1, e2, tpe, purity, loc)
    }
    case Expression.Is(sym, tag, exp, purity, loc) =>
      val e = substituteExp(exp, env0)
      Expression.Is(sym, tag, e, purity, loc)

    case Expression.Tag(sym, tag, exp, tpe, purity, loc) =>
      val e = substituteExp(exp, env0)
      Expression.Tag(sym, tag, e, tpe, purity, loc)

    case Expression.Untag(sym, tag, exp, tpe, purity, loc) =>
      val e = substituteExp(exp, env0)
      Expression.Untag(sym, tag, e, tpe, purity, loc)

    case Expression.Index(base, offset, tpe, purity, loc) =>
      val b = substituteExp(base, env0)
      Expression.Index(b, offset, tpe, purity, loc)

    case Expression.Tuple(elms, tpe, purity, loc) =>
      val es = elms.map(substituteExp(_, env0))
      Expression.Tuple(es, tpe, purity, loc)

    case Expression.RecordEmpty(_, _) => exp0

    case Expression.RecordSelect(exp, field, tpe, purity, loc) =>
      val e = substituteExp(exp, env0)
      Expression.RecordSelect(e, field, tpe, purity, loc)

    case Expression.RecordExtend(field, value, rest, tpe, purity, loc) =>
      val v = substituteExp(value, env0)
      val r = substituteExp(rest, env0)
      Expression.RecordExtend(field, v, r, tpe, purity, loc)

    case Expression.RecordRestrict(field, rest, tpe, purity, loc) =>
      val r = substituteExp(rest, env0)
      Expression.RecordRestrict(field, r, tpe, purity, loc)

    case Expression.ArrayLit(elms, tpe, loc) =>
      val es = elms.map(substituteExp(_, env0))
      Expression.ArrayLit(es, tpe, loc)

    case Expression.ArrayNew(elm, len, tpe, loc) =>
      val e = substituteExp(elm, env0)
      val l = substituteExp(len, env0)
      Expression.ArrayNew(e, l, tpe, loc)

    case Expression.ArrayLoad(base, index, tpe, loc) =>
      val b = substituteExp(base, env0)
      val i = substituteExp(index, env0)
      Expression.ArrayLoad(b, i, tpe, loc)

    case Expression.ArrayStore(base, index, elm, tpe, loc) =>
      val b = substituteExp(base, env0)
      val i = substituteExp(index, env0)
      val e = substituteExp(elm, env0)
      Expression.ArrayStore(b, i, e, tpe, loc)

    case Expression.ArrayLength(base, tpe, purity, loc) =>
      val b = substituteExp(base, env0)
      Expression.ArrayLength(b, tpe, purity, loc)

    case Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) =>
      val b = substituteExp(base, env0)
      val i1 = substituteExp(beginIndex, env0)
      val i2 = substituteExp(endIndex, env0)
      Expression.ArraySlice(b, i1, i2, tpe, loc)

    case Expression.Ref(exp, tpe, loc) =>
      val e = substituteExp(exp, env0)
      Expression.Ref(e, tpe, loc)

    case Expression.Deref(exp, tpe, loc) =>
      val e = substituteExp(exp, env0)
      Expression.Deref(e, tpe, loc)

    case Expression.Assign(exp1, exp2, tpe, loc) =>
      val e1 = substituteExp(exp1, env0)
      val e2 = substituteExp(exp2, env0)
      Expression.Assign(e1, e2, tpe, loc)

    case Expression.Cast(exp, tpe, purity, loc) =>
      val e = substituteExp(exp, env0)
      Expression.Cast(e, tpe, purity, loc)

    case Expression.TryCatch(exp, rules, tpe, purity, loc) =>
      val e = substituteExp(exp, env0)
      val rs = rules.map {
        case OccurrenceAst.CatchRule(sym, clazz, exp) =>
          val freshVar = Symbol.freshVarSym(sym)
          val env1 = env0 + (sym -> freshVar)
          val e = substituteExp(exp, env1)
          CatchRule(freshVar, clazz, e)
      }
      Expression.TryCatch(e, rs, tpe, purity, loc)

    case Expression.InvokeConstructor(constructor, args, tpe, purity, loc) =>
      val as = args.map(substituteExp(_, env0))
      Expression.InvokeConstructor(constructor, as, tpe, purity, loc)

    case Expression.InvokeMethod(method, exp, args, tpe, purity, loc) =>
      val e = substituteExp(exp, env0)
      val as = args.map(substituteExp(_, env0))
      Expression.InvokeMethod(method, e, as, tpe, purity, loc)

    case Expression.InvokeStaticMethod(method, args, tpe, purity, loc) =>
      val as = args.map(substituteExp(_, env0))
      Expression.InvokeStaticMethod(method, as, tpe, purity, loc)

    case Expression.GetField(field, exp, tpe, purity, loc) =>
      val e = substituteExp(exp, env0)
      Expression.GetField(field, e, tpe, purity, loc)

    case Expression.PutField(field, exp1, exp2, tpe, purity, loc) =>
      val e1 = substituteExp(exp1, env0)
      val e2 = substituteExp(exp2, env0)
      Expression.PutField(field, e1, e2, tpe, purity, loc)

    case Expression.GetStaticField(_, _, _, _) => exp0

    case Expression.PutStaticField(field, exp, tpe, purity, loc) =>
      val e = substituteExp(exp, env0)
      Expression.PutStaticField(field, e, tpe, purity, loc)

    case Expression.NewChannel(exp, tpe, loc) =>
      val e = substituteExp(exp, env0)
      Expression.NewChannel(e, tpe, loc)

    case Expression.GetChannel(exp, tpe, loc) =>
      val e = substituteExp(exp, env0)
      Expression.GetChannel(e, tpe, loc)

    case Expression.PutChannel(exp1, exp2, tpe, loc) =>
      val e1 = substituteExp(exp1, env0)
      val e2 = substituteExp(exp2, env0)
      Expression.PutChannel(e1, e2, tpe, loc)

    case Expression.SelectChannel(rules, default, tpe, loc) =>
      val rs = rules.map {
        case OccurrenceAst.SelectChannelRule(sym, chan, exp) =>
          val freshVar = Symbol.freshVarSym(sym)
          val env1 = env0 + (sym -> freshVar)
          val c = substituteExp(chan, env1)
          val e = substituteExp(exp, env1)
          SelectChannelRule(freshVar, c, e)
      }
      val d = default.map(substituteExp(_, env0))
      Expression.SelectChannel(rs, d, tpe, loc)

    case Expression.Spawn(exp, tpe, loc) =>
      val e = substituteExp(exp, env0)
      Expression.Spawn(e, tpe, loc)

    case Expression.Lazy(exp, tpe, loc) =>
      val e = substituteExp(exp, env0)
      Expression.Lazy(e, tpe, loc)

    case Expression.Force(exp, tpe, loc) =>
      val e = substituteExp(exp, env0)
      Expression.Force(e, tpe, loc)

    case Expression.HoleError(_, _, _) => exp0

    case Expression.MatchError(_, _) => exp0
  }
}
