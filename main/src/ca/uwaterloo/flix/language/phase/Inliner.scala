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
import ca.uwaterloo.flix.language.ast.OccurrenceAst.Occur._
import ca.uwaterloo.flix.language.ast.{OccurrenceAst, LiftedAst, Purity, Symbol}
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

/**
 * The inliner replaces closures and functions by their code to improve performance.
 */
object Inliner {

  sealed trait Expression

  object Expression {
    case class LiftedExp(exp: LiftedAst.Expression) extends Expression
    case class OccurrenceExp(exp: OccurrenceAst.Expression) extends Expression
  }

  /**
   * Performs inlining on the given AST `root`.
   */
  def run(root: OccurrenceAst.Root)(implicit flix: Flix): Validation[LiftedAst.Root, CompilationMessage] = flix.subphase("Inliner") {
    // Visit every definition in the program.
    val defs = root.defs.map {
      case (sym, defn) => sym -> visitDef(defn)
    }

    // Visit every enum in the program.
    val enums = root.enums.map {
      case (sym, enum) => sym -> visitEnum(enum)
    }

    // Reassemble the ast root.
    val result = LiftedAst.Root(defs, enums, root.entryPoint, root.reachable, root.sources)

    result.toSuccess
  }

  /**
   * Performs expression inlining on the given definition `def0`.
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
   * Converts enum from OccurrenceAst to LiftedAst
   */
  private def visitEnum(enum: OccurrenceAst.Enum): LiftedAst.Enum = {
    val cases = enum.cases.map {
      case (tag, caze) =>
        tag -> LiftedAst.Case(caze.sym, tag, caze.tpeDeprecated, caze.loc)
    }
    LiftedAst.Enum(enum.ann, enum.mod, enum.sym, cases, enum.tpeDeprecated, enum.loc)
  }

  /**
   * Performs inlining operations on the expression `exp0` of type OccurrenceAst.Expression.
   * Returns an expression of type Expression
   */
  private def visitExp(exp0: OccurrenceAst.Expression, subst0: Map[Symbol.VarSym, Expression]): LiftedAst.Expression = exp0 match {
    case OccurrenceAst.Expression.Unit(loc) => LiftedAst.Expression.Unit(loc)

    case OccurrenceAst.Expression.Null(tpe, loc) => LiftedAst.Expression.Null(tpe, loc)

    case OccurrenceAst.Expression.True(loc) => LiftedAst.Expression.True(loc)

    case OccurrenceAst.Expression.False(loc) => LiftedAst.Expression.False(loc)

    case OccurrenceAst.Expression.Char(lit, loc) => LiftedAst.Expression.Char(lit, loc)

    case OccurrenceAst.Expression.Float32(lit, loc) => LiftedAst.Expression.Float32(lit, loc)

    case OccurrenceAst.Expression.Float64(lit, loc) => LiftedAst.Expression.Float64(lit, loc)

    case OccurrenceAst.Expression.Int8(lit, loc) => LiftedAst.Expression.Int8(lit, loc)

    case OccurrenceAst.Expression.Int16(lit, loc) => LiftedAst.Expression.Int16(lit, loc)

    case OccurrenceAst.Expression.Int32(lit, loc) => LiftedAst.Expression.Int32(lit, loc)

    case OccurrenceAst.Expression.Int64(lit, loc) => LiftedAst.Expression.Int64(lit, loc)

    case OccurrenceAst.Expression.BigInt(lit, loc) => LiftedAst.Expression.BigInt(lit, loc)

    case OccurrenceAst.Expression.Str(lit, loc) => LiftedAst.Expression.Str(lit, loc)

    case OccurrenceAst.Expression.Var(sym, tpe, loc) =>
      subst0.get(sym) match {
        // Case 1:
        // The variable `sym` is not in the substitution map and will not be inlined.
        case None => LiftedAst.Expression.Var(sym, tpe, loc)
        // Case 2:
        // The variable `sym` is in the substitution map. Replace `sym` with `e1`.
        case Some(e1) =>
          e1 match {
            // If `e1` is a `LiftedExp` then `e1` has already been visited
            case Expression.LiftedExp(exp) => exp
            // If `e1` is a `OccurrenceExp` then `e1` has not been visited. Visit `e1`
            case Expression.OccurrenceExp(exp) => visitExp(exp, subst0)
          }
      }

    case OccurrenceAst.Expression.Closure(sym, freeVars, tpe, loc) =>
      val fvs = freeVars.map {
        case OccurrenceAst.FreeVar(s, varType) => LiftedAst.FreeVar(s, varType)
      }
      LiftedAst.Expression.Closure(sym, fvs, tpe, loc)

    case OccurrenceAst.Expression.ApplyClo(exp, args, tpe, loc) =>
      val e = visitExp(exp, subst0)
      val as = args.map(visitExp(_, subst0))
      LiftedAst.Expression.ApplyClo(e, as, tpe, loc)

    case OccurrenceAst.Expression.ApplyDef(sym, args, tpe, loc) =>
      val as = args.map(visitExp(_, subst0))
      LiftedAst.Expression.ApplyDef(sym, as, tpe, loc)

    case OccurrenceAst.Expression.ApplyCloTail(exp, args, tpe, loc) =>
      val e = visitExp(exp, subst0)
      val as = args.map(visitExp(_, subst0))
      LiftedAst.Expression.ApplyCloTail(e, as, tpe, loc)

    case OccurrenceAst.Expression.ApplyDefTail(sym, args, tpe, loc) =>
      val as = args.map(visitExp(_, subst0))
      LiftedAst.Expression.ApplyDefTail(sym, as, tpe, loc)

    case OccurrenceAst.Expression.ApplySelfTail(sym, formals, actuals, tpe, loc) =>
      val as = actuals.map(visitExp(_, subst0))
      val fs = formals.map(visitFormalParam)
      LiftedAst.Expression.ApplySelfTail(sym, fs, as, tpe, loc)

    case OccurrenceAst.Expression.Unary(sop, op, exp, tpe, loc) =>
      val e = visitExp(exp, subst0)
      LiftedAst.Expression.Unary(sop, op, e, tpe, loc)

    case OccurrenceAst.Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
      val e1 = visitExp(exp1, subst0)
      val e2 = visitExp(exp2, subst0)
      LiftedAst.Expression.Binary(sop, op, e1, e2, tpe, loc)

    case OccurrenceAst.Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
      val e1 = visitExp(exp1, subst0)
      val e2 = visitExp(exp2, subst0)
      val e3 = visitExp(exp3, subst0)
      e1 match {
        case LiftedAst.Expression.True(_) => e2
        case LiftedAst.Expression.False(_) => e3
        case _ => LiftedAst.Expression.IfThenElse(e1, e2, e3, tpe, loc)
      }

    case OccurrenceAst.Expression.Branch(exp, branches, tpe, loc) =>
      val e = visitExp(exp, subst0)
      val bs = branches.map {
        case (sym, br) => sym -> visitExp(br, subst0)
      }
      LiftedAst.Expression.Branch(e, bs, tpe, loc)

    case OccurrenceAst.Expression.JumpTo(sym, tpe, loc) => LiftedAst.Expression.JumpTo(sym, tpe, loc)

    case OccurrenceAst.Expression.Let(sym, exp1, exp2, occur, tpe, purity, loc) =>
      /// Case 1:
      /// If `sym` is never used (it is `Dead`)  and `exp1` is pure, so it has no side effects, then it is safe to remove `sym`
      /// Both code size and runtime are reduced
      if (isDeadAndPure(occur, purity)) {
        visitExp(exp2, subst0)
      } else {
        /// Case 2:
        /// If `exp1` occurs once and it is pure, then it is safe to inline.
        /// There is a small decrease in code size and runtime.
        val wantToPreInline = isUsedOnceAndPure(occur, purity)
        if (wantToPreInline) {
          val subst1 = subst0 + (sym -> Expression.OccurrenceExp(exp1))
          visitExp(exp2, subst1)
        } else {
          val e1 = visitExp(exp1, subst0)
          /// Case 3:
          /// If `e1` is trivial and pure, then it is safe to inline.
          // Code size and runtime are not impacted, because only trivial expressions are inlined
          val wantToPostInline = isTrivialAndPure(e1, purity) && occur != DontInline
          if (wantToPostInline) {
            /// If `e1` is to be inlined:
            /// Add map `sym` to `e1` and return `e2` without constructing the let expression.
            val subst1 = subst0 + (sym -> Expression.LiftedExp(e1))
            visitExp(exp2, subst1)
          } else {
            /// Case 4:
            /// If none of the previous cases pass, `sym` is not inlined. Return a let expression with the visited expressions
            /// Code size and runtime are not impacted
            val e2 = visitExp(exp2, subst0)
            LiftedAst.Expression.Let(sym, e1, e2, tpe, purity, loc)
          }
        }
      }

    case OccurrenceAst.Expression.LetRec(varSym, index, defSym, exp1, exp2, tpe, loc) =>
      val e1 = visitExp(exp1, subst0)
      val e2 = visitExp(exp2, subst0)
      LiftedAst.Expression.LetRec(varSym, index, defSym, e1, e2, tpe, loc)

    case OccurrenceAst.Expression.Is(sym, tag, exp, loc) =>
      val e = visitExp(exp, subst0)
      LiftedAst.Expression.Is(sym, tag, e, loc)

    case OccurrenceAst.Expression.Tag(sym, tag, exp, tpe, loc) =>
      val e = visitExp(exp, subst0)
      LiftedAst.Expression.Tag(sym, tag, e, tpe, loc)

    case OccurrenceAst.Expression.Untag(sym, tag, exp, tpe, loc) =>
      val e = visitExp(exp, subst0)
      LiftedAst.Expression.Untag(sym, tag, e, tpe, loc)

    case OccurrenceAst.Expression.Index(base, offset, tpe, loc) =>
      val b = visitExp(base, subst0)
      LiftedAst.Expression.Index(b, offset, tpe, loc)

    case OccurrenceAst.Expression.Tuple(elms, tpe, loc) =>
      val es = elms.map(visitExp(_, subst0))
      LiftedAst.Expression.Tuple(es, tpe, loc)

    case OccurrenceAst.Expression.RecordEmpty(tpe, loc) => LiftedAst.Expression.RecordEmpty(tpe, loc)

    case OccurrenceAst.Expression.RecordSelect(exp, field, tpe, loc) =>
      val e = visitExp(exp, subst0)
      LiftedAst.Expression.RecordSelect(e, field, tpe, loc)

    case OccurrenceAst.Expression.RecordExtend(field, value, rest, tpe, loc) =>
      val v = visitExp(value, subst0)
      val r = visitExp(rest, subst0)
      LiftedAst.Expression.RecordExtend(field, v, r, tpe, loc)

    case OccurrenceAst.Expression.RecordRestrict(field, rest, tpe, loc) =>
      val r = visitExp(rest, subst0)
      LiftedAst.Expression.RecordRestrict(field, r, tpe, loc)

    case OccurrenceAst.Expression.ArrayLit(elms, tpe, loc) =>
      val es = elms.map(visitExp(_, subst0))
      LiftedAst.Expression.ArrayLit(es, tpe, loc)

    case OccurrenceAst.Expression.ArrayNew(elm, len, tpe, loc) =>
      val e = visitExp(elm, subst0)
      val l = visitExp(len, subst0)
      LiftedAst.Expression.ArrayNew(e, l, tpe, loc)

    case OccurrenceAst.Expression.ArrayLoad(base, index, tpe, loc) =>
      val b = visitExp(base, subst0)
      val i = visitExp(index, subst0)
      LiftedAst.Expression.ArrayLoad(b, i, tpe, loc)

    case OccurrenceAst.Expression.ArrayStore(base, index, elm, tpe, loc) =>
      val b = visitExp(base, subst0)
      val i = visitExp(index, subst0)
      val e = visitExp(elm, subst0)
      LiftedAst.Expression.ArrayStore(b, i, e, tpe, loc)

    case OccurrenceAst.Expression.ArrayLength(base, tpe, loc) =>
      val b = visitExp(base, subst0)
      LiftedAst.Expression.ArrayLength(b, tpe, loc)

    case OccurrenceAst.Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) =>
      val b = visitExp(base, subst0)
      val i1 = visitExp(beginIndex, subst0)
      val i2 = visitExp(endIndex, subst0)
      LiftedAst.Expression.ArraySlice(b, i1, i2, tpe, loc)

    case OccurrenceAst.Expression.Ref(exp, tpe, loc) =>
      val e = visitExp(exp, subst0)
      LiftedAst.Expression.Ref(e, tpe, loc)

    case OccurrenceAst.Expression.Deref(exp, tpe, loc) =>
      val e = visitExp(exp, subst0)
      LiftedAst.Expression.Deref(e, tpe, loc)

    case OccurrenceAst.Expression.Assign(exp1, exp2, tpe, loc) =>
      val e1 = visitExp(exp1, subst0)
      val e2 = visitExp(exp2, subst0)
      LiftedAst.Expression.Assign(e1, e2, tpe, loc)

    case OccurrenceAst.Expression.Cast(exp, tpe, loc) =>
      val e = visitExp(exp, subst0)
      LiftedAst.Expression.Cast(e, tpe, loc)

    case OccurrenceAst.Expression.TryCatch(exp, rules, tpe, loc) =>
      val e = visitExp(exp, subst0)
      val rs = rules.map {
        case OccurrenceAst.CatchRule(sym, clazz, exp) =>
          val e = visitExp(exp, subst0)
          LiftedAst.CatchRule(sym, clazz, e)
      }
      LiftedAst.Expression.TryCatch(e, rs, tpe, loc)

    case OccurrenceAst.Expression.InvokeConstructor(constructor, args, tpe, loc) =>
      val as = args.map(visitExp(_, subst0))
      LiftedAst.Expression.InvokeConstructor(constructor, as, tpe, loc)

    case OccurrenceAst.Expression.InvokeMethod(method, exp, args, tpe, loc) =>
      val e = visitExp(exp, subst0)
      val as = args.map(visitExp(_, subst0))
      LiftedAst.Expression.InvokeMethod(method, e, as, tpe, loc)

    case OccurrenceAst.Expression.InvokeStaticMethod(method, args, tpe, loc) =>
      val as = args.map(visitExp(_, subst0))
      LiftedAst.Expression.InvokeStaticMethod(method, as, tpe, loc)

    case OccurrenceAst.Expression.GetField(field, exp, tpe, loc) =>
      val e = visitExp(exp, subst0)
      LiftedAst.Expression.GetField(field, e, tpe, loc)

    case OccurrenceAst.Expression.PutField(field, exp1, exp2, tpe, loc) =>
      val e1 = visitExp(exp1, subst0)
      val e2 = visitExp(exp2, subst0)
      LiftedAst.Expression.PutField(field, e1, e2, tpe, loc)

    case OccurrenceAst.Expression.GetStaticField(field, tpe, loc) => LiftedAst.Expression.GetStaticField(field, tpe, loc)

    case OccurrenceAst.Expression.PutStaticField(field, exp, tpe, loc) =>
      val e = visitExp(exp, subst0)
      LiftedAst.Expression.PutStaticField(field, e, tpe, loc)

    case OccurrenceAst.Expression.NewChannel(exp, tpe, loc) =>
      val e = visitExp(exp, subst0)
      LiftedAst.Expression.NewChannel(e, tpe, loc)

    case OccurrenceAst.Expression.GetChannel(exp, tpe, loc) =>
      val e = visitExp(exp, subst0)
      LiftedAst.Expression.GetChannel(e, tpe, loc)

    case OccurrenceAst.Expression.PutChannel(exp1, exp2, tpe, loc) =>
      val e1 = visitExp(exp1, subst0)
      val e2 = visitExp(exp2, subst0)
      LiftedAst.Expression.PutChannel(e1, e2, tpe, loc)

    case OccurrenceAst.Expression.SelectChannel(rules, default, tpe, loc) =>
      val rs = rules.map {
        case OccurrenceAst.SelectChannelRule(sym, chan, exp) =>
          val c = visitExp(chan, subst0)
          val e = visitExp(exp, subst0)
          LiftedAst.SelectChannelRule(sym, c, e)
      }
      val d = default.map(visitExp(_, subst0))
      LiftedAst.Expression.SelectChannel(rs, d, tpe, loc)

    case OccurrenceAst.Expression.Spawn(exp, tpe, loc) =>
      val e = visitExp(exp, subst0)
      LiftedAst.Expression.Spawn(e, tpe, loc)

    case OccurrenceAst.Expression.Lazy(exp, tpe, loc) =>
      val e = visitExp(exp, subst0)
      LiftedAst.Expression.Lazy(e, tpe, loc)

    case OccurrenceAst.Expression.Force(exp, tpe, loc) =>
      val e = visitExp(exp, subst0)
      LiftedAst.Expression.Force(e, tpe, loc)

    case OccurrenceAst.Expression.HoleError(sym, tpe, loc) => LiftedAst.Expression.HoleError(sym, tpe, loc)

    case OccurrenceAst.Expression.MatchError(tpe, loc) => LiftedAst.Expression.MatchError(tpe, loc)
  }

  /**
   * Checks if `occur` is Dead and purity is `Pure`
   */
  private def isDeadAndPure(occur: OccurrenceAst.Occur, purity: Purity): Boolean = (occur, purity) match {
    case (Dead, Purity.Pure) => true
    case _ => false
  }

  /**
   * Checks if `occur` is Once and `purity` is Pure
   */
  private def isUsedOnceAndPure(occur: OccurrenceAst.Occur, purity: Purity): Boolean = (occur, purity) match {
    case (Once, Purity.Pure) => true
    case _ => false
  }

  /**
   * Checks if `exp0` is trivial and `purity` is pure
   */
  private def isTrivialAndPure(exp0: LiftedAst.Expression, purity: Purity): Boolean = purity match {
    case Purity.Pure => isTrivialExp(exp0)
    case _ => false
  }

  /**
   * returns `true` if `exp0` is considered a trivial expression.
   *
   * An expression is trivial if:
   * It is either a literal (float, string, int, bool, unit), or it is a variable.
   *
   * A pure and trivial expression can always be inlined even without duplicating work.
   */
  private def isTrivialExp(exp0: LiftedAst.Expression): Boolean = exp0 match {
    case LiftedAst.Expression.Unit(_) => true
    case LiftedAst.Expression.Null(_, _) => true
    case LiftedAst.Expression.True(_) => true
    case LiftedAst.Expression.False(_) => true
    case LiftedAst.Expression.Char(_, _) => true
    case LiftedAst.Expression.Float32(_, _) => true
    case LiftedAst.Expression.Float64(_, _) => true
    case LiftedAst.Expression.Int8(_, _) => true
    case LiftedAst.Expression.Int16(_, _) => true
    case LiftedAst.Expression.Int32(_, _) => true
    case LiftedAst.Expression.Int64(_, _) => true
    case LiftedAst.Expression.BigInt(_, _) => true
    case LiftedAst.Expression.Str(_, _) => true
    case LiftedAst.Expression.Var(_, _, _) => true
    case _ => false
  }

  /**
   * Translates the given formal parameter `fparam` from OccurrenceAst.FormalParam into a lifted formal parameter.
   */
  private def visitFormalParam(fparam: OccurrenceAst.FormalParam): LiftedAst.FormalParam = fparam match {
    case OccurrenceAst.FormalParam(sym, mod, tpe, loc) => LiftedAst.FormalParam(sym, mod, tpe, loc)
  }
}
