/*
 * Copyright 2018 Magnus Madsen, Anna Krogh, Patrick Lundvig, Christian Bonde
 * 2024 Jakob Schneider Villumsen
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
import ca.uwaterloo.flix.language.ast.OccurrenceAst1.Occur
import ca.uwaterloo.flix.language.ast.OccurrenceAst1.Occur.*
import ca.uwaterloo.flix.language.ast.{AtomicOp, MonoAst, OccurrenceAst1, Symbol, Type}
import ca.uwaterloo.flix.util.{ParOps, Validation}

/**
  * The inliner optionally performs beta-reduction at call-sites.
  * TODO: Improve this documentation
  */
object Inliner1 {

  sealed private trait SubstRange

  private object SubstRange {

    case class DoneExp(exp: MonoAst.Expr) extends SubstRange

    case class SuspendedExp(exp: OccurrenceAst1.Expr) extends SubstRange

  }

  private type VarSubst = Map[Symbol.VarSym, Symbol.VarSym]

  /**
    * Returns `true` if `def0` should be inlined.
    */
  private def canInlineDef(def0: OccurrenceAst1.Def): Boolean = {
    val mayInline = def0.context.occur != DontInline && !def0.context.isSelfRecursive
    val shouldInline = def0.context.isDirectCall ||
      def0.context.occur == Once ||
      def0.context.occur == OnceInLambda || // May duplicate work?
      def0.context.occur == OnceInLocalDef // May duplicate work?
    mayInline && shouldInline
  }

  /**
    * Performs inlining on the given AST `root`.
    */
  def run(root: OccurrenceAst1.Root)(implicit flix: Flix): Validation[MonoAst.Root, CompilationMessage] = {
    val defs = ParOps.parMapValues(root.defs)(d => visitDef(d)(flix, root))
    val structs = ParOps.parMapValues(root.structs)(visitStruct)
    val effects = ParOps.parMapValues(root.effects)(visitEffect)
    Validation.success(MonoAst.Root(defs, structs, effects, root.entryPoint, root.reachable, root.sources))
  }

  /**
    * Performs expression inlining on the given definition `def0`.
    * Converts definition from [[OccurrenceAst1]] to [[MonoAst]].
    */
  private def visitDef(def0: OccurrenceAst1.Def)(implicit flix: Flix, root: OccurrenceAst1.Root): MonoAst.Def = def0 match {
    case OccurrenceAst1.Def(sym, fparams, spec, exp, _, loc) =>
      val e = visitExp(exp, Map.empty)(root, flix)
      val sp = visitSpec(spec, fparams.map { case (fp, _) => fp })
      MonoAst.Def(sym, sp, e, loc)
  }

  private def visitSpec(spec0: OccurrenceAst1.Spec, fparams0: List[OccurrenceAst1.FormalParam]): MonoAst.Spec = spec0 match {
    case OccurrenceAst1.Spec(doc, ann, mod, functionType, retTpe, eff) =>
      val fps = fparams0.map(visitFormalParam)
      MonoAst.Spec(doc, ann, mod, fps, functionType, retTpe, eff)
  }

  private def visitStruct(struct0: OccurrenceAst1.Struct): MonoAst.Struct = struct0 match {
    case OccurrenceAst1.Struct(doc, ann, mod, sym, tparams, fields, loc) =>
      val fs = fields.map(visitStructField)
      MonoAst.Struct(doc, ann, mod, sym, tparams, fs, loc)
  }

  private def visitStructField(field0: OccurrenceAst1.StructField): MonoAst.StructField = field0 match {
    case OccurrenceAst1.StructField(sym, tpe, loc) =>
      MonoAst.StructField(sym, tpe, loc)
  }

  private def visitEffect(effect0: OccurrenceAst1.Effect): MonoAst.Effect = effect0 match {
    case OccurrenceAst1.Effect(doc0, ann0, mod0, sym0, ops0, loc0) =>
      val ops = ops0.map(visitEffectOp)
      MonoAst.Effect(doc0, ann0, mod0, sym0, ops, loc0)
  }

  private def visitEffectOp(op: OccurrenceAst1.Op): MonoAst.Op = op match {
    case OccurrenceAst1.Op(sym0, fparams0, spec0, loc) =>
      val spec = visitSpec(spec0, fparams0)
      MonoAst.Op(sym0, spec, loc)
  }

  /**
    * Translates the given formal parameter `fparam` from [[OccurrenceAst1.FormalParam]] into a [[MonoAst.FormalParam]].
    */
  private def visitFormalParam(fparam: OccurrenceAst1.FormalParam): MonoAst.FormalParam = fparam match {
    case OccurrenceAst1.FormalParam(sym, mod, tpe, src, loc) => MonoAst.FormalParam(sym, mod, tpe, src, loc)
  }

  /**
    * Performs inlining operations on the expression `exp0` from [[OccurrenceAst1.Expr]].
    * Returns a [[MonoAst.Expr]]
    */
  private def visitExp(exp0: OccurrenceAst1.Expr, subst0: Map[Symbol.VarSym, SubstRange])(implicit root: OccurrenceAst1.Root, flix: Flix): MonoAst.Expr = exp0 match { // TODO: Add local `visit` function that captures `subst0`
    case OccurrenceAst1.Expr.Cst(cst, tpe, loc) =>
      MonoAst.Expr.Cst(cst, tpe, loc)

    case OccurrenceAst1.Expr.Var(sym, tpe, loc) =>
      subst0.get(sym) match {
        // Case 1:
        // The variable `sym` is not in the substitution map and will not be inlined.
        case None => MonoAst.Expr.Var(sym, tpe, loc)
        // Case 2:
        // The variable `sym` is in the substitution map. Replace `sym` with `e1`.
        case Some(e1) =>
          e1 match {
            // If `e1` is a `LiftedExp` then `e1` has already been visited
            case SubstRange.DoneExp(exp) => exp
            // If `e1` is a `OccurrenceExp` then `e1` has not been visited. Visit `e1`
            case SubstRange.SuspendedExp(exp) => visitExp(exp, subst0)
          }
      }

    case OccurrenceAst1.Expr.Lambda(fparam, exp, tpe, loc) =>
      val fps = visitFormalParam(fparam)
      val e = visitExp(exp, subst0)
      MonoAst.Expr.Lambda(fps, e, tpe, loc)

    case OccurrenceAst1.Expr.ApplyAtomic(op, exps, tpe, eff, loc) =>
      val es = exps.map(visitExp(_, subst0))
      op match {
        case AtomicOp.Untag(_) =>
          val List(e) = es
          // Inline expressions of the form Untag(Tag(e)) => e
          e match {
            case MonoAst.Expr.ApplyAtomic(AtomicOp.Tag(_), innerExps, _, _, _) => innerExps.head
            case _ => MonoAst.Expr.ApplyAtomic(op, es, tpe, eff, loc)
          }

        case _ => MonoAst.Expr.ApplyAtomic(op, es, tpe, eff, loc)
      }

    case OccurrenceAst1.Expr.ApplyClo(exp, exps, tpe, eff, loc) =>
      val e = visitExp(exp, subst0)
      val es = exps.map(visitExp(_, subst0))
      e match {
        case MonoAst.Expr.ApplyAtomic(AtomicOp.Closure(sym), closureArgs, _, _, _) =>
          val def1 = root.defs.apply(sym)
          // If `def1` is a single non-self call or is trivial
          // then inline the body of `def1`
          if (canInlineDef(def1)) {
            // Map for substituting formal parameters of a function with the closureArgs currently in scope
            bindFormals(def1.exp, def1.fparams, closureArgs ++ es, Map.empty)
          } else {
            MonoAst.Expr.ApplyClo(e, es, tpe, eff, loc)
          }
        case _ => MonoAst.Expr.ApplyClo(e, es, tpe, eff, loc)
      }

    case OccurrenceAst1.Expr.ApplyDef(sym, exps, itpe, tpe, eff, loc) =>
      val es = exps.map(visitExp(_, subst0))
      val def1 = root.defs.apply(sym)
      // If `def1` is a single non-self call or is trivial
      // then inline the body of `def1`
      if (canInlineDef(def1)) {
        bindFormals(def1.exp, def1.fparams, es, Map.empty)
      } else {
        MonoAst.Expr.ApplyDef(sym, es, itpe, tpe, eff, loc)
      }

    case OccurrenceAst1.Expr.ApplyLocalDef(sym, exps, tpe, eff, loc) =>
      val es = exps.map(visitExp(_, subst0))
      MonoAst.Expr.ApplyLocalDef(sym, es, tpe, eff, loc)

    case OccurrenceAst1.Expr.Let(sym, exp1, exp2, tpe, eff, occur, loc) =>
      if (isDead(occur)) {
        if (isPure(exp1.eff)) {
          // Case 1:
          // If `sym` is never used (it is `Dead`)  and `exp1` is pure, so it has no side effects, then it is safe to remove `sym`
          // Both code size and runtime are reduced
          visitExp(exp2, subst0)
        } else {
          // Case 2:
          // If `sym` is never used (it is `Dead`) so it is safe to make a Stm.
          MonoAst.Expr.Stm(visitExp(exp1, subst0), visitExp(exp2, subst0), tpe, eff, loc)
        }
      } else {
        // Case 3:
        // If `exp1` occurs once and it is pure, then it is safe to inline.
        // There is a small decrease in code size and runtime.
        val wantToPreInline = isUsedOnceAndPure(occur, exp1.eff)
        if (wantToPreInline) {
          val subst1 = subst0 + (sym -> SubstRange.SuspendedExp(exp1))
          visitExp(exp2, subst1)
        } else {
          val e1 = visitExp(exp1, subst0)
          // Case 4:
          // If `e1` is trivial and pure, then it is safe to inline.
          // Code size and runtime are not impacted, because only trivial expressions are inlined
          val wantToPostInline = isTrivialAndPure(e1, exp1.eff) && occur != DontInline
          if (wantToPostInline) {
            // If `e1` is to be inlined:
            // Add map `sym` to `e1` and return `e2` without constructing the let expression.
            val subst1 = subst0 + (sym -> SubstRange.DoneExp(e1))
            visitExp(exp2, subst1)
          } else {
            // Case 5:
            // If none of the previous cases pass, `sym` is not inlined. Return a let expression with the visited expressions
            // Code size and runtime are not impacted
            val e2 = visitExp(exp2, subst0)
            MonoAst.Expr.Let(sym, e1, e2, tpe, eff, loc)
          }
        }
      }

    case OccurrenceAst1.Expr.LocalDef(sym, fparams, exp1, exp2, tpe, eff, occur, loc) =>
      // TODO: Update this case if we want to inline
      // Current impl is just placeholder
      val fps = fparams.map(visitFormalParam)
      val e1 = visitExp(exp1, subst0)
      val e2 = visitExp(exp2, subst0)
      MonoAst.Expr.LocalDef(sym, fps, e1, e2, tpe, eff, loc)

    case OccurrenceAst1.Expr.Scope(sym, rvar, exp, tpe, eff, loc) =>
      val e = visitExp(exp, subst0)
      MonoAst.Expr.Scope(sym, rvar, e, tpe, eff, loc)

    case OccurrenceAst1.Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
      val e1 = visitExp(exp1, subst0)
      val e2 = visitExp(exp2, subst0)
      val e3 = visitExp(exp3, subst0)
      MonoAst.Expr.IfThenElse(e1, e2, e3, tpe, eff, loc)

    case OccurrenceAst1.Expr.Stm(exp1, exp2, tpe, eff, loc) =>
      // Case 1:
      // If `exp1` is pure, so it has no side effects, then it is safe to remove
      // Both code size and runtime are reduced
      if (isPure(exp1.eff)) {
        visitExp(exp2, subst0)
      } else {
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        MonoAst.Expr.Stm(e1, e2, tpe, eff, loc)
      }

    case OccurrenceAst1.Expr.Discard(exp, eff, loc) =>
      val e = visitExp(exp, subst0)
      MonoAst.Expr.Discard(e, eff, loc)

    case OccurrenceAst1.Expr.Match(exp, rules, tpe, eff, loc) =>
      val e = visitExp(exp, subst0)
      val rs = rules.map {
        case OccurrenceAst1.MatchRule(pat, guard, exp) =>
          val p = visitPattern(pat)
          val g = guard.map(visitExp(_, subst0))
          val e = visitExp(exp, subst0)
          MonoAst.MatchRule(p, g, e)
      }
      MonoAst.Expr.Match(e, rs, tpe, eff, loc)

    case OccurrenceAst1.Expr.VectorLit(exps, tpe, eff, loc) =>
      val es = exps.map(visitExp(_, subst0))
      MonoAst.Expr.VectorLit(es, tpe, eff, loc)

    case OccurrenceAst1.Expr.VectorLoad(exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1, subst0)
      val e2 = visitExp(exp2, subst0)
      MonoAst.Expr.VectorLoad(e1, e2, tpe, eff, loc)

    case OccurrenceAst1.Expr.VectorLength(exp, loc) =>
      val e = visitExp(exp, subst0)
      MonoAst.Expr.VectorLength(e, loc)

    case OccurrenceAst1.Expr.Ascribe(exp, tpe, eff, loc) =>
      val e = visitExp(exp, subst0)
      MonoAst.Expr.Ascribe(e, tpe, eff, loc)

    case OccurrenceAst1.Expr.Cast(exp, declaredType, declaredEff, tpe, eff, loc) =>
      val e = visitExp(exp, subst0)
      MonoAst.Expr.Cast(e, declaredType, declaredEff, tpe, eff, loc)

    case OccurrenceAst1.Expr.TryCatch(exp, rules, tpe, eff, loc) =>
      val e = visitExp(exp, subst0)
      val rs = rules.map {
        case OccurrenceAst1.CatchRule(sym, clazz, exp) =>
          val e = visitExp(exp, subst0)
          MonoAst.CatchRule(sym, clazz, e)
      }
      MonoAst.Expr.TryCatch(e, rs, tpe, eff, loc)

    case OccurrenceAst1.Expr.TryWith(exp, effUse, rules, tpe, eff, loc) =>
      val e = visitExp(exp, subst0)
      val rs = rules.map {
        case OccurrenceAst1.HandlerRule(op, fparams, exp) =>
          val fps = fparams.map(visitFormalParam)
          val e = visitExp(exp, subst0)
          MonoAst.HandlerRule(op, fps, e)
      }
      MonoAst.Expr.TryWith(e, effUse, rs, tpe, eff, loc)

    case OccurrenceAst1.Expr.Do(op, exps, tpe, eff, loc) =>
      val es = exps.map(visitExp(_, subst0))
      MonoAst.Expr.Do(op, es, tpe, eff, loc)

    case OccurrenceAst1.Expr.NewObject(name, clazz, tpe, eff, methods0, loc) =>
      val methods = methods0.map {
        case OccurrenceAst1.JvmMethod(ident, fparams, exp, retTpe, eff, loc) =>
          val fps = fparams.map(visitFormalParam)
          val e = visitExp(exp, subst0)
          MonoAst.JvmMethod(ident, fps, e, retTpe, eff, loc)
      }
      MonoAst.Expr.NewObject(name, clazz, tpe, eff, methods, loc)

  }

  private def visitPattern(pattern00: OccurrenceAst1.Pattern): MonoAst.Pattern = {

    // TODO: Figure out what to do with occurrence information in Pattern.Var
    def visit(pattern0: OccurrenceAst1.Pattern): MonoAst.Pattern = pattern0 match {
      case OccurrenceAst1.Pattern.Wild(tpe, loc) =>
        MonoAst.Pattern.Wild(tpe, loc)

      case OccurrenceAst1.Pattern.Var(sym, tpe, _, loc) =>
        MonoAst.Pattern.Var(sym, tpe, loc)

      case OccurrenceAst1.Pattern.Cst(cst, tpe, loc) =>
        MonoAst.Pattern.Cst(cst, tpe, loc)

      case OccurrenceAst1.Pattern.Tag(sym, pat, tpe, loc) =>
        val p = visit(pat)
        MonoAst.Pattern.Tag(sym, p, tpe, loc)

      case OccurrenceAst1.Pattern.Tuple(pats, tpe, loc) =>
        val ps = pats.map(visit)
        MonoAst.Pattern.Tuple(ps, tpe, loc)

      case OccurrenceAst1.Pattern.Record(pats, pat, tpe, loc) =>
        val ps = pats.map(visitRecordLabelPattern)
        val p = visit(pat)
        MonoAst.Pattern.Record(ps, p, tpe, loc)

      case OccurrenceAst1.Pattern.RecordEmpty(tpe, loc) =>
        MonoAst.Pattern.RecordEmpty(tpe, loc)
    }

    def visitRecordLabelPattern(pattern0: OccurrenceAst1.Pattern.Record.RecordLabelPattern): MonoAst.Pattern.Record.RecordLabelPattern = pattern0 match {
      case OccurrenceAst1.Pattern.Record.RecordLabelPattern(label, pat, tpe, loc) =>
        val p = visit(pat)
        MonoAst.Pattern.Record.RecordLabelPattern(label, p, tpe, loc)
    }

    visit(pattern00)
  }

  private def isPure(eff0: Type): Boolean = {
    eff0.effects.isEmpty
  }

  /**
    * Checks if `occur` is Dead.
    */
  private def isDead(occur: OccurrenceAst1.Occur): Boolean = occur match {
    case Dead => true
    case _ => false
  }

  /**
    * Checks if `occur` is Once and `eff` is Pure
    */
  private def isUsedOnceAndPure(occur: OccurrenceAst1.Occur, eff0: Type): Boolean = occur match {
    case Once => isPure(eff0)
    case _ => false
  }

  /**
    * Checks if `exp0` is trivial and `eff` is pure
    */
  private def isTrivialAndPure(exp0: MonoAst.Expr, eff0: Type): Boolean = {
    isPure(eff0) && isTrivialExp(exp0)
  }

  /**
    * Checks if `occur` is dead and `exp` is pure.
    */
  private def isDeadAndPure(occur: OccurrenceAst1.Occur, eff0: Type): Boolean = occur match {
    case Dead => isPure(eff0)
    case _ => false
  }

  /**
    * Recursively bind each argument in `args` to a let-expression with a fresh symbol
    * Add corresponding symbol from `symbols` to substitution map `env0`, mapping old symbols to fresh symbols.
    * Substitute variables in `exp0` via the filled substitution map `env0`
    */
  private def bindFormals(exp0: OccurrenceAst1.Expr, symbols: List[(OccurrenceAst1.FormalParam, Occur)], args: List[MonoAst.Expr], env0: Map[Symbol.VarSym, Symbol.VarSym])(implicit root: OccurrenceAst1.Root, flix: Flix): MonoAst.Expr = (symbols, args) match {
    case ((_, occur) :: nextSymbols, e1 :: nextExpressions) if isDeadAndPure(occur, e1.eff) =>
      // If the parameter is unused and the argument is pure, then throw it away.
      bindFormals(exp0, nextSymbols, nextExpressions, env0)

    case ((_, occur) :: nextSymbols, e1 :: nextExpressions) if isDead(occur) =>
      // If the parameter is unused and the argument is NOT pure, then put it in a statement.
      val nextLet = bindFormals(exp0, nextSymbols, nextExpressions, env0)
      val eff = Type.mkUnion(e1.eff, nextLet.eff, e1.loc)
      MonoAst.Expr.Stm(e1, nextLet, exp0.tpe, eff, exp0.loc)

    case ((OccurrenceAst1.FormalParam(sym, _, _, _, _), _) :: nextSymbols, e1 :: nextExpressions) =>
      val freshVar = Symbol.freshVarSym(sym)
      val env1 = env0 + (sym -> freshVar)
      val nextLet = bindFormals(exp0, nextSymbols, nextExpressions, env1)
      val eff = Type.mkUnion(e1.eff, nextLet.eff, e1.loc)
      MonoAst.Expr.Let(freshVar, e1, nextLet, exp0.tpe, eff, exp0.loc)

    case _ => applySubst(exp0, env0)
  }

  /**
    * Returns `true` if `exp0` is considered a trivial expression.
    *
    * An expression is trivial if:
    * It is either a literal (float, string, int, bool, unit), or it is a variable.
    *
    * A pure and trivial expression can always be inlined even without duplicating work.
    */
  private def isTrivialExp(exp0: MonoAst.Expr): Boolean = exp0 match {
    case MonoAst.Expr.Cst(_, _, _) => true
    case MonoAst.Expr.Var(_, _, _) => true
    case MonoAst.Expr.ApplyAtomic(AtomicOp.Tag(_), exps, _, _, _) => exps.forall(isTrivialExp)
    case _ => false
  }

  /**
    * Substitute variables in `exp0` for new fresh variables in `env0`
    */
  private def applySubst(exp0: OccurrenceAst1.Expr, subst0: VarSubst)(implicit root: OccurrenceAst1.Root, flix: Flix): MonoAst.Expr = exp0 match {
    case OccurrenceAst1.Expr.Cst(cst, tpe, loc) =>
      MonoAst.Expr.Cst(cst, tpe, loc)

    case OccurrenceAst1.Expr.Var(sym, tpe, loc) =>
      MonoAst.Expr.Var(subst0.getOrElse(sym, sym), tpe, loc)

    case OccurrenceAst1.Expr.Lambda(fparam, exp, tpe, loc) =>
      val (fp, subst1) = applySubstFormalParam(fparam)
      val subst2 = subst0 ++ subst1
      val e = applySubst(exp, subst2)
      MonoAst.Expr.Lambda(fp, e, tpe, loc)

    case OccurrenceAst1.Expr.ApplyAtomic(op, exps, tpe, eff, loc) =>
      val es = exps.map(applySubst(_, subst0))
      MonoAst.Expr.ApplyAtomic(op, es, tpe, eff, loc)

    case OccurrenceAst1.Expr.ApplyClo(exp, exps, tpe, eff, loc) =>
      val e = applySubst(exp, subst0)
      val es = exps.map(applySubst(_, subst0))
      MonoAst.Expr.ApplyClo(e, es, tpe, eff, loc)

    case OccurrenceAst1.Expr.ApplyDef(sym, exps, itpe, tpe, eff, loc) =>
      val es = exps.map(applySubst(_, subst0))
      MonoAst.Expr.ApplyDef(sym, es, itpe, tpe, eff, loc)

    case OccurrenceAst1.Expr.ApplyLocalDef(sym, exps, tpe, eff, loc) =>
      val es = exps.map(applySubst(_, subst0))
      MonoAst.Expr.ApplyLocalDef(subst0.getOrElse(sym, sym), es, tpe, eff, loc)

    case OccurrenceAst1.Expr.Let(sym, exp1, exp2, tpe, eff, _, loc) =>
      val freshVar = Symbol.freshVarSym(sym)
      val env1 = subst0 + (sym -> freshVar)
      val e1 = applySubst(exp1, env1)
      val e2 = applySubst(exp2, env1)
      MonoAst.Expr.Let(freshVar, e1, e2, tpe, eff, loc)

    case OccurrenceAst1.Expr.LocalDef(sym, fparams, exp1, exp2, tpe, eff, _, loc) =>
      val freshVarSym = Symbol.freshVarSym(sym)
      val subst1 = subst0 + (sym -> freshVarSym)
      val e2 = applySubst(exp2, subst1) // e2 should not know about fparams
      val (fps, substs) = fparams.map(applySubstFormalParam).unzip
      val subst2 = subst1 ++ substs.flatten
      val e1 = applySubst(exp1, subst2)
      MonoAst.Expr.LocalDef(freshVarSym, fps, e1, e2, tpe, eff, loc)

    case OccurrenceAst1.Expr.Scope(sym, rvar, exp, tpe, eff, loc) =>
      val e = applySubst(exp, subst0)
      MonoAst.Expr.Scope(sym, rvar, e, tpe, eff, loc)

    case OccurrenceAst1.Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
      val e1 = applySubst(exp1, subst0)
      val e2 = applySubst(exp2, subst0)
      val e3 = applySubst(exp3, subst0)
      MonoAst.Expr.IfThenElse(e1, e2, e3, tpe, eff, loc)

    case OccurrenceAst1.Expr.Stm(exp1, exp2, tpe, eff, loc) =>
      val e1 = applySubst(exp1, subst0)
      val e2 = applySubst(exp2, subst0)
      MonoAst.Expr.Stm(e1, e2, tpe, eff, loc)

    case OccurrenceAst1.Expr.Discard(exp, eff, loc) =>
      val e = applySubst(exp, subst0)
      MonoAst.Expr.Discard(e, eff, loc)

    case OccurrenceAst1.Expr.Match(exp, rules, tpe, eff, loc) =>
      val e = applySubst(exp, subst0)
      val rs = rules.map {
        case OccurrenceAst1.MatchRule(pat, guard, exp) =>
          val (p, env1) = applySubstPattern(pat)
          val env2 = subst0 ++ env1
          val g = guard.map(applySubst(_, env2))
          val e = applySubst(exp, env2)
          MonoAst.MatchRule(p, g, e)
      }
      MonoAst.Expr.Match(e, rs, tpe, eff, loc)

    case OccurrenceAst1.Expr.VectorLit(exps, tpe, eff, loc) =>
      val es = exps.map(applySubst(_, subst0))
      MonoAst.Expr.VectorLit(es, tpe, eff, loc)

    case OccurrenceAst1.Expr.VectorLoad(exp1, exp2, tpe, eff, loc) =>
      val e1 = applySubst(exp1, subst0)
      val e2 = applySubst(exp2, subst0)
      MonoAst.Expr.VectorLoad(e1, e2, tpe, eff, loc)

    case OccurrenceAst1.Expr.VectorLength(exp, loc) =>
      val e = applySubst(exp, subst0)
      MonoAst.Expr.VectorLength(e, loc)

    case OccurrenceAst1.Expr.Ascribe(exp, tpe, eff, loc) =>
      val e = applySubst(exp, subst0)
      MonoAst.Expr.Ascribe(e, tpe, eff, loc)

    case OccurrenceAst1.Expr.Cast(exp, declaredType, declaredEff, tpe, eff, loc) =>
      val e = applySubst(exp, subst0)
      MonoAst.Expr.Cast(e, declaredType, declaredEff, tpe, eff, loc)

    case OccurrenceAst1.Expr.TryCatch(exp, rules, tpe, eff, loc) =>
      val e = applySubst(exp, subst0)
      val rs = rules.map {
        case OccurrenceAst1.CatchRule(sym, clazz, exp) =>
          val freshVar = Symbol.freshVarSym(sym)
          val env1 = subst0 + (sym -> freshVar)
          val e = applySubst(exp, env1)
          MonoAst.CatchRule(freshVar, clazz, e)
      }
      MonoAst.Expr.TryCatch(e, rs, tpe, eff, loc)

    case OccurrenceAst1.Expr.TryWith(exp, effUse, rules, tpe, eff, loc) =>
      val e = applySubst(exp, subst0)
      val rs = rules.map {
        case OccurrenceAst1.HandlerRule(op, fparams, exp) =>
          val (fps, substs) = fparams.map(applySubstFormalParam).unzip
          val subst1 = subst0 ++ substs.flatten
          val e = applySubst(exp, subst1)
          MonoAst.HandlerRule(op, fps, e)
      }
      MonoAst.Expr.TryWith(e, effUse, rs, tpe, eff, loc)

    case OccurrenceAst1.Expr.Do(op, exps, tpe, eff, loc) =>
      val es = exps.map(applySubst(_, subst0))
      MonoAst.Expr.Do(op, es, tpe, eff, loc)

    case OccurrenceAst1.Expr.NewObject(name, clazz, tpe, eff, methods0, loc) =>
      val methods = methods0.map {
        case OccurrenceAst1.JvmMethod(ident, fparams, exp, retTpe, eff, loc) =>
          val (fps, substs) = fparams.map(applySubstFormalParam).unzip
          val subst1 = subst0 ++ substs.flatten
          val e = applySubst(exp, subst1)
          MonoAst.JvmMethod(ident, fps, e, retTpe, eff, loc)
      }
      MonoAst.Expr.NewObject(name, clazz, tpe, eff, methods, loc)
  }

  private def applySubstPattern(pattern00: OccurrenceAst1.Pattern)(implicit flix: Flix): (MonoAst.Pattern, VarSubst) = {

    def visit(pattern0: OccurrenceAst1.Pattern): (MonoAst.Pattern, VarSubst) = pattern0 match {
      case OccurrenceAst1.Pattern.Wild(tpe, loc) =>
        (MonoAst.Pattern.Wild(tpe, loc), Map.empty)

      case OccurrenceAst1.Pattern.Var(sym, tpe, _, loc) =>
        val freshVarSym = Symbol.freshVarSym(sym)
        val subst = Map(sym -> freshVarSym)
        (MonoAst.Pattern.Var(freshVarSym, tpe, loc), subst)

      case OccurrenceAst1.Pattern.Cst(cst, tpe, loc) =>
        (MonoAst.Pattern.Cst(cst, tpe, loc), Map.empty)

      case OccurrenceAst1.Pattern.Tag(sym, pat, tpe, loc) =>
        val (p, subst) = visit(pat)
        (MonoAst.Pattern.Tag(sym, p, tpe, loc), subst)

      case OccurrenceAst1.Pattern.Tuple(pats, tpe, loc) =>
        val (ps, substs) = pats.map(visit).unzip
        val subst = substs.foldLeft(Map.empty[Symbol.VarSym, Symbol.VarSym])(_ ++ _)
        (MonoAst.Pattern.Tuple(ps, tpe, loc), subst)

      case OccurrenceAst1.Pattern.Record(pats, pat, tpe, loc) =>
        val (ps, substs) = pats.map(visitRecordLabelPattern).unzip
        val (p, subst0) = visit(pat)
        val subst = substs.foldLeft(Map.empty[Symbol.VarSym, Symbol.VarSym])(_ ++ _) ++ subst0
        (MonoAst.Pattern.Record(ps, p, tpe, loc), subst)

      case OccurrenceAst1.Pattern.RecordEmpty(tpe, loc) =>
        (MonoAst.Pattern.RecordEmpty(tpe, loc), Map.empty)
    }

    def visitRecordLabelPattern(pattern0: OccurrenceAst1.Pattern.Record.RecordLabelPattern): (MonoAst.Pattern.Record.RecordLabelPattern, VarSubst) = pattern0 match {
      case OccurrenceAst1.Pattern.Record.RecordLabelPattern(label, pat, tpe, loc) =>
        val (p, subst) = visit(pat)
        (MonoAst.Pattern.Record.RecordLabelPattern(label, p, tpe, loc), subst)
    }

    visit(pattern00)
  }

  private def applySubstFormalParam(fp0: OccurrenceAst1.FormalParam)(implicit flix: Flix): (MonoAst.FormalParam, VarSubst) = fp0 match {
    case OccurrenceAst1.FormalParam(sym, mod, tpe, src, loc) =>
      val freshVarSym = Symbol.freshVarSym(sym)
      val subst = Map(sym -> freshVarSym)
      (MonoAst.FormalParam(freshVarSym, mod, tpe, src, loc), subst)
  }
}
