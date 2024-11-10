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
import ca.uwaterloo.flix.language.ast.Ast.BoundBy
import ca.uwaterloo.flix.language.ast.OccurrenceAst1.{DefContext, Occur}
import ca.uwaterloo.flix.language.ast.OccurrenceAst1.Occur.*
import ca.uwaterloo.flix.language.ast.shared.Constant
import ca.uwaterloo.flix.language.ast.{AtomicOp, MonoAst, OccurrenceAst1, Symbol, Type}
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps, Validation}

/**
  * The inliner optionally performs beta-reduction at call-sites.
  * TODO: Improve this documentation
  */
object Inliner1 {

  /**
    * Performs inlining on the given AST `root`.
    */
  def run(root: OccurrenceAst1.Root)(implicit flix: Flix): Validation[MonoAst.Root, CompilationMessage] = {
    val defs = ParOps.parMapValues(root.defs)(d => visitDef(d)(flix, root))
    val effects = ParOps.parMapValues(root.effects)(visitEffect)
    val enums = ParOps.parMapValues(root.enums)(visitEnum)
    val structs = ParOps.parMapValues(root.structs)(visitStruct)
    Validation.success(MonoAst.Root(defs, enums, structs, effects, root.entryPoint, root.reachable, root.sources))
  }

  private type InVar = Symbol.VarSym

  private type OutVar = Symbol.VarSym

  private type InExpr = OccurrenceAst1.Expr

  private type OutExpr = MonoAst.Expr

  private type InCase = OccurrenceAst1.Case

  private type OutCase = MonoAst.Case

  sealed private trait SubstRange

  private object SubstRange {

    case class SuspendedExp(exp: InExpr) extends SubstRange

    case class DoneExp(exp: OutExpr) extends SubstRange

  }

  private sealed trait Const

  private object Const {

    case class Lit(lit: Constant) extends Const

    case class Tag(tag: Symbol.CaseSym) extends Const

  }

  private sealed trait Definition

  private object Definition {

    case object CaseOrLambda extends Definition

    case class LetBound(expr: OutExpr, occur: Occur) extends Definition

    case class NotAmong(cases: List[Const]) extends Definition

  }

  private type VarSubst = Map[InVar, OutVar]

  private type Subst = Map[InVar, SubstRange]

  private type InScopeSet = Map[OutVar, Definition]

  private sealed trait Context

  private object Context {

    case object Stop extends Context

    case class Application(inExpr: InExpr, subst: Subst, context: Context) extends Context

    case class Argument(continuation: OutExpr => OutExpr) extends Context

    case class Match(sym: InVar, cases: List[InCase], subst: Subst, context: Context) extends Context

  }

  /**
    * Performs expression inlining on the given definition `def0`.
    * Converts definition from [[OccurrenceAst1]] to [[MonoAst]].
    */
  private def visitDef(def0: OccurrenceAst1.Def)(implicit flix: Flix, root: OccurrenceAst1.Root): MonoAst.Def = def0 match {
    case OccurrenceAst1.Def(sym, fparams, spec, exp, _, loc) =>
      val e = visitExp(exp, Map.empty, Map.empty)(root, flix)
      val sp = visitSpec(spec, fparams.map { case (fp, _) => fp })
      MonoAst.Def(sym, sp, e, loc)
  }

  private def visitSpec(spec0: OccurrenceAst1.Spec, fparams0: List[OccurrenceAst1.FormalParam]): MonoAst.Spec = spec0 match {
    case OccurrenceAst1.Spec(doc, ann, mod, functionType, retTpe, eff) =>
      val fps = fparams0.map(visitFormalParam)
      MonoAst.Spec(doc, ann, mod, fps, functionType, retTpe, eff)
  }

  private def visitEnum(enum0: OccurrenceAst1.Enum): MonoAst.Enum = enum0 match {
    case OccurrenceAst1.Enum(doc, ann, mod, sym, tparams, cases, loc) =>
      val tps = tparams.map(visitTypeParam)
      val cs = cases.map { case (sym, caze) => sym -> visitEnumCase(caze) }
      MonoAst.Enum(doc, ann, mod, sym, tps, cs, loc)
  }

  private def visitEnumCase(case0: OccurrenceAst1.Case): MonoAst.Case = case0 match {
    case OccurrenceAst1.Case(sym, tpe, loc) => MonoAst.Case(sym, tpe, loc)
  }

  private def visitStruct(struct0: OccurrenceAst1.Struct): MonoAst.Struct = struct0 match {
    case OccurrenceAst1.Struct(doc, ann, mod, sym, tparams, fields, loc) =>
      val tps = tparams.map(visitTypeParam)
      val fs = fields.map(visitStructField)
      MonoAst.Struct(doc, ann, mod, sym, tps, fs, loc)
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

  private def visitEffectOp(op0: OccurrenceAst1.Op): MonoAst.Op = op0 match {
    case OccurrenceAst1.Op(sym0, fparams0, spec0, loc) =>
      val spec = visitSpec(spec0, fparams0)
      MonoAst.Op(sym0, spec, loc)
  }

  private def visitTypeParam(tparam0: OccurrenceAst1.TypeParam): MonoAst.TypeParam = tparam0 match {
    case OccurrenceAst1.TypeParam(name, sym, loc) => MonoAst.TypeParam(name, sym, loc)
  }

  /**
    * Translates the given formal parameter `fparam` from [[OccurrenceAst1.FormalParam]] into a [[MonoAst.FormalParam]].
    */
  private def visitFormalParam(fparam0: OccurrenceAst1.FormalParam): MonoAst.FormalParam = fparam0 match {
    case OccurrenceAst1.FormalParam(sym, mod, tpe, src, loc) => MonoAst.FormalParam(sym, mod, tpe, src, loc)
  }

  /**
    * Performs inlining operations on the expression `exp0` from [[OccurrenceAst1.Expr]].
    * Returns a [[MonoAst.Expr]]
    */
  private def visitExp(exp00: OccurrenceAst1.Expr, subst0: Subst, inScopeSet0: InScopeSet)(implicit root: OccurrenceAst1.Root, flix: Flix): MonoAst.Expr = {

    def visit(exp0: OccurrenceAst1.Expr): MonoAst.Expr = exp0 match {
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
              case SubstRange.SuspendedExp(exp) => visit(exp)
            }
        }

      case OccurrenceAst1.Expr.Lambda((fparam, occur), exp, tpe, loc) =>
        val fps = visitFormalParam(fparam)
        val e = visit(exp)
        MonoAst.Expr.Lambda(fps, e, tpe, loc)

      case OccurrenceAst1.Expr.ApplyAtomic(op, exps, tpe, eff, loc) =>
        val es = exps.map(visit)
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
        val es = exps.map(visit)
        exp match {
          case OccurrenceAst1.Expr.Var(sym, _, _) =>
            inScopeSet0.get(sym) match {
              case Some(Definition.LetBound(lambda, occur)) if occur == Occur.OnceInAbstraction && isPure(lambda.eff) =>
                MonoAst.Expr.ApplyClo(lambda, es, tpe, eff, loc)

              case _ =>
                val e = visit(exp)
                MonoAst.Expr.ApplyClo(e, es, tpe, eff, loc)
            }

          case OccurrenceAst1.Expr.Lambda(fparam, exp, _, _) =>
            bindFormals(exp, List(fparam), es, subst0)

          case _ =>
            val e = visit(exp)
            MonoAst.Expr.ApplyClo(e, es, tpe, eff, loc)
        }

      case OccurrenceAst1.Expr.ApplyDef(sym, exps, itpe, tpe, eff, loc) =>
        val es = exps.map(visit)
        val def1 = root.defs.apply(sym)
        // If `def1` is a single non-self call or is trivial
        // then inline the body of `def1`
        if (canInlineDef(def1.context)) {
          bindFormals(def1.exp, def1.fparams, es, Map.empty)
        } else {
          MonoAst.Expr.ApplyDef(sym, es, itpe, tpe, eff, loc)
        }

      case OccurrenceAst1.Expr.ApplyLocalDef(sym, exps, tpe, eff, loc) =>
        val es = exps.map(visit)
        MonoAst.Expr.ApplyLocalDef(sym, es, tpe, eff, loc)

      case OccurrenceAst1.Expr.Let(sym, exp1, exp2, tpe, eff, occur, loc) =>
        if (isDead(occur)) {
          if (isPure(exp1.eff)) {
            // Case 1:
            // If `sym` is never used (it is `Dead`)  and `exp1` is pure, so it has no side effects, then it is safe to remove `sym`
            // Both code size and runtime are reduced
            visit(exp2)
          } else {
            // Case 2:
            // If `sym` is never used (it is `Dead`) so it is safe to make a Stm.
            MonoAst.Expr.Stm(visit(exp1), visit(exp2), tpe, eff, loc)
          }
        } else {
          // Case 3:
          // If `exp1` occurs once and it is pure, then it is safe to inline.
          // There is a small decrease in code size and runtime.
          val wantToPreInline = isUsedOnceAndPure(occur, exp1.eff)
          if (wantToPreInline) {
            val subst1 = subst0 + (sym -> SubstRange.SuspendedExp(exp1))
            visitExp(exp2, subst1, inScopeSet0)
          } else {
            val e1 = visit(exp1)
            // Case 4:
            // If `e1` is trivial and pure, then it is safe to inline.
            // Code size and runtime are not impacted, because only trivial expressions are inlined
            val wantToPostInline = isTrivialAndPure(e1, exp1.eff) && occur != DontInline
            if (wantToPostInline) {
              // If `e1` is to be inlined:
              // Add map `sym` to `e1` and return `e2` without constructing the let expression.
              val subst1 = subst0 + (sym -> SubstRange.DoneExp(e1))
              visitExp(exp2, subst1, inScopeSet0)
            } else {
              // Case 5:
              // If none of the previous cases pass, `sym` is not inlined. Return a let expression with the visited expressions
              // Code size and runtime are not impacted
              val inScopeSet = inScopeSet0 + (sym -> Definition.LetBound(e1, occur))
              val e2 = visitExp(exp2, subst0, inScopeSet)
              MonoAst.Expr.Let(sym, e1, e2, tpe, eff, loc)
            }
          }
        }

      case OccurrenceAst1.Expr.LocalDef(sym, fparams, exp1, exp2, tpe, eff, occur, loc) =>
        // TODO: Update this case if we want to inline
        // Current impl is just placeholder
        val fps = fparams.map(visitFormalParam)
        val e1 = visit(exp1)
        val e2 = visit(exp2)
        MonoAst.Expr.LocalDef(sym, fps, e1, e2, tpe, eff, loc)

      case OccurrenceAst1.Expr.Scope(sym, rvar, exp, tpe, eff, loc) =>
        val e = visit(exp)
        MonoAst.Expr.Scope(sym, rvar, e, tpe, eff, loc)

      case OccurrenceAst1.Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
        val e1 = visit(exp1)
        val e2 = visit(exp2)
        val e3 = visit(exp3)
        MonoAst.Expr.IfThenElse(e1, e2, e3, tpe, eff, loc)

      case OccurrenceAst1.Expr.Stm(exp1, exp2, tpe, eff, loc) =>
        // Case 1:
        // If `exp1` is pure, so it has no side effects, then it is safe to remove
        // Both code size and runtime are reduced
        if (isPure(exp1.eff)) {
          visit(exp2)
        } else {
          val e1 = visit(exp1)
          val e2 = visit(exp2)
          MonoAst.Expr.Stm(e1, e2, tpe, eff, loc)
        }

      case OccurrenceAst1.Expr.Discard(exp, eff, loc) =>
        val e = visit(exp)
        MonoAst.Expr.Discard(e, eff, loc)

      case OccurrenceAst1.Expr.Match(exp, rules, tpe, eff, loc) =>
        val e = visit(exp)
        val rs = rules.map {
          case OccurrenceAst1.MatchRule(pat, guard, exp) =>
            val p = visitPattern(pat)
            val g = guard.map(visit)
            val e = visit(exp)
            MonoAst.MatchRule(p, g, e)
        }
        MonoAst.Expr.Match(e, rs, tpe, eff, loc)

      case OccurrenceAst1.Expr.VectorLit(exps, tpe, eff, loc) =>
        val es = exps.map(visit)
        MonoAst.Expr.VectorLit(es, tpe, eff, loc)

      case OccurrenceAst1.Expr.VectorLoad(exp1, exp2, tpe, eff, loc) =>
        val e1 = visit(exp1)
        val e2 = visit(exp2)
        MonoAst.Expr.VectorLoad(e1, e2, tpe, eff, loc)

      case OccurrenceAst1.Expr.VectorLength(exp, loc) =>
        val e = visit(exp)
        MonoAst.Expr.VectorLength(e, loc)

      case OccurrenceAst1.Expr.Ascribe(exp, tpe, eff, loc) =>
        val e = visit(exp)
        MonoAst.Expr.Ascribe(e, tpe, eff, loc)

      case OccurrenceAst1.Expr.Cast(exp, declaredType, declaredEff, tpe, eff, loc) =>
        val e = visit(exp)
        MonoAst.Expr.Cast(e, declaredType, declaredEff, tpe, eff, loc)

      case OccurrenceAst1.Expr.TryCatch(exp, rules, tpe, eff, loc) =>
        val e = visit(exp)
        val rs = rules.map {
          case OccurrenceAst1.CatchRule(sym, clazz, exp) =>
            val e = visit(exp)
            MonoAst.CatchRule(sym, clazz, e)
        }
        MonoAst.Expr.TryCatch(e, rs, tpe, eff, loc)

      case OccurrenceAst1.Expr.TryWith(exp, effUse, rules, tpe, eff, loc) =>
        val e = visit(exp)
        val rs = rules.map {
          case OccurrenceAst1.HandlerRule(op, fparams, exp) =>
            val fps = fparams.map(visitFormalParam)
            val e = visit(exp)
            MonoAst.HandlerRule(op, fps, e)
        }
        MonoAst.Expr.TryWith(e, effUse, rs, tpe, eff, loc)

      case OccurrenceAst1.Expr.Do(op, exps, tpe, eff, loc) =>
        val es = exps.map(visit)
        MonoAst.Expr.Do(op, es, tpe, eff, loc)

      case OccurrenceAst1.Expr.NewObject(name, clazz, tpe, eff, methods0, loc) =>
        val methods = methods0.map {
          case OccurrenceAst1.JvmMethod(ident, fparams, exp, retTpe, eff, loc) =>
            val fps = fparams.map(visitFormalParam)
            val e = visit(exp)
            MonoAst.JvmMethod(ident, fps, e, retTpe, eff, loc)
        }
        MonoAst.Expr.NewObject(name, clazz, tpe, eff, methods, loc)

    }

    visit(exp00)
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

  /**
    * Returns `true` if `def0` should be inlined.
    */
  private def canInlineDef(ctx: DefContext): Boolean = {
    val mayInline = ctx.occur != DontInline && !ctx.isSelfRecursive
    val shouldInline = ctx.isDirectCall ||
      ctx.occur == Once ||
      ctx.occur == OnceInAbstraction // May duplicate work?
    mayInline && shouldInline
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
  private def bindFormals(exp0: OccurrenceAst1.Expr, symbols: List[(OccurrenceAst1.FormalParam, Occur)], args: List[OutExpr], subst0: Subst)(implicit root: OccurrenceAst1.Root, flix: Flix): MonoAst.Expr = {
    def bind(syms: List[(OccurrenceAst1.FormalParam, Occur)], as: List[OutExpr], env: VarSubst): MonoAst.Expr = (syms, as) match {
      case ((_, occur) :: nextSymbols, e1 :: nextExpressions) if isDeadAndPure(occur, e1.eff) =>
        // If the parameter is unused and the argument is pure, then throw it away.
        bind(nextSymbols, nextExpressions, env)

      case ((_, occur) :: nextSymbols, e1 :: nextExpressions) if isDead(occur) =>
        // If the parameter is unused and the argument is NOT pure, then put it in a statement.
        val nextLet = bind(nextSymbols, nextExpressions, env)
        val eff = Type.mkUnion(e1.eff, nextLet.eff, e1.loc)
        MonoAst.Expr.Stm(e1, nextLet, exp0.tpe, eff, exp0.loc)

      case ((OccurrenceAst1.FormalParam(sym, _, _, _, _), _) :: nextSymbols, e1 :: nextExpressions) =>
        val freshVar = Symbol.freshVarSym(sym)
        val env1 = env + (sym -> freshVar)
        val nextLet = bind(nextSymbols, nextExpressions, env1)
        val eff = Type.mkUnion(e1.eff, nextLet.eff, e1.loc)
        MonoAst.Expr.Let(freshVar, e1, nextLet, exp0.tpe, eff, exp0.loc)

      case _ => applySubst(exp0, env)(subst0, root, flix)
    }

    bind(symbols, args, Map.empty)
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
    case MonoAst.Expr.ApplyAtomic(AtomicOp.Unary(_), exps, _, _, _) => exps.forall(isTrivialExp)
    case MonoAst.Expr.ApplyAtomic(AtomicOp.Binary(_), exps, _, _, _) => exps.forall(isTrivialExp)
    case MonoAst.Expr.ApplyAtomic(AtomicOp.Tag(_), exps, _, _, _) => exps.forall(isTrivialExp)
    case _ => false
  }

  /**
    * Substitute variables in `exp0` for new fresh variables in `env0`
    */
  private def applySubst(exp0: OccurrenceAst1.Expr, varSubst: VarSubst)(implicit subst0: Subst, root: OccurrenceAst1.Root, flix: Flix): MonoAst.Expr = exp0 match {
    case OccurrenceAst1.Expr.Cst(cst, tpe, loc) =>
      MonoAst.Expr.Cst(cst, tpe, loc)

    case OccurrenceAst1.Expr.Var(sym, tpe, loc) =>
      subst0.get(sym) match {
        case Some(substExpr) => substExpr match {
          case SubstRange.SuspendedExp(exp) =>
            // We are inlining an abstraction that has captured `sym` from its environment.
            // However, if that is the case the OccurrenceAnalyzer has marked it as OnceInAbstraction
            // in which case the Inliner will NEVER produce a suspended exp for the binder.
            // We could visit `exp` here but it is better to throw an error since this should be a bug.
            throw InternalCompilerException("inlined unexpected suspended exp", exp.loc)
          case SubstRange.DoneExp(exp) => exp
        }
        case None => MonoAst.Expr.Var(varSubst.getOrElse(sym, sym), tpe, loc)
      }

    case OccurrenceAst1.Expr.Lambda((fparam, occur), exp, tpe, loc) =>
      val (fp, subst1) = applySubstFormalParam(fparam)
      val subst2 = varSubst ++ subst1
      val e = applySubst(exp, subst2)
      MonoAst.Expr.Lambda(fp, e, tpe, loc)

    case OccurrenceAst1.Expr.ApplyAtomic(op, exps, tpe, eff, loc) =>
      val es = exps.map(applySubst(_, varSubst))
      MonoAst.Expr.ApplyAtomic(op, es, tpe, eff, loc)

    case OccurrenceAst1.Expr.ApplyClo(exp, exps, tpe, eff, loc) =>
      val e = applySubst(exp, varSubst)
      val es = exps.map(applySubst(_, varSubst))
      MonoAst.Expr.ApplyClo(e, es, tpe, eff, loc)

    case OccurrenceAst1.Expr.ApplyDef(sym, exps, itpe, tpe, eff, loc) =>
      val es = exps.map(applySubst(_, varSubst))
      MonoAst.Expr.ApplyDef(sym, es, itpe, tpe, eff, loc)

    case OccurrenceAst1.Expr.ApplyLocalDef(sym, exps, tpe, eff, loc) =>
      val es = exps.map(applySubst(_, varSubst))
      MonoAst.Expr.ApplyLocalDef(varSubst.getOrElse(sym, sym), es, tpe, eff, loc)

    case OccurrenceAst1.Expr.Let(sym, exp1, exp2, tpe, eff, _, loc) =>
      val freshVar = Symbol.freshVarSym(sym)
      val env1 = varSubst + (sym -> freshVar)
      val e1 = applySubst(exp1, env1)
      val e2 = applySubst(exp2, env1)
      MonoAst.Expr.Let(freshVar, e1, e2, tpe, eff, loc)

    case OccurrenceAst1.Expr.LocalDef(sym, fparams, exp1, exp2, tpe, eff, _, loc) =>
      val freshVarSym = Symbol.freshVarSym(sym)
      val subst1 = varSubst + (sym -> freshVarSym)
      val e2 = applySubst(exp2, subst1) // e2 should not know about fparams
      val (fps, substs) = fparams.map(applySubstFormalParam).unzip
      val subst2 = subst1 ++ substs.flatten
      val e1 = applySubst(exp1, subst2)
      MonoAst.Expr.LocalDef(freshVarSym, fps, e1, e2, tpe, eff, loc)

    case OccurrenceAst1.Expr.Scope(sym, rvar, exp, tpe, eff, loc) =>
      val e = applySubst(exp, varSubst)
      MonoAst.Expr.Scope(sym, rvar, e, tpe, eff, loc)

    case OccurrenceAst1.Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
      val e1 = applySubst(exp1, varSubst)
      val e2 = applySubst(exp2, varSubst)
      val e3 = applySubst(exp3, varSubst)
      MonoAst.Expr.IfThenElse(e1, e2, e3, tpe, eff, loc)

    case OccurrenceAst1.Expr.Stm(exp1, exp2, tpe, eff, loc) =>
      val e1 = applySubst(exp1, varSubst)
      val e2 = applySubst(exp2, varSubst)
      MonoAst.Expr.Stm(e1, e2, tpe, eff, loc)

    case OccurrenceAst1.Expr.Discard(exp, eff, loc) =>
      val e = applySubst(exp, varSubst)
      MonoAst.Expr.Discard(e, eff, loc)

    case OccurrenceAst1.Expr.Match(exp, rules, tpe, eff, loc) =>
      val e = applySubst(exp, varSubst)
      val rs = rules.map {
        case OccurrenceAst1.MatchRule(pat, guard, exp) =>
          val (p, env1) = applySubstPattern(pat)
          val env2 = varSubst ++ env1
          val g = guard.map(applySubst(_, env2))
          val e = applySubst(exp, env2)
          MonoAst.MatchRule(p, g, e)
      }
      MonoAst.Expr.Match(e, rs, tpe, eff, loc)

    case OccurrenceAst1.Expr.VectorLit(exps, tpe, eff, loc) =>
      val es = exps.map(applySubst(_, varSubst))
      MonoAst.Expr.VectorLit(es, tpe, eff, loc)

    case OccurrenceAst1.Expr.VectorLoad(exp1, exp2, tpe, eff, loc) =>
      val e1 = applySubst(exp1, varSubst)
      val e2 = applySubst(exp2, varSubst)
      MonoAst.Expr.VectorLoad(e1, e2, tpe, eff, loc)

    case OccurrenceAst1.Expr.VectorLength(exp, loc) =>
      val e = applySubst(exp, varSubst)
      MonoAst.Expr.VectorLength(e, loc)

    case OccurrenceAst1.Expr.Ascribe(exp, tpe, eff, loc) =>
      val e = applySubst(exp, varSubst)
      MonoAst.Expr.Ascribe(e, tpe, eff, loc)

    case OccurrenceAst1.Expr.Cast(exp, declaredType, declaredEff, tpe, eff, loc) =>
      val e = applySubst(exp, varSubst)
      MonoAst.Expr.Cast(e, declaredType, declaredEff, tpe, eff, loc)

    case OccurrenceAst1.Expr.TryCatch(exp, rules, tpe, eff, loc) =>
      val e = applySubst(exp, varSubst)
      val rs = rules.map {
        case OccurrenceAst1.CatchRule(sym, clazz, exp) =>
          val freshVar = Symbol.freshVarSym(sym)
          val env1 = varSubst + (sym -> freshVar)
          val e = applySubst(exp, env1)
          MonoAst.CatchRule(freshVar, clazz, e)
      }
      MonoAst.Expr.TryCatch(e, rs, tpe, eff, loc)

    case OccurrenceAst1.Expr.TryWith(exp, effUse, rules, tpe, eff, loc) =>
      val e = applySubst(exp, varSubst)
      val rs = rules.map {
        case OccurrenceAst1.HandlerRule(op, fparams, exp) =>
          val (fps, substs) = fparams.map(applySubstFormalParam).unzip
          val subst1 = varSubst ++ substs.flatten
          val e = applySubst(exp, subst1)
          MonoAst.HandlerRule(op, fps, e)
      }
      MonoAst.Expr.TryWith(e, effUse, rs, tpe, eff, loc)

    case OccurrenceAst1.Expr.Do(op, exps, tpe, eff, loc) =>
      val es = exps.map(applySubst(_, varSubst))
      MonoAst.Expr.Do(op, es, tpe, eff, loc)

    case OccurrenceAst1.Expr.NewObject(name, clazz, tpe, eff, methods0, loc) =>
      val methods = methods0.map {
        case OccurrenceAst1.JvmMethod(ident, fparams, exp, retTpe, eff, loc) =>
          val (fps, substs) = fparams.map(applySubstFormalParam).unzip
          val subst1 = varSubst ++ substs.flatten
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
