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
import ca.uwaterloo.flix.language.ast.OccurrenceAst1.Occur.*
import ca.uwaterloo.flix.language.ast.Purity.Pure
import ca.uwaterloo.flix.language.ast.{AtomicOp, SimplifiedAst, OccurrenceAst1, Purity, Symbol}
import ca.uwaterloo.flix.util.{ParOps, Validation}

/**
  * The inliner optionally performs beta-reduction at call-sites.
  * TODO: Improve this documentation
  */
object Inliner1 {

  sealed trait Expr

  object Expr {
    case class SimplifiedExp(exp: SimplifiedAst.Expr) extends Expr

    case class OccurrenceExp(exp: OccurrenceAst1.Expr) extends Expr
  }

  /**
    * A function below the soft threshold is typically inlined.
    */
  private val SoftInlineThreshold: Int = 4

  /**
    * A function above the hard threshold is never inlined.
    */
  private val HardInlineThreshold: Int = 8

  /**
    * Returns `true` if `def0` should be inlined.
    */
  private def canInlineDef(def0: OccurrenceAst1.Def): Boolean = {
    val mayInline = def0.context.occur != DontInline && !def0.context.isSelfRecursive
    val belowSoft = def0.context.size < SoftInlineThreshold
    val belowHard = def0.context.size < HardInlineThreshold
    val shouldInline = belowSoft || (def0.context.isDirectCall && belowHard) || (def0.context.occur == Once && belowHard)
    mayInline && shouldInline
  }

  /**
    * Performs inlining on the given AST `root`.
    */
  def run(root: OccurrenceAst1.Root)(implicit flix: Flix): Validation[SimplifiedAst.Root, CompilationMessage] = {
    val defs = ParOps.parMapValues(root.defs)(d => visitDef(d)(flix, root))
    val effects = ParOps.parMapValues(root.effects)(visitEffect)

    Validation.success(SimplifiedAst.Root(defs, effects, root.entryPoint, root.reachable, root.sources))
  }

  /**
    * Performs expression inlining on the given definition `def0`.
    * Converts definition from [[OccurrenceAst1]] to [[SimplifiedAst]].
    */
  private def visitDef(def0: OccurrenceAst1.Def)(implicit flix: Flix, root: OccurrenceAst1.Root): SimplifiedAst.Def = {
    val convertedExp = visitExp(def0.exp, Map.empty)(root, flix)
    val fparams = def0.fparams.map {
      case (OccurrenceAst1.FormalParam(sym, mod, tpe, loc), _) => SimplifiedAst.FormalParam(sym, mod, tpe, loc)
    }
    SimplifiedAst.Def(def0.ann, def0.mod, def0.sym, fparams, convertedExp, def0.tpe, def0.purity, def0.loc)
  }

  private def visitEffect(effect: OccurrenceAst1.Effect): SimplifiedAst.Effect = effect match {
    case OccurrenceAst1.Effect(ann, mod, sym, ops0, loc) =>
      val ops = ops0.map(visitEffectOp)
      SimplifiedAst.Effect(ann, mod, sym, ops, loc)
  }

  private def visitEffectOp(op: OccurrenceAst1.Op): SimplifiedAst.Op = op match {
    case OccurrenceAst1.Op(sym, ann, mod, fparams0, tpe, purity, loc) =>
      val fparams = fparams0.map(visitFormalParam)
      SimplifiedAst.Op(sym, ann, mod, fparams, tpe, purity, loc)
  }

  /**
    * Performs inlining operations on the expression `exp0` from [[OccurrenceAst1.Expr]].
    * Returns a [[SimplifiedAst.Expr]]
    */
  private def visitExp(exp0: OccurrenceAst1.Expr, subst0: Map[Symbol.VarSym, Expr])(implicit root: OccurrenceAst1.Root, flix: Flix): SimplifiedAst.Expr = exp0 match { // TODO: Add local `visit` function that captures `subst0`
    case OccurrenceAst1.Expr.Cst(cst, tpe, loc) => SimplifiedAst.Expr.Cst(cst, tpe, loc)

    case OccurrenceAst1.Expr.Var(sym, tpe, loc) =>
      subst0.get(sym) match {
        // Case 1:
        // The variable `sym` is not in the substitution map and will not be inlined.
        case None => SimplifiedAst.Expr.Var(sym, tpe, loc)
        // Case 2:
        // The variable `sym` is in the substitution map. Replace `sym` with `e1`.
        case Some(e1) =>
          e1 match {
            // If `e1` is a `LiftedExp` then `e1` has already been visited
            case Expr.SimplifiedExp(exp) => exp
            // If `e1` is a `OccurrenceExp` then `e1` has not been visited. Visit `e1`
            case Expr.OccurrenceExp(exp) => visitExp(exp, subst0)
          }
      }

    case OccurrenceAst1.Expr.Lambda(fparams, exp, tpe, loc) => ???

    case OccurrenceAst1.Expr.ApplyAtomic(op, exps, tpe, purity, loc) =>
      val es = exps.map(visitExp(_, subst0))
      op match {
        case AtomicOp.Untag(_) =>
          val List(e) = es
          // Inline expressions of the form Untag(Tag(e)) => e
          e match {
            case SimplifiedAst.Expr.ApplyAtomic(AtomicOp.Tag(_), innerExps, _, _, _) => innerExps.head
            case _ => SimplifiedAst.Expr.ApplyAtomic(op, es, tpe, purity, loc)
          }

        case _ => SimplifiedAst.Expr.ApplyAtomic(op, es, tpe, purity, loc)
      }

    case OccurrenceAst1.Expr.ApplyClo(exp, exps, tpe, purity, loc) =>
      val e = visitExp(exp, subst0)
      val es = exps.map(visitExp(_, subst0))
      e match {
        case SimplifiedAst.Expr.ApplyAtomic(AtomicOp.Closure(sym), closureArgs, _, _, _) =>
          val def1 = root.defs.apply(sym)
          // If `def1` is a single non-self call or
          // it is trivial
          // then inline the body of `def1`
          if (canInlineDef(def1)) {
            // Map for substituting formal parameters of a function with the closureArgs currently in scope
            bindFormals(def1.exp, def1.fparams, closureArgs ++ es, Map.empty)
          } else {
            SimplifiedAst.Expr.ApplyClo(e, es, tpe, purity, loc)
          }
        case _ => SimplifiedAst.Expr.ApplyClo(e, es, tpe, purity, loc)
      }

    case OccurrenceAst1.Expr.ApplyDef(sym, exps, tpe, purity, loc) =>
      val es = exps.map(visitExp(_, subst0))
      val def1 = root.defs.apply(sym)
      // If `def1` is a single non-self call or
      // it is trivial
      // then inline the body of `def1`
      if (canInlineDef(def1)) {
        bindFormals(def1.exp, def1.fparams, es, Map.empty)
      } else {
        SimplifiedAst.Expr.ApplyDef(sym, es, tpe, purity, loc)
      }

    case OccurrenceAst1.Expr.ApplyLocalDef(sym, exps, tpe, purity, loc) => ???

    case OccurrenceAst1.Expr.IfThenElse(exp1, exp2, exp3, tpe, purity, loc) =>
      val e1 = visitExp(exp1, subst0)
      val e2 = visitExp(exp2, subst0)
      val e3 = visitExp(exp3, subst0)
      SimplifiedAst.Expr.IfThenElse(e1, e2, e3, tpe, purity, loc)

    case OccurrenceAst1.Expr.Branch(exp, branches, tpe, purity, loc) =>
      val e = visitExp(exp, subst0)
      val bs = branches.map {
        case (sym, br) => sym -> visitExp(br, subst0)
      }
      SimplifiedAst.Expr.Branch(e, bs, tpe, purity, loc)

    case OccurrenceAst1.Expr.JumpTo(sym, tpe, purity, loc) => SimplifiedAst.Expr.JumpTo(sym, tpe, purity, loc)

    case OccurrenceAst1.Expr.Let(sym, exp1, exp2, occur, tpe, purity, loc) =>
      if (isDead(occur)) {
        if (Purity.isPure(exp1.purity)) {
          /// Case 1:
          /// If `sym` is never used (it is `Dead`)  and `exp1` is pure, so it has no side effects, then it is safe to remove `sym`
          /// Both code size and runtime are reduced
          visitExp(exp2, subst0)
        } else {
          /// Case 2:
          /// If `sym` is never used (it is `Dead`) so it is safe to make a Stmt.
          SimplifiedAst.Expr.Stm(visitExp(exp1, subst0), visitExp(exp2, subst0), tpe, purity, loc)
        }
      } else {
        /// Case 3:
        /// If `exp1` occurs once and it is pure, then it is safe to inline.
        /// There is a small decrease in code size and runtime.
        val wantToPreInline = isUsedOnceAndPure(occur, exp1.purity)
        if (wantToPreInline) {
          val subst1 = subst0 + (sym -> Expr.OccurrenceExp(exp1))
          visitExp(exp2, subst1)
        } else {
          val e1 = visitExp(exp1, subst0)
          /// Case 4:
          /// If `e1` is trivial and pure, then it is safe to inline.
          // Code size and runtime are not impacted, because only trivial expressions are inlined
          val wantToPostInline = isTrivialAndPure(e1, exp1.purity) && occur != DontInline
          if (wantToPostInline) {
            /// If `e1` is to be inlined:
            /// Add map `sym` to `e1` and return `e2` without constructing the let expression.
            val subst1 = subst0 + (sym -> Expr.SimplifiedExp(e1))
            visitExp(exp2, subst1)
          } else {
            /// Case 5:
            /// If none of the previous cases pass, `sym` is not inlined. Return a let expression with the visited expressions
            /// Code size and runtime are not impacted
            val e2 = visitExp(exp2, subst0)
            SimplifiedAst.Expr.Let(sym, e1, e2, tpe, purity, loc)
          }
        }
      }

    case OccurrenceAst1.Expr.LocalDef(sym, fparams, exp1, exp2, occur, tpe, purity, loc) =>
      // TODO: Update this case if we want to inline
      // Current impl is just placeholder
      val fps = fparams.map(visitFormalParam)
      val e1 = visitExp(exp1, subst0)
      val e2 = visitExp(exp2, subst0)
      SimplifiedAst.Expr.LocalDef(sym, fps, e1, e2, tpe, purity, loc)

    case OccurrenceAst1.Expr.Stmt(exp1, exp2, tpe, purity, loc) =>
      /// Case 1:
      /// If `exp1` is pure, so it has no side effects, then it is safe to remove
      /// Both code size and runtime are reduced
      if (exp1.purity == Purity.Pure) {
        visitExp(exp2, subst0)
      } else {
        val e1 = visitExp(exp1, subst0)
        val e2 = visitExp(exp2, subst0)
        SimplifiedAst.Expr.Stm(e1, e2, tpe, purity, loc)
      }

    case OccurrenceAst1.Expr.Scope(sym, exp, tpe, purity, loc) =>
      val e = visitExp(exp, subst0)
      SimplifiedAst.Expr.Scope(sym, e, tpe, purity, loc)

    case OccurrenceAst1.Expr.TryCatch(exp, rules, tpe, purity, loc) =>
      val e = visitExp(exp, subst0)
      val rs = rules.map {
        case OccurrenceAst1.CatchRule(sym, clazz, exp) =>
          val e = visitExp(exp, subst0)
          SimplifiedAst.CatchRule(sym, clazz, e)
      }
      SimplifiedAst.Expr.TryCatch(e, rs, tpe, purity, loc)

    case OccurrenceAst1.Expr.TryWith(exp, effUse, rules, tpe, purity, loc) =>
      val e = visitExp(exp, subst0)
      val rs = rules.map {
        case OccurrenceAst1.HandlerRule(op, fparams, exp) =>
          val fps = fparams.map(visitFormalParam)
          val e = visitExp(exp, subst0)
          SimplifiedAst.HandlerRule(op, fps, e)
      }
      SimplifiedAst.Expr.TryWith(e, effUse, rs, tpe, purity, loc)

    case OccurrenceAst1.Expr.Do(op, exps, tpe, purity, loc) =>
      val es = exps.map(visitExp(_, subst0))
      SimplifiedAst.Expr.Do(op, es, tpe, purity, loc)

    case OccurrenceAst1.Expr.NewObject(name, clazz, tpe, purity, methods0, loc) =>
      val methods = methods0.map {
        case OccurrenceAst1.JvmMethod(ident, fparams, clo, retTpe, purity, loc) =>
          val f = fparams.map {
            case OccurrenceAst1.FormalParam(sym, mod, tpe, loc) => SimplifiedAst.FormalParam(sym, mod, tpe, loc)
          }
          val c = visitExp(clo, subst0)
          SimplifiedAst.JvmMethod(ident, f, c, retTpe, purity, loc)
      }
      SimplifiedAst.Expr.NewObject(name, clazz, tpe, purity, methods, loc)

  }

  /**
    * Checks if `occur` is Dead.
    */
  private def isDead(occur: OccurrenceAst1.Occur): Boolean = occur match {
    case Dead => true
    case _ => false
  }

  /**
    * Checks if `occur` is Once and `purity` is Pure
    */
  private def isUsedOnceAndPure(occur: OccurrenceAst1.Occur, purity: Purity): Boolean = (occur, purity) match {
    case (Once, Purity.Pure) => true
    case _ => false
  }

  /**
    * Checks if `exp0` is trivial and `purity` is pure
    */
  private def isTrivialAndPure(exp0: SimplifiedAst.Expr, purity: Purity): Boolean = purity match {
    case Purity.Pure => isTrivialExp(exp0)
    case _ => false
  }

  /**
    * Checks if `occur` is dead and `exp` is pure.
    */
  private def isDeadAndPure(occur: OccurrenceAst1.Occur, exp: SimplifiedAst.Expr): Boolean = (occur, exp.purity) match {
    case (Dead, Pure) => true
    case _ => false
  }

  /**
    * Recursively bind each argument in `args` to a let-expression with a fresh symbol
    * Add corresponding symbol from `symbols` to substitution map `env0`, mapping old symbols to fresh symbols.
    * Substitute variables in `exp0` via the filled substitution map `env0`
    */
  private def bindFormals(exp0: OccurrenceAst1.Expr, symbols: List[(OccurrenceAst1.FormalParam, OccurrenceAst1.Occur)], args: List[SimplifiedAst.Expr], env0: Map[Symbol.VarSym, Symbol.VarSym])(implicit root: OccurrenceAst1.Root, flix: Flix): SimplifiedAst.Expr = {
    (symbols, args) match {
      case ((_, occur) :: nextSymbols, e1 :: nextExpressions) if isDeadAndPure(occur, e1) =>
        // if the parameter is unused and the argument is pure, then throw it away.
        bindFormals(exp0, nextSymbols, nextExpressions, env0)
      case ((_, occur) :: nextSymbols, e1 :: nextExpressions) if isDead(occur) =>
        // if the parameter is unused and the argument is NOT pure, then put it in a statement.
        val nextLet = bindFormals(exp0, nextSymbols, nextExpressions, env0)
        val purity = Purity.combine(e1.purity, nextLet.purity)
        SimplifiedAst.Expr.Stm(e1, nextLet, exp0.tpe, purity, exp0.loc)
      case ((formal, _) :: nextSymbols, e1 :: nextExpressions) =>
        val freshVar = Symbol.freshVarSym(formal.sym)
        val env1 = env0 + (formal.sym -> freshVar)
        val nextLet = bindFormals(exp0, nextSymbols, nextExpressions, env1)
        val purity = Purity.combine(e1.purity, nextLet.purity)
        SimplifiedAst.Expr.Let(freshVar, e1, nextLet, exp0.tpe, purity, exp0.loc)
      case _ => applySubst(exp0, env0)
    }
  }

  /**
    * Returns `true` if `exp0` is considered a trivial expression.
    *
    * An expression is trivial if:
    * It is either a literal (float, string, int, bool, unit), or it is a variable.
    *
    * A pure and trivial expression can always be inlined even without duplicating work.
    */
  private def isTrivialExp(exp0: SimplifiedAst.Expr): Boolean = exp0 match {
    // TODO: Add recursive case for Tag, i.e., if its args are trivial, then it is trivial
    case SimplifiedAst.Expr.Cst(_, _, _) => true
    case SimplifiedAst.Expr.Var(_, _, _) => true
    case _ => false
  }

  /**
    * Substitute variables in `exp0` for new fresh variables in `env0`
    */
  private def applySubst(exp0: OccurrenceAst1.Expr, env0: Map[Symbol.VarSym, Symbol.VarSym])(implicit root: OccurrenceAst1.Root, flix: Flix): SimplifiedAst.Expr = exp0 match {
    case OccurrenceAst1.Expr.Cst(cst, tpe, loc) => SimplifiedAst.Expr.Cst(cst, tpe, loc)

    case OccurrenceAst1.Expr.Var(sym, tpe, loc) => SimplifiedAst.Expr.Var(env0.getOrElse(sym, sym), tpe, loc)

    case OccurrenceAst1.Expr.Lambda(fparams, exp, tpe, loc) =>
      val freshFps = fparams.map {
        case OccurrenceAst1.FormalParam(sym, mod, tpe, loc) =>
          val newSym = Symbol.freshVarSym(sym)
          sym -> SimplifiedAst.FormalParam(newSym, mod, tpe, loc)
      }
      val fps = freshFps.map(_._2)
      val fpsEnv = freshFps.map {
        case (sym, fp) => sym -> fp.sym
      }.toMap
      val env1 = env0 ++ fpsEnv
      val e = applySubst(exp, env1)
      SimplifiedAst.Expr.Lambda(fps, e, tpe, loc)

    case OccurrenceAst1.Expr.ApplyAtomic(op, exps, tpe, purity, loc) =>
      val es = exps.map(applySubst(_, env0))
      SimplifiedAst.Expr.ApplyAtomic(op, es, tpe, purity, loc)

    case OccurrenceAst1.Expr.ApplyClo(exp, exps, tpe, purity, loc) =>
      val e = applySubst(exp, env0)
      val es = exps.map(applySubst(_, env0))
      SimplifiedAst.Expr.ApplyClo(e, es, tpe, purity, loc)

    case OccurrenceAst1.Expr.ApplyDef(sym, exps, tpe, purity, loc) =>
      val es = exps.map(applySubst(_, env0))
      SimplifiedAst.Expr.ApplyDef(sym, es, tpe, purity, loc)

    case OccurrenceAst1.Expr.ApplyLocalDef(sym, exps, tpe, purity, loc) =>
      val es = exps.map(applySubst(_, env0))
      SimplifiedAst.Expr.ApplyLocalDef(env0.getOrElse(sym, sym), es, tpe, purity, loc)

    case OccurrenceAst1.Expr.IfThenElse(exp1, exp2, exp3, tpe, purity, loc) =>
      val e1 = applySubst(exp1, env0)
      val e2 = applySubst(exp2, env0)
      val e3 = applySubst(exp3, env0)
      SimplifiedAst.Expr.IfThenElse(e1, e2, e3, tpe, purity, loc)

    case OccurrenceAst1.Expr.Branch(exp, branches, tpe, purity, loc) =>
      val e = applySubst(exp, env0)
      val bs = branches.map {
        case (sym, br) => sym -> applySubst(br, env0)
      }
      SimplifiedAst.Expr.Branch(e, bs, tpe, purity, loc)

    case OccurrenceAst1.Expr.JumpTo(sym, tpe, purity, loc) => SimplifiedAst.Expr.JumpTo(sym, tpe, purity, loc)

    case OccurrenceAst1.Expr.Let(sym, exp1, exp2, _, tpe, purity, loc) =>
      val freshVar = Symbol.freshVarSym(sym)
      val env1 = env0 + (sym -> freshVar)
      val e1 = applySubst(exp1, env1)
      val e2 = applySubst(exp2, env1)
      SimplifiedAst.Expr.Let(freshVar, e1, e2, tpe, purity, loc)

    case OccurrenceAst1.Expr.LocalDef(sym, fparams, exp1, exp2, occur, tpe, purity, loc) =>
      val newSym = Symbol.freshVarSym(sym)
      val freshFps = fparams.map {
        case OccurrenceAst1.FormalParam(sym, mod, tpe, loc) =>
          val newSym = Symbol.freshVarSym(sym)
          sym -> SimplifiedAst.FormalParam(newSym, mod, tpe, loc)
      }
      val fps = freshFps.map(_._2)
      val fpsEnv = freshFps.map {
        case (sym, fp) => sym -> fp.sym
      }.toMap
      val env1 = env0 + (sym -> newSym)
      val e1 = applySubst(exp1, env1 ++ fpsEnv)
      val e2 = applySubst(exp2, env1)
      SimplifiedAst.Expr.LocalDef(newSym, fps, e1, e2, tpe, purity, loc)

    case OccurrenceAst1.Expr.Stmt(exp1, exp2, tpe, purity, loc) =>
      val e1 = applySubst(exp1, env0)
      val e2 = applySubst(exp2, env0)
      SimplifiedAst.Expr.Stm(e1, e2, tpe, purity, loc)

    case OccurrenceAst1.Expr.Scope(sym, exp, tpe, purity, loc) =>
      val e = applySubst(exp, env0)
      SimplifiedAst.Expr.Scope(sym, e, tpe, purity, loc)

    case OccurrenceAst1.Expr.TryCatch(exp, rules, tpe, purity, loc) =>
      val e = applySubst(exp, env0)
      val rs = rules.map {
        case OccurrenceAst1.CatchRule(sym, clazz, exp) =>
          val freshVar = Symbol.freshVarSym(sym)
          val env1 = env0 + (sym -> freshVar)
          val e = applySubst(exp, env1)
          SimplifiedAst.CatchRule(freshVar, clazz, e)
      }
      SimplifiedAst.Expr.TryCatch(e, rs, tpe, purity, loc)

    case OccurrenceAst1.Expr.TryWith(exp, effUse, rules, tpe, purity, loc) =>
      val e = applySubst(exp, env0)
      val rs = rules.map {
        case OccurrenceAst1.HandlerRule(op, fparams, exp) =>
          val fps = fparams.map(visitFormalParam)
          val e = applySubst(exp, env0)
          SimplifiedAst.HandlerRule(op, fps, e)
      }
      SimplifiedAst.Expr.TryWith(e, effUse, rs, tpe, purity, loc)

    case OccurrenceAst1.Expr.Do(op, exps, tpe, purity, loc) =>
      val es = exps.map(applySubst(_, env0))
      SimplifiedAst.Expr.Do(op, es, tpe, purity, loc)

    case OccurrenceAst1.Expr.NewObject(name, clazz, tpe, purity, methods0, loc) =>
      val methods = methods0.map {
        case OccurrenceAst1.JvmMethod(ident, fparams, clo, retTpe, purity, loc) =>
          val f = fparams.map {
            case OccurrenceAst1.FormalParam(sym, mod, tpe, loc) => SimplifiedAst.FormalParam(sym, mod, tpe, loc)
          }
          val c = applySubst(clo, env0)
          SimplifiedAst.JvmMethod(ident, f, c, retTpe, purity, loc)
      }
      SimplifiedAst.Expr.NewObject(name, clazz, tpe, purity, methods, loc)

  }

  /**
    * Translates the given formal parameter `fparam` from [[OccurrenceAst1.FormalParam]] into a [[SimplifiedAst.FormalParam]].
    */
  private def visitFormalParam(fparam: OccurrenceAst1.FormalParam): SimplifiedAst.FormalParam = fparam match {
    case OccurrenceAst1.FormalParam(sym, mod, tpe, loc) => SimplifiedAst.FormalParam(sym, mod, tpe, loc)
  }

}
