/*
 * Copyright 2025 Jakob Schneider Villumsen
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

package ca.uwaterloo.flix.language.phase.optimizer

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.MonoAst.Expr
import ca.uwaterloo.flix.language.ast.shared.{BoundBy, Scope}
import ca.uwaterloo.flix.language.ast.{MonoAst, SourceLocation, Symbol, Type}
import ca.uwaterloo.flix.language.dbg.AstPrinter.DebugMonoAst
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps}

import scala.collection.mutable

/**
  * Rewrites functions that recursively call themselves in tail-position to
  * non-recursive functions with a recursive _al def.
  */
object RecursionRewriter {

  def run(root: MonoAst.Root)(implicit flix: Flix): MonoAst.Root = flix.phase("RecursionRewriter") {
    val newDefs = ParOps.parMapValues(root.defs)(visitDef)
    root.copy(defs = newDefs)
  }

  private def visitDef(defn: MonoAst.Def)(implicit flix: Flix): MonoAst.Def = {
    // 1. Check that every recursive call is in tail position
    //    Return a set of alive parameters, i.e, function parameters that are changed in a recursive call (if it is an Expr.Var with the same symbol, then it is dead).
    implicit val ctx: LocalContext = LocalContext.mk()
    val isRewritable = checkTailPosition(defn.exp, tailPos = true)(defn.sym, defn.spec.fparams, ctx, flix)
    if (!isRewritable) {
      return defn
    }

    // 2. Rewrite eligible functions
    // 2.1 Create a substitution from the function symbol and alive parameters to fresh symbols (maybe this can be created during step 2)
    // 2.2 Copy the function body, visit and apply the substitution and rewrite nodes. Any ApplyDef expr becomes an ApplyLocalDef expr.
    val freshLocalDefSym = Symbol.freshVarSym(defn.sym.text, BoundBy.LocalDef, defn.sym.loc)(Scope.Top, flix)
    val varSubst = ctx.alive.toMap
    val subst = Subst.from(defn.sym, freshLocalDefSym, varSubst)
    val localDef = rewriteExp(defn.exp)(subst, defn.spec.fparams)

    // 2.3 Replace the original function body with a LocalDef declaration that has the body from 3.2, followed by an ApplyLocalDef expr.
    val aliveParams = defn.spec.fparams.filter(fp => varSubst.contains(fp.sym))
    val body = mkLocalDefExpr(subst, localDef, freshLocalDefSym, aliveParams, defn.spec.retTpe, defn.spec.eff)
    defn.copy(exp = body)
  }

  private def checkTailPosition(exp0: MonoAst.Expr, tailPos: Boolean)(implicit sym0: Symbol.DefnSym, fparams: List[MonoAst.FormalParam], ctx: LocalContext, flix: Flix): Boolean = exp0 match {
    case Expr.Cst(_, _, _) =>
      true

    case Expr.Var(sym, _, _) =>
      if (fparams.map(_.sym).contains(sym)) {
        ctx.alive.getOrElseUpdate(sym, Symbol.freshVarSym(sym))
      }
      true

    case Expr.Lambda(_, exp, _, _) =>
      checkTailPosition(exp, tailPos = false)

    case Expr.ApplyAtomic(_, exps, _, _, _) =>
      exps.forall(checkTailPosition(_, tailPos = false))

    case Expr.ApplyClo(exp1, exp2, _, _, _) =>
      checkTailPosition(exp1, tailPos) &&
        checkTailPosition(exp2, tailPos = false)

    case Expr.ApplyDef(sym, exps, _, _, _, _) =>
      if (sym != sym0) {
        // Not recursive call
        return exps.forall(checkTailPosition(_, tailPos = false))
      }

      // Recursive Call
      if (!tailPos) {
        // Not tailpos => abort!
        return false
      }

      // Recursive call in tailpos
      // Check alive parameters
      exps.zip(fparams)
        .filter {
          case (exp, fparam) => aliveParam(exp, fparam)
        }.forall {
          case (exp, _) => checkTailPosition(exp, tailPos = false)
        }

    case Expr.ApplyLocalDef(_, exps, _, _, _) =>
      exps.forall(checkTailPosition(_, tailPos = false))

    case Expr.Let(_, exp1, exp2, _, _, _) =>
      checkTailPosition(exp1, tailPos = false) &&
        checkTailPosition(exp2, tailPos)


    case Expr.LocalDef(_, _, exp1, exp2, _, _, _) =>
      checkTailPosition(exp1, tailPos = false) &&
        checkTailPosition(exp2, tailPos)

    case Expr.Scope(_, _, exp, _, _, _) =>
      checkTailPosition(exp, tailPos)

    case Expr.IfThenElse(exp1, exp2, exp3, _, _, _) =>
      checkTailPosition(exp1, tailPos = false) &&
        checkTailPosition(exp2, tailPos) &&
        checkTailPosition(exp3, tailPos)

    case Expr.Stm(exp1, exp2, _, _, _) =>
      checkTailPosition(exp1, tailPos = false) &&
        checkTailPosition(exp2, tailPos)

    case Expr.Discard(exp, _, _) =>
      checkTailPosition(exp, tailPos)

    case Expr.Match(exp1, rules, _, _, _) =>
      checkTailPosition(exp1, tailPos = false) &&
        rules.forall {
          case MonoAst.MatchRule(_, guard, exp2) =>
            guard.forall(checkTailPosition(_, tailPos = false)) &&
              checkTailPosition(exp2, tailPos)
        }

    case Expr.VectorLit(exps, _, _, _) =>
      exps.forall(checkTailPosition(_, tailPos = false))

    case Expr.VectorLoad(exp1, exp2, _, _, _) =>
      checkTailPosition(exp1, tailPos = false) &&
        checkTailPosition(exp2, tailPos = false)

    case Expr.VectorLength(exp, _) =>
      checkTailPosition(exp, tailPos = false)

    case Expr.Ascribe(exp, _, _, _) => // Is this erased???
      checkTailPosition(exp, tailPos)

    case Expr.Cast(exp, _, _, _, _, _) => // Is this erased???
      checkTailPosition(exp, tailPos)

    case Expr.TryCatch(exp1, rules, _, _, _) =>
      checkTailPosition(exp1, tailPos = false) &&
        rules.forall {
          case MonoAst.CatchRule(_, _, exp2) =>
            checkTailPosition(exp2, tailPos)
        }
    case Expr.RunWith(exp1, _, rules, _, _, _) =>
      checkTailPosition(exp1, tailPos = false) &&
        rules.forall {
          case MonoAst.HandlerRule(_, _, exp2) =>
            checkTailPosition(exp2, tailPos)
        }

    case Expr.Do(_, exps, _, _, _) =>
      exps.forall(checkTailPosition(_, tailPos))

    case Expr.NewObject(_, _, _, _, methods, _) =>
      methods.forall {
        case MonoAst.JvmMethod(_, _, exp, _, _, _) =>
          checkTailPosition(exp, tailPos = false)
      }
  }

  private def rewriteExp(expr0: MonoAst.Expr)(implicit subst: Subst, fparams0: List[MonoAst.FormalParam]): MonoAst.Expr = expr0 match {
    case Expr.Cst(_, _, _) =>
      expr0

    case Expr.Var(sym, tpe, loc) =>
      Expr.Var(subst(sym), tpe, loc)

    case Expr.Lambda(fparam, exp, tpe, loc) =>
      val e = rewriteExp(exp)
      Expr.Lambda(fparam, e, tpe, loc)

    case Expr.ApplyAtomic(op, exps, tpe, eff, loc) =>
      val es = exps.map(rewriteExp)
      Expr.ApplyAtomic(op, es, tpe, eff, loc)

    case Expr.ApplyClo(exp1, exp2, tpe, eff, loc) =>
      val e1 = rewriteExp(exp1)
      val e2 = rewriteExp(exp2)
      Expr.ApplyClo(e1, e2, tpe, eff, loc)

    case Expr.ApplyDef(sym, exps, itpe, tpe, eff, loc) =>
      subst(sym) match {
        case None => // It is not a recursive call
          Expr.ApplyDef(sym, exps, itpe, tpe, eff, loc)

        case Some(localDefSym) =>
          val es = exps.zipWithIndex.filter {
            case (_, i) => subst.isAlive(i, fparams0)
          }.map {
            case (e, _) => rewriteExp(e)
          }
          Expr.ApplyLocalDef(localDefSym, es, tpe, eff, loc)
      }

    case Expr.ApplyLocalDef(sym, exps, tpe, eff, loc) =>
      val es = exps.map(rewriteExp)
      Expr.ApplyLocalDef(sym, es, tpe, eff, loc)

    case Expr.Let(sym, exp1, exp2, tpe, eff, loc) =>
      val e1 = rewriteExp(exp1)
      val e2 = rewriteExp(exp2)
      Expr.Let(sym, e1, e2, tpe, eff, loc)

    case Expr.LocalDef(sym, fparams, exp1, exp2, tpe, eff, loc) =>
      val e1 = rewriteExp(exp1)
      val e2 = rewriteExp(exp2)
      Expr.LocalDef(sym, fparams, e1, e2, tpe, eff, loc)

    case Expr.Scope(sym, regSym, exp, tpe, eff, loc) =>
      val e = rewriteExp(exp)
      Expr.Scope(sym, regSym, e, tpe, eff, loc)

    case Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
      val e1 = rewriteExp(exp1)
      val e2 = rewriteExp(exp2)
      val e3 = rewriteExp(exp3)
      Expr.IfThenElse(e1, e2, e3, tpe, eff, loc)

    case Expr.Stm(exp1, exp2, tpe, eff, loc) =>
      val e1 = rewriteExp(exp1)
      val e2 = rewriteExp(exp2)
      Expr.Stm(e1, e2, tpe, eff, loc)

    case Expr.Discard(exp, eff, loc) =>
      val e = rewriteExp(exp)
      Expr.Discard(e, eff, loc)

    case Expr.Match(exp1, rules, tpe, eff, loc) =>
      val e1 = rewriteExp(exp1)
      val rs = rules.map {
        case MonoAst.MatchRule(pat, guard, exp2) =>
          val g = guard.map(rewriteExp)
          val e2 = rewriteExp(exp2)
          MonoAst.MatchRule(pat, g, e2)
      }
      Expr.Match(e1, rs, tpe, eff, loc)

    case Expr.VectorLit(exps, tpe, eff, loc) =>
      val es = exps.map(rewriteExp)
      Expr.VectorLit(es, tpe, eff, loc)

    case Expr.VectorLoad(exp1, exp2, tpe, eff, loc) =>
      val e1 = rewriteExp(exp1)
      val e2 = rewriteExp(exp2)
      Expr.VectorLoad(e1, e2, tpe, eff, loc)

    case Expr.VectorLength(exp, loc) =>
      val e = rewriteExp(exp)
      Expr.VectorLength(e, loc)

    case Expr.Ascribe(exp, tpe, eff, loc) =>
      val e = rewriteExp(exp)
      Expr.Ascribe(e, tpe, eff, loc)

    case Expr.Cast(exp, declaredType, declaredEff, tpe, eff, loc) =>
      val e = rewriteExp(exp)
      Expr.Cast(e, declaredType, declaredEff, tpe, eff, loc)

    case Expr.TryCatch(exp1, rules, tpe, eff, loc) =>
      val e1 = rewriteExp(exp1)
      val rs = rules.map {
        case MonoAst.CatchRule(sym, clazz, exp2) =>
          val e2 = rewriteExp(exp2)
          MonoAst.CatchRule(sym, clazz, e2)
      }
      Expr.TryCatch(e1, rs, tpe, eff, loc)

    case Expr.RunWith(exp1, effUse, rules, tpe, eff, loc) =>
      val e1 = rewriteExp(exp1)
      val rs = rules.map {
        case MonoAst.HandlerRule(op, fparams, exp2) =>
          val e2 = rewriteExp(exp2)
          MonoAst.HandlerRule(op, fparams, e2)
      }
      Expr.RunWith(e1, effUse, rs, tpe, eff, loc)

    case Expr.Do(op, exps, tpe, eff, loc) =>
      val es = exps.map(rewriteExp)
      Expr.Do(op, es, tpe, eff, loc)

    case Expr.NewObject(name, clazz, tpe, eff1, methods, loc1) =>
      val ms = methods.map {
        case MonoAst.JvmMethod(ident, fparams, exp, retTpe, eff2, loc2) =>
          val e = rewriteExp(exp)
          MonoAst.JvmMethod(ident, fparams, e, retTpe, eff2, loc2)
      }
      Expr.NewObject(name, clazz, tpe, eff1, ms, loc1)

  }

  /** Returns `false` if `expr` is a var with same symbol as `fparam`. Returns `true` otherwise. */
  private def aliveParam(expr: MonoAst.Expr, fparam: MonoAst.FormalParam): Boolean = expr match {
    case Expr.Var(sym, _, _) => sym != fparam.sym
    case _ => true
  }

  /**
    * Returns a [[Expr.LocalDef]] that is immediately applied after its declaration. Only the parameters that are alive are declared and applied.
    * Dead parameters are captured from the containing function.
    *
    * @param subst       The variable substitution.
    * @param body        The body of the local def to be created.
    * @param localDefSym The symbol of the local def to be created.
    * @param aliveParams The list of parameters that are alive in the body.
    * @param tpe         The return type of the local def, i.e., the type obtained by applying the local def.
    * @param eff         The effect the local def, i.e., the effect obtained by applying the local def.
    */
  private def mkLocalDefExpr(subst: Subst, body: Expr, localDefSym: Symbol.VarSym, aliveParams: List[MonoAst.FormalParam], tpe: Type, eff: Type): Expr = {
    // Make ApplyLocalDef Expr
    val args = aliveParams.map(fp => Expr.Var(fp.sym, fp.tpe, fp.loc.asSynthetic))
    val applyLocalDef = Expr.ApplyLocalDef(localDefSym, args, tpe, eff, body.loc.asSynthetic)

    // Make LocalDef expr
    val params = aliveParams.map(fp => fp.copy(sym = subst(fp.sym), loc = fp.loc.asSynthetic))
    Expr.LocalDef(localDefSym, params, body, applyLocalDef, tpe, eff, body.loc.asSynthetic)
  }


  private object LocalContext {

    def mk(): LocalContext = new LocalContext(new mutable.HashMap())

  }

  private case class LocalContext(alive: mutable.HashMap[Symbol.VarSym, Symbol.VarSym])

  private object Subst {
    def from(old: Symbol.DefnSym, fresh: Symbol.VarSym, vars: Map[Symbol.VarSym, Symbol.VarSym]): Subst = Subst(old, fresh, vars)
  }

  private case class Subst(private val old: Symbol.DefnSym, private val fresh: Symbol.VarSym, private val vars: Map[Symbol.VarSym, Symbol.VarSym]) {

    def apply(sym: Symbol.DefnSym): Option[Symbol.VarSym] = {
      if (sym == old)
        Some(fresh)
      else
        None
    }

    def apply(sym: Symbol.VarSym): Symbol.VarSym = vars.get(sym) match {
      case Some(freshSym) => freshSym
      case None => sym
    }

    /**
      * Returns `true` if the fparam at index `i` is alive.
      *
      * @param i       The index of the fparam of which to check aliveness.
      * @param fparams The list of [[MonoAst.FormalParam]] to traverse.
      */
    def isAlive(i: Int, fparams: List[MonoAst.FormalParam]): Boolean = {
      fparams.drop(i) match {
        case fp :: _ => vars.contains(fp.sym)
        case Nil => throw InternalCompilerException("unexpected empty fparam", SourceLocation.Unknown)
      }
    }

  }

}
