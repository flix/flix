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
import ca.uwaterloo.flix.language.ast.MonoAst.{Expr, empty}
import ca.uwaterloo.flix.language.ast.shared.{BoundBy, Scope}
import ca.uwaterloo.flix.language.ast.{MonoAst, SourceLocation, Symbol, Type}
import ca.uwaterloo.flix.language.dbg.AstPrinter.DebugMonoAst
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps}

import scala.collection.mutable

/**
  * Rewrites functions that recursively call themselves in tail-position to
  * non-recursive functions with a recursive local def that capture constant
  * parameters.
  *
  * A constant parameter is a parameter that is never updated or changed in
  * a recursive call.
  *
  * Example:
  *
  * {{{
  *   def lastMap(f: a -> b, l: List[a]): Option[b] = match l {
  *       case Nil      => None
  *       case x :: Nil => Some(f(x))
  *       case _ :: xs  => lastMap(f, xs)
  *   }
  * }}}
  *
  * Is rewritten to:
  *
  * {{{
  *   def lastMap(f: a -> b, l: List[a]): Option[b] =
  *       def lastMapLoop(l1) = match l1 {
  *           case Nil      => None
  *           case x :: Nil => Some(f(x))
  *           case _ :: xs  => lastMapLoop(xs)
  *       };
  *       lastMapLoop(l)
  * }}}
  */
object RecursionRewriter {

  def run(root: MonoAst.Root)(implicit flix: Flix): MonoAst.Root = flix.phase("RecursionRewriter") {
    val newDefs = ParOps.parMapValues(root.defs)(visitDef)
    root.copy(defs = newDefs)
  }

  private def visitDef(defn: MonoAst.Def)(implicit flix: Flix): MonoAst.Def = {
    implicit val ctx: LocalContext = LocalContext.mk()
    if (isRewritable(defn))
      rewriteDefn(defn)
    else
      defn
  }

  private def visitExp(exp0: MonoAst.Expr, tailPos: TailPosition)(implicit sym0: Symbol.DefnSym, ctx: LocalContext, flix: Flix): Unit = exp0 match {
    case Expr.Cst(_, _, _) =>

    case Expr.Var(_, _, _) =>

    case Expr.Lambda(_, exp, _, _) =>
      visitExp(exp, tailPos = TailPosition.NonTail)

    case Expr.ApplyAtomic(_, exps, _, _, _) =>
      exps.foreach(visitExp(_, tailPos = TailPosition.NonTail))

    case Expr.ApplyClo(exp1, exp2, _, _, _) =>
      visitExp(exp1, tailPos)
      visitExp(exp2, tailPos = TailPosition.NonTail)

    case Expr.ApplyDef(sym, exps, _, _, _, _) =>
      // Check for recursion
      if (sym == sym0) {
        val applyExp = exp0.asInstanceOf[Expr.ApplyDef]
        ctx.selfTailCalls.addOne((applyExp, tailPos))
      }
      exps.foreach(visitExp(_, tailPos = TailPosition.NonTail))

    case Expr.ApplyLocalDef(_, exps, _, _, _) =>
      exps.foreach(visitExp(_, tailPos = TailPosition.NonTail))

    case Expr.Let(_, exp1, exp2, _, _, _) =>
      visitExp(exp1, tailPos = TailPosition.NonTail)
      visitExp(exp2, tailPos)

    case Expr.LocalDef(_, _, exp1, exp2, _, _, _) =>
      visitExp(exp1, tailPos = TailPosition.NonTail)
      visitExp(exp2, tailPos)

    case Expr.Scope(_, _, exp, _, _, _) =>
      visitExp(exp, tailPos)

    case Expr.IfThenElse(exp1, exp2, exp3, _, _, _) =>
      visitExp(exp1, tailPos = TailPosition.NonTail)
      visitExp(exp2, tailPos)
      visitExp(exp3, tailPos)

    case Expr.Stm(exp1, exp2, _, _, _) =>
      visitExp(exp1, tailPos = TailPosition.NonTail)
      visitExp(exp2, tailPos)

    case Expr.Discard(exp, _, _) =>
      visitExp(exp, tailPos)

    case Expr.Match(exp1, rules, _, _, _) =>
      visitExp(exp1, tailPos = TailPosition.NonTail)
      rules.foreach {
        case MonoAst.MatchRule(_, guard, exp2) =>
          guard.foreach(visitExp(_, tailPos = TailPosition.NonTail))
          visitExp(exp2, tailPos)
      }

    case Expr.VectorLit(exps, _, _, _) =>
      exps.foreach(visitExp(_, tailPos = TailPosition.NonTail))

    case Expr.VectorLoad(exp1, exp2, _, _, _) =>
      visitExp(exp1, tailPos = TailPosition.NonTail)
      visitExp(exp2, tailPos = TailPosition.NonTail)

    case Expr.VectorLength(exp, _) =>
      visitExp(exp, tailPos = TailPosition.NonTail)

    case Expr.Ascribe(exp, _, _, _) => // TODO: Is this erased???
      visitExp(exp, tailPos)

    case Expr.Cast(exp, _, _, _, _, _) => // TODO: Is this erased???
      visitExp(exp, tailPos)

    case Expr.TryCatch(exp1, rules, _, _, _) =>
      visitExp(exp1, tailPos = TailPosition.NonTail)
      rules.foreach(rule => visitExp(rule.exp, tailPos))

    case Expr.RunWith(exp1, _, rules, _, _, _) =>
      visitExp(exp1, tailPos = TailPosition.NonTail)
      rules.foreach(rule => visitExp(rule.exp, tailPos))

    case Expr.Do(_, exps, _, _, _) =>
      exps.foreach(visitExp(_, tailPos))

    case Expr.NewObject(_, _, _, _, methods, _) =>
      methods.foreach(m => visitExp(m.exp, tailPos = TailPosition.NonTail))
  }

  private def rewriteExp(expr0: MonoAst.Expr)(implicit subst: Substitution, fparams0: List[(MonoAst.FormalParam, ParamKind)]): MonoAst.Expr = expr0 match {
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
          val es = exps.map(rewriteExp)
          Expr.ApplyDef(sym, es, itpe, tpe, eff, loc)

        case Some(localDefSym) =>
          val es = exps.zip(fparams0).filter { // Exclude constant parameters
            case (_, (_, pkind)) => pkind == ParamKind.NonConst
          }.map {
            case (e, (_, _)) => rewriteExp(e)
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

  private def isRewritable(defn: MonoAst.Def)(implicit ctx: LocalContext, flix: Flix): Boolean = {
    visitExp(defn.exp, tailPos = TailPosition.Tail)(defn.sym, ctx, flix)
    val containsRecursiveCall = ctx.selfTailCalls.nonEmpty
    val allRecursiveCallsInTailPos = ctx.selfTailCalls.forall { case (_, tps) => tps == TailPosition.Tail }
    containsRecursiveCall && allRecursiveCallsInTailPos
  }

  private def rewriteDefn(defn: MonoAst.Def)(implicit ctx: LocalContext, flix: Flix): MonoAst.Def = {
    implicit val params: List[(MonoAst.FormalParam, ParamKind)] = paramKinds(ctx.selfTailCalls.map(_._1), defn.spec.fparams)
    implicit val subst: Substitution = mkSubst(defn, params)
    val rewrittenExp = rewriteExp(defn.exp)
    val body = mkLocalDefExpr(rewrittenExp)
    defn.copy(exp = body)
  }

  private def mkSubst(defn: MonoAst.Def, params: List[(MonoAst.FormalParam, ParamKind)])(implicit flix: Flix): Substitution = {
    val nonConstantParams = params.filter { case (_, pkind) => pkind == ParamKind.NonConst }
    val varSubst = nonConstantParams.map { case (fp, _) => fp.sym -> Symbol.freshVarSym(fp.sym) }.toMap
    val freshLocalDefSym = mkFreshLocalDefSym(defn)
    Substitution.from(defn.sym, freshLocalDefSym, varSubst)
  }

  private def paramKinds(calls: Iterable[Expr.ApplyDef], fparams: Iterable[MonoAst.FormalParam]): List[(MonoAst.FormalParam, ParamKind)] = {
    val matrix = calls.map(call => fparams.zip(call.exps)).transpose
    matrix.map {
      case invocations =>
        val allConstant = invocations.forall {
          case (fp, Expr.Var(sym, _, _)) => fp.sym == sym
          case _ => false
        }
        invocations.headOption match {
          case Some((fp, _)) if allConstant => (fp, ParamKind.Const)
          case Some((fp, _)) => (fp, ParamKind.NonConst)
          case None => throw InternalCompilerException("unexpected empty head", SourceLocation.Unknown)
        }
    }.toList
  }

  private def mkFreshLocalDefSym(defn: MonoAst.Def)(implicit flix: Flix): Symbol.VarSym = {
    Symbol.freshVarSym(defn.sym.text + "Loop", BoundBy.LocalDef, defn.sym.loc)(Scope.Top, flix)
  }

  /**
    * Returns a [[Expr.LocalDef]] that is immediately applied after its declaration. Only the parameters that are alive are declared and applied.
    * Dead parameters are captured from the containing function.
    *
    * @param body   The body of the local def to be created.
    * @param subst  The variable substitution.
    * @param params The list of formal parameters and their [[ParamKind]].
    */
  private def mkLocalDefExpr(body: Expr)(implicit subst: Substitution, params: List[(MonoAst.FormalParam, ParamKind)]): Expr = {
    val nonConstantParams = params.filter {
      case (_, pkind) => pkind == ParamKind.NonConst
    }
    val args = nonConstantParams.map {
      case (fp, _) => Expr.Var(fp.sym, fp.tpe, fp.loc.asSynthetic)
    }
    val applyLocalDefExpr = Expr.ApplyLocalDef(subst.fresh, args, body.tpe, body.eff, body.loc.asSynthetic)

    val localDefParams = nonConstantParams.map {
      case (fp, _) => fp.copy(sym = subst(fp.sym), loc = fp.loc.asSynthetic)
    }
    val tpe = applyLocalDefExpr.tpe
    val eff = applyLocalDefExpr.eff
    val loc = applyLocalDefExpr.loc.asSynthetic
    Expr.LocalDef(subst.fresh, localDefParams, body, applyLocalDefExpr, tpe, eff, loc)
  }

  private object LocalContext {

    def mk(): LocalContext = new LocalContext(mutable.ArrayBuffer.empty)

  }

  private case class LocalContext(selfTailCalls: mutable.ArrayBuffer[(Expr.ApplyDef, TailPosition)])

  private object Substitution {
    def from(old: Symbol.DefnSym, fresh: Symbol.VarSym, vars: Map[Symbol.VarSym, Symbol.VarSym]): Substitution = Substitution(old, fresh, vars)
  }

  private case class Substitution(old: Symbol.DefnSym, fresh: Symbol.VarSym, vars: Map[Symbol.VarSym, Symbol.VarSym]) {
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
  }

  private sealed trait TailPosition

  private object TailPosition {

    case object Tail extends TailPosition

    case object NonTail extends TailPosition

  }

  private sealed trait ParamKind

  private object ParamKind {

    case object Const extends ParamKind

    case object NonConst extends ParamKind

  }
}
