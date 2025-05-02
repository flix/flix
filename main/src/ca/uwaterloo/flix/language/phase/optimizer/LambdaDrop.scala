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
import ca.uwaterloo.flix.language.ast.{MonoAst, SourceLocation, Symbol, TypeConstructor}
import ca.uwaterloo.flix.language.dbg.AstPrinter.DebugMonoAst
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps}

import scala.collection.mutable

/**
  * Rewrites recursive higher-order functions to non-recursive functions with
  * a recursive local def where constant parameters become closure captured.
  *
  * A constant parameter is one that has the same value in every recursive call.
  *
  * Example:
  *
  * {{{
  *   def map(f: a -> b, l: List[a]): List[b] = match l {
  *       case Nil     => Nil
  *       case x :: xs => f(x) :: map(f, xs)
  *   }
  * }}}
  *
  * Is rewritten to:
  *
  * {{{
  *   def map(f: a -> b, l: List[a]): List[b] =
  *       def map$loop(l1) = match l1 {
  *           case Nil      => Nil
  *           case x :: xs  => f(x) :: map$loop(xs)
  *       };
  *       map$loop(l)
  * }}}
  */
object LambdaDrop {

  /** See [[LambdaDrop]] for documentation. */
  def run(root: MonoAst.Root)(implicit flix: Flix): MonoAst.Root = flix.phase("LambdaDrop") {
    val newDefs = ParOps.parMapValues(root.defs)(visitDef)
    root.copy(defs = newDefs)
  }

  /**
    * Performs lambda dropping on `defn` if the [[isDroppable]] predicate holds.
    * Returns `defn` as is otherwise.
    */
  private def visitDef(defn: MonoAst.Def)(implicit flix: Flix): MonoAst.Def = {
    implicit val lctx: LocalContext = LocalContext.mk()
    if (isDroppable(defn))
      lambdaDrop(defn)
    else
      defn
  }

  /**
    * A function is droppable if
    * (a) it contains at least one recursive call and
    * (b) is a higher-order function, i.e., it has at least one formal parameter
    * with a function type and
    * (c) has at least one constant parameter.
    *
    * The latter condition can be checked by the [[isHigherOrder]] predicate.
    */
  private def isDroppable(defn: MonoAst.Def)(implicit lctx: LocalContext): Boolean = {
    if (isHigherOrder(defn)) {
      visitExp(defn.exp)(defn.sym, lctx)
      lctx.recursiveCalls.nonEmpty && hasConstantParameter(lctx.recursiveCalls.toList, defn.spec.fparams)
    } else {
      false
    }
  }

  /** Returns `true` if at least one formal parameter of `defn` has an arrow type. */
  private def isHigherOrder(defn: MonoAst.Def): Boolean = {
    defn.spec.fparams.exists {
      fp =>
        fp.tpe.typeConstructor match {
          case Some(TypeConstructor.Arrow(_)) => true
          case Some(TypeConstructor.ArrowWithoutEffect(_)) => true
          case Some(_) => false
          case None => false
        }
    }
  }

  /**
    * Replaces the body of `defn` with a [[Expr.LocalDef]] where constant parameters
    * are closure captured.
    */
  private def lambdaDrop(defn: MonoAst.Def)(implicit lctx: LocalContext, flix: Flix): MonoAst.Def = {
    implicit val params: List[(MonoAst.FormalParam, ParamKind)] = paramKinds(lctx.recursiveCalls.toList, defn.spec.fparams)
    implicit val (newDefnSym, subst): (Symbol.VarSym, Substitution) = mkSubst(defn, params)
    val rewrittenExp = rewriteExp(defn.exp)(defn.sym, newDefnSym, subst, params)
    val body = mkLocalDefExpr(rewrittenExp, newDefnSym)
    defn.copy(exp = body)
  }

  /**
    * Collects all recursive calls in `exp0` and stores them in the local context `lctx`.
    *
    * @param exp0 the expression to visit.
    * @param sym0 the symbol of function being visited.
    * @param lctx the local context. This will be mutated.
    */
  private def visitExp(exp0: MonoAst.Expr)(implicit sym0: Symbol.DefnSym, lctx: LocalContext): Unit = exp0 match {
    case Expr.Cst(_, _, _) =>

    case Expr.Var(_, _, _) =>

    case Expr.Lambda(_, exp, _, _) =>
      visitExp(exp)

    case Expr.ApplyAtomic(_, exps, _, _, _) =>
      exps.foreach(visitExp)

    case Expr.ApplyClo(exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)

    case expr@Expr.ApplyDef(sym, exps, _, _, _, _) =>
      // Check for self-recursive call
      if (sym == sym0) {
        lctx.recursiveCalls.addOne(expr)
      }
      exps.foreach(visitExp)

    case Expr.ApplyLocalDef(_, exps, _, _, _) =>
      exps.foreach(visitExp)

    case Expr.Let(_, exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)

    case Expr.LocalDef(_, _, exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)

    case Expr.Scope(_, _, exp, _, _, _) =>
      visitExp(exp)

    case Expr.IfThenElse(exp1, exp2, exp3, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)
      visitExp(exp3)

    case Expr.Stm(exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)

    case Expr.Discard(exp, _, _) =>
      visitExp(exp)

    case Expr.Match(exp1, rules, _, _, _) =>
      visitExp(exp1)
      rules.foreach {
        case MonoAst.MatchRule(_, guard, exp2) =>
          guard.foreach(visitExp)
          visitExp(exp2)
      }

    case Expr.VectorLit(exps, _, _, _) =>
      exps.foreach(visitExp)

    case Expr.VectorLoad(exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)

    case Expr.VectorLength(exp, _) =>
      visitExp(exp)

    case Expr.Ascribe(exp, _, _, _) =>
      visitExp(exp)

    case Expr.Cast(exp, _, _, _, _, _) =>
      visitExp(exp)

    case Expr.TryCatch(exp1, rules, _, _, _) =>
      visitExp(exp1)
      rules.foreach(rule => visitExp(rule.exp))

    case Expr.RunWith(exp1, _, rules, _, _, _) =>
      visitExp(exp1)
      rules.foreach(rule => visitExp(rule.exp))

    case Expr.Do(_, exps, _, _, _) =>
      exps.foreach(visitExp)

    case Expr.NewObject(_, _, _, _, methods, _) =>
      methods.foreach(m => visitExp(m.exp))
  }

  /**
    * Rewrites every [[Expr.ApplyDef]] expression of `oldDefnSym` to
    * an [[Expr.ApplyLocalDef]] expression, replacing `oldDefnSym` with
    * `newDefnSym`.
    *
    * Also applies the substitution `subst` on all variables, so non-constant
    * variables are renamed to matching formal parameters of the local def
    * with name `newDefnSym`.
    *
    * @param expr0      the expression to rewrite
    * @param oldDefnSym the symbol of the function to rewrite
    * @param newDefnSym the symbol of the local def to insert
    * @param subst      the substitution defined on non-constant parameters.
    *                   It is up to the caller to ensure which variables the substitution is defined over.
    * @param fparams0   the formal parameters and their [[ParamKind]]s of the function to rewrite.
    */
  private def rewriteExp(expr0: MonoAst.Expr)(implicit oldDefnSym: Symbol.DefnSym, newDefnSym: Symbol.VarSym, subst: Substitution, fparams0: List[(MonoAst.FormalParam, ParamKind)]): MonoAst.Expr = expr0 match {
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
      if (sym == oldDefnSym) {
        // Rewrite call to new function symbol and drop constant parameters
        val es = exps.zip(fparams0).filter {
          case (_, (_, pkind)) => pkind == ParamKind.NonConst
        }.map {
          case (e, (_, _)) => rewriteExp(e)
        }
        Expr.ApplyLocalDef(newDefnSym, es, tpe, eff, loc)
      } else {
        // Preserve call as is
        val es = exps.map(rewriteExp)
        Expr.ApplyDef(sym, es, itpe, tpe, eff, loc)
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

  /**
    * Returns a tuple with the new local def symbol and a substitution for every variable in `defn`.
    */
  private def mkSubst(defn: MonoAst.Def, params: List[(MonoAst.FormalParam, ParamKind)])(implicit flix: Flix): (Symbol.VarSym, Substitution) = {
    val nonConstantParams = params.filter { case (_, pkind) => pkind == ParamKind.NonConst }
    val substMap = nonConstantParams.map { case (fp, _) => fp.sym -> Symbol.freshVarSym(fp.sym) }.toMap
    val freshLocalDefSym = mkFreshLocalDefSym(defn)
    (freshLocalDefSym, Substitution(substMap))
  }

  /**
    * Returns the [[ParamKind]] of each formal parameter in `fparams`.
    *
    * If all expressions in `calls` that are in the position of formal parameter `x`
    * are the symbol `x` then it is marked [[ParamKind.Const]].
    *
    * Otherwise, it is marked [[ParamKind.NonConst]]
    */
  private def paramKinds(calls: List[Expr.ApplyDef], fparams: List[MonoAst.FormalParam]): List[(MonoAst.FormalParam, ParamKind)] = {
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
    }
  }

  /** Returns `true` if there exists at least one constant parameter. */
  private def hasConstantParameter(calls: List[Expr.ApplyDef], fparams: List[MonoAst.FormalParam]): Boolean = {
    paramKinds(calls, fparams).exists {
      case (_, pkind) => pkind == ParamKind.Const
    }
  }

  /** Returns a fresh [[Symbol.VarSym]] for a local def. */
  private def mkFreshLocalDefSym(defn: MonoAst.Def)(implicit flix: Flix): Symbol.VarSym = {
    val text = defn.sym.text + Flix.Delimiter + "loop"
    Symbol.freshVarSym(text, BoundBy.LocalDef, defn.sym.loc)(Scope.Top, flix)
  }

  /**
    * Returns a [[Expr.LocalDef]] that is immediately applied after its declaration.
    * Only non-constant parameters are declared as parameters for the local def.
    *
    * See [[LambdaDrop]] for an example.
    *
    * @param exp0 The body of the local def to be created.
    * @param sym    The symbol of the local def to be created.
    * @param subst  The variable substitution.
    * @param params The list of formal parameters and their [[ParamKind]].
    */
  private def mkLocalDefExpr(exp0: Expr, sym: Symbol.VarSym)(implicit subst: Substitution, params: List[(MonoAst.FormalParam, ParamKind)]): Expr = {
    val nonConstantParams = params.filter {
      case (_, pkind) => pkind == ParamKind.NonConst
    }
    val args = nonConstantParams.map {
      case (fp, _) => Expr.Var(fp.sym, fp.tpe, fp.loc.asSynthetic)
    }
    val applyLocalDefExpr = Expr.ApplyLocalDef(sym, args, exp0.tpe, exp0.eff, exp0.loc.asSynthetic)

    val localDefParams = nonConstantParams.map {
      case (fp, _) => fp.copy(sym = subst(fp.sym), loc = fp.loc.asSynthetic)
    }
    val tpe = applyLocalDefExpr.tpe
    val eff = applyLocalDefExpr.eff
    val loc = applyLocalDefExpr.loc.asSynthetic
    Expr.LocalDef(sym, localDefParams, exp0, applyLocalDefExpr, tpe, eff, loc)
  }

  private object LocalContext {

    /** Returns a fresh [[LocalContext]] */
    def mk(): LocalContext = new LocalContext(mutable.ArrayBuffer.empty)

  }

  /**
    * A local mutable context for [[visitExp]]. No requirements for thread-safety.
    *
    * @param recursiveCalls A mutable buffer to collect recursive calls.
    */
  private case class LocalContext(recursiveCalls: mutable.ArrayBuffer[Expr.ApplyDef])

  /**
    * A substitution defined on `vars`.
    * Applying the substitution to a variable not in `vars` returns the symbol itself.
    *
    * More formally, for all `x` not in `vars` it holds that `subst(x) = x`.
    * Likewise, for all `x` in `vars` where `vars(x) = y`  it holds that `subst(x) = y`.
    *
    * @param vars the partial map from stale to fresh variables.
    */
  private case class Substitution(vars: Map[Symbol.VarSym, Symbol.VarSym]) {
    def apply(sym: Symbol.VarSym): Symbol.VarSym = vars.get(sym) match {
      case Some(freshSym) => freshSym
      case None => sym
    }
  }

  private sealed trait ParamKind

  private object ParamKind {

    case object Const extends ParamKind

    case object NonConst extends ParamKind

  }
}
