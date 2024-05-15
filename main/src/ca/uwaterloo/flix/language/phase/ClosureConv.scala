/*
 * Copyright 2015-2016, 2022 Ming-Ho Yee, Magnus Madsen
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
import ca.uwaterloo.flix.language.ast.SimplifiedAst._
import ca.uwaterloo.flix.language.ast.{Ast, AtomicOp, MonoType, Purity, SourceLocation, Symbol}
import ca.uwaterloo.flix.language.dbg.AstPrinter.DebugSimplifiedAst
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps}

import scala.collection.immutable.SortedSet
import scala.collection.mutable

object ClosureConv {

  /**
    * Performs closure conversion on the given AST `root`.
    */
  def run(root: Root)(implicit flix: Flix): Root = flix.phase("ClosureConv") {
    val newDefs = ParOps.parMapValues(root.defs)(visitDef)

    root.copy(defs = newDefs)
  }

  /**
    * Performs closure conversion on the given definition `def0`.
    */
  private def visitDef(def0: Def)(implicit flix: Flix): Def = {
    def0.copy(exp = visitExp(def0.exp))
  }

  /**
    * Performs closure conversion on the given expression `exp0`.
    */
  private def visitExp(exp0: Expr)(implicit flix: Flix): Expr = exp0 match {
    case Expr.Cst(_, _, _) => exp0

    case Expr.Var(_, _, _) => exp0

    case Expr.Def(sym, tpe, loc) =>
      //
      // Special Case: A def expression occurs outside of an `Apply` expression.
      //

      //
      // We must create a closure that references the definition symbol.
      //
      // The closure has no free variables since it is a reference to a top-level function.
      //
      // This case happens if the programmers writes e.g.:
      //
      // let m = List.map; ...
      //
      Expr.ApplyAtomic(AtomicOp.Closure(sym), List.empty, tpe, Purity.Pure, loc)

    case Expr.Lambda(fparams, exp, tpe, loc) =>
      //
      // Main case: Convert a lambda expression to a lambda closure.
      //
      mkLambdaClosure(fparams, exp, tpe, loc)

    case Expr.Apply(exp, exps, tpe, purity, loc) => exp match {
      case Expr.Def(sym, _, _) =>
        //
        // Special Case: Direct call to a known function symbol.
        //
        val es = exps.map(visitExp)
        Expr.ApplyDef(sym, es, tpe, purity, loc)
      case _ =>
        //
        // General Case: Call to closure.
        //
        val e = visitExp(exp)
        val es = exps.map(visitExp)
        Expr.ApplyClo(e, es, tpe, purity, loc)
    }

    case Expr.ApplyAtomic(op, exps, tpe, purity, loc) =>
      val es = exps map visitExp
      Expr.ApplyAtomic(op, es, tpe, purity, loc)

    case Expr.IfThenElse(exp1, exp2, exp3, tpe, purity, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val e3 = visitExp(exp3)
      Expr.IfThenElse(e1, e2, e3, tpe, purity, loc)

    case Expr.Stm(exp1, exp2, tpe, purity, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      Expr.Stm(e1, e2, tpe, purity, loc)

    case Expr.Branch(exp, branches, tpe, purity, loc) =>
      val e = visitExp(exp)
      val bs = branches map {
        case (sym, br) => sym -> visitExp(br)
      }
      Expr.Branch(e, bs, tpe, purity, loc)

    case Expr.JumpTo(sym, tpe, purity, loc) =>
      Expr.JumpTo(sym, tpe, purity, loc)

    case Expr.Let(sym, e1, e2, tpe, purity, loc) =>
      Expr.Let(sym, visitExp(e1), visitExp(e2), tpe, purity, loc)

    case Expr.LetRec(sym, e1, e2, tpe, purity, loc) =>
      Expr.LetRec(sym, visitExp(e1), visitExp(e2), tpe, purity, loc)

    case Expr.Scope(sym, e, tpe, purity, loc) =>
      Expr.Scope(sym, visitExp(e), tpe, purity, loc)

    case Expr.TryCatch(exp, rules, tpe, purity, loc) =>
      val e = visitExp(exp)
      val rs = rules map {
        case CatchRule(sym, clazz, body) =>
          val b = visitExp(body)
          CatchRule(sym, clazz, b)
      }
      Expr.TryCatch(e, rs, tpe, purity, loc)

    case Expr.TryWith(exp, effUse, rules, tpe, purity, loc) =>
      // Lift the body and all the rule expressions
      val expLoc = exp.loc.asSynthetic
      val freshSym = Symbol.freshVarSym("_closureConv", Ast.BoundBy.FormalParam, expLoc)
      val fp = FormalParam(freshSym, Ast.Modifiers.Empty, MonoType.Unit, expLoc)
      val e = mkLambdaClosure(List(fp), exp, MonoType.Arrow(List(MonoType.Unit), tpe), expLoc)
      val rs = rules map {
        case HandlerRule(opUse, fparams, body) =>
          val cloType = MonoType.Arrow(fparams.map(_.tpe), body.tpe)
          val clo = mkLambdaClosure(fparams, body, cloType, opUse.loc)
          HandlerRule(opUse, fparams, clo)
      }
      Expr.TryWith(e, effUse, rs, tpe, purity, loc)

    case Expr.Do(op, exps, tpe, purity, loc) =>
      val es = exps.map(visitExp)
      Expr.Do(op, es, tpe, purity, loc)

    case Expr.NewObject(name, clazz, tpe, purity, methods0, loc) =>
      val methods = methods0 map {
        case JvmMethod(ident, fparams, exp, retTpe, purity, loc) =>
          val cloType = MonoType.Arrow(fparams.map(_.tpe), retTpe)
          val clo = mkLambdaClosure(fparams, exp, cloType, loc)
          JvmMethod(ident, fparams, clo, retTpe, purity, loc)
      }
      Expr.NewObject(name, clazz, tpe, purity, methods, loc)

    case Expr.LambdaClosure(_, _, _, _, _, loc) => throw InternalCompilerException(s"Unexpected expression: '$exp0'.", loc)

    case Expr.ApplyClo(_, _, _, _, loc) => throw InternalCompilerException(s"Unexpected expression: '$exp0'.", loc)

    case Expr.ApplyDef(_, _, _, _, loc) => throw InternalCompilerException(s"Unexpected expression: '$exp0'.", loc)
  }

  /**
    * Returns a LambdaClosure under the given formal parameters fparams for the body expression exp where the overall lambda has type tpe.
    *
    * `exp` is visited inside this function and should not be visited before.
    */
  private def mkLambdaClosure(fparams: List[FormalParam], exp: Expr, tpe: MonoType, loc: SourceLocation)(implicit flix: Flix): Expr.LambdaClosure = {
    // Step 1: Compute the free variables in the lambda expression.
    //         (Remove the variables bound by the lambda itself).
    val fvs = filterBoundParams(freeVars(exp), fparams).toList

    // Step 2: Convert the free variables into a new parameter list and substitution.
    val (cloParams, subst) = getFormalParamsAndSubst(fvs, loc)

    // Step 3: Replace every old symbol by its new symbol in the body of the lambda.
    val newBody = visitExp(applySubst(exp, subst))

    // Step 4: Put everything back together.
    Expr.LambdaClosure(cloParams, fparams, fvs, newBody, tpe, loc)
  }

  /**
    * Returns a pair of a formal parameter list and a substitution
    */
  private def getFormalParamsAndSubst(fvs: List[FreeVar], loc: SourceLocation)(implicit flix: Flix): (List[FormalParam], Map[Symbol.VarSym, Symbol.VarSym]) = {
    val subst = mutable.Map.empty[Symbol.VarSym, Symbol.VarSym]
    val fparams = fvs.map {
      case FreeVar(oldSym, ptpe) =>
        val newSym = Symbol.freshVarSym(oldSym)
        subst += (oldSym -> newSym)
        FormalParam(newSym, Ast.Modifiers.Empty, ptpe, loc)
    }
    (fparams, subst.toMap)
  }

  /**
    * Returns all free variables in the given expression `exp0`.
    *
    * Note: The result:
    *   - (A) must be a set to avoid duplicates, and
    *   - (B) must be sorted to ensure deterministic compilation.
    */
  private def freeVars(exp0: Expr): SortedSet[FreeVar] = exp0 match {
    case Expr.Cst(_, _, _) => SortedSet.empty

    case Expr.Var(sym, tpe, _) => SortedSet(FreeVar(sym, tpe))

    case Expr.Def(_, _, _) => SortedSet.empty

    case Expr.Lambda(args, body, _, _) =>
      filterBoundParams(freeVars(body), args)

    case Expr.Apply(exp, args, _, _, _) =>
      freeVars(exp) ++ freeVarsExps(args)

    case Expr.ApplyAtomic(_, exps, _, _, _) =>
      freeVarsExps(exps)

    case Expr.IfThenElse(exp1, exp2, exp3, _, _, _) =>
      freeVars(exp1) ++ freeVars(exp2) ++ freeVars(exp3)

    case Expr.Stm(exp1, exp2, _, _, _) =>
      freeVars(exp1) ++ freeVars(exp2)

    case Expr.Branch(exp, branches, _, _, _) =>
      freeVars(exp) ++ (branches flatMap {
        case (_, br) => freeVars(br)
      })

    case Expr.JumpTo(_, _, _, _) => SortedSet.empty

    case Expr.Let(sym, exp1, exp2, _, _, _) =>
      filterBoundVar(freeVars(exp1) ++ freeVars(exp2), sym)

    case Expr.LetRec(sym, exp1, exp2, _, _, _) =>
      filterBoundVar(freeVars(exp1) ++ freeVars(exp2), sym)

    case Expr.Scope(sym, exp, _, _, _) => filterBoundVar(freeVars(exp), sym)

    case Expr.TryCatch(exp, rules, _, _, _) => rules.foldLeft(freeVars(exp)) {
      case (acc, CatchRule(sym, _, exp)) =>
        acc ++ filterBoundVar(freeVars(exp), sym)
    }

    case Expr.TryWith(exp, _, rules, _, _, _) => rules.foldLeft(freeVars(exp)) {
      case (acc, HandlerRule(_, fparams, exp)) =>
        acc ++ filterBoundParams(freeVars(exp), fparams)
    }

    case Expr.Do(_, exps, _, _, _) => freeVarsExps(exps)

    case Expr.NewObject(_, _, _, _, methods, _) =>
      methods.foldLeft(SortedSet.empty[FreeVar]) {
        case (acc, JvmMethod(_, fparams, exp, _, _, _)) =>
          acc ++ filterBoundParams(freeVars(exp), fparams)
      }

    case Expr.LambdaClosure(_, _, _, _, _, loc) => throw InternalCompilerException(s"Unexpected expression: '$exp0'.", loc)

    case Expr.ApplyClo(_, _, _, _, loc) => throw InternalCompilerException(s"Unexpected expression: '$exp0'.", loc)

    case Expr.ApplyDef(_, _, _, _, loc) => throw InternalCompilerException(s"Unexpected expression: '$exp0'.", loc)
  }

  /**
    * Returns the free variables in `exps0`.
    */
  private def freeVarsExps(exps0: List[Expr]): SortedSet[FreeVar] =
    exps0.foldLeft(SortedSet.empty[FreeVar]) {
      case (acc, exp) => acc ++ freeVars(exp)
    }

  /**
    * Returns `fvs` without the variable symbol `bound`.
    */
  private def filterBoundVar(fvs: SortedSet[FreeVar], bound: Symbol.VarSym): SortedSet[FreeVar] =
    fvs.filter {
      case FreeVar(sym, _) => sym != bound
    }

  /**
    * Returns `fvs` without all the variable symbols in the formal parameters `bound`.
    */
  private def filterBoundParams(fvs: SortedSet[FreeVar], bound: List[FormalParam]): SortedSet[FreeVar] =
    fvs.filter {
      case FreeVar(sym, _) => !bound.exists(fparam => sym == fparam.sym)
    }

  /**
    * Applies the given substitution map `subst` to the given expression `e`.
    */
  private def applySubst(e0: Expr, subst: Map[Symbol.VarSym, Symbol.VarSym])(implicit flix: Flix): Expr = {

    def visitExp(e: Expr): Expr = e match {
      case Expr.Cst(_, _, _) => e

      case Expr.Var(sym, tpe, loc) => subst.get(sym) match {
        case None => Expr.Var(sym, tpe, loc)
        case Some(newSym) => Expr.Var(newSym, tpe, loc)
      }

      case Expr.Def(_, _, _) => e

      case Expr.Lambda(fparams, exp, tpe, loc) =>
        val fs = fparams.map(visitFormalParam)
        val e = visitExp(exp)
        Expr.Lambda(fs, e, tpe, loc)

      case Expr.ApplyAtomic(op, exps, tpe, purity, loc) =>
        val es = exps map visitExp
        Expr.ApplyAtomic(op, es, tpe, purity, loc)

      case Expr.LambdaClosure(cparams, fparams, freeVars, exp, tpe, loc) =>
        val e = visitExp(exp)
        Expr.LambdaClosure(cparams, fparams, freeVars, e, tpe, loc)

      case Expr.ApplyClo(exp, args, tpe, purity, loc) =>
        val e = visitExp(exp)
        val as = args map visitExp
        Expr.ApplyClo(e, as, tpe, purity, loc)

      case Expr.ApplyDef(sym, args, tpe, purity, loc) =>
        val as = args map visitExp
        Expr.ApplyDef(sym, as, tpe, purity, loc)

      case Expr.Apply(exp, args, tpe, purity, loc) =>
        val e = visitExp(exp)
        val as = args map visitExp
        Expr.Apply(e, as, tpe, purity, loc)

      case Expr.IfThenElse(exp1, exp2, exp3, tpe, purity, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        val e3 = visitExp(exp3)
        Expr.IfThenElse(e1, e2, e3, tpe, purity, loc)

      case Expr.Stm(exp1, exp2, tpe, purity, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        Expr.Stm(e1, e2, tpe, purity, loc)

      case Expr.Branch(exp, branches, tpe, purity, loc) =>
        val e = visitExp(exp)
        val bs = branches map {
          case (sym, br) => sym -> visitExp(br)
        }
        Expr.Branch(e, bs, tpe, purity, loc)

      case Expr.JumpTo(sym, tpe, purity, loc) =>
        Expr.JumpTo(sym, tpe, purity, loc)

      case Expr.Let(sym, exp1, exp2, tpe, purity, loc) =>
        val newSym = subst.getOrElse(sym, sym)
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        Expr.Let(newSym, e1, e2, tpe, purity, loc)

      case Expr.LetRec(sym, exp1, exp2, tpe, purity, loc) =>
        val newSym = subst.getOrElse(sym, sym)
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        Expr.LetRec(newSym, e1, e2, tpe, purity, loc)

      case Expr.Scope(sym, exp, tpe, purity, loc) =>
        val newSym = subst.getOrElse(sym, sym)
        val e = visitExp(exp)
        Expr.Scope(newSym, e, tpe, purity, loc)

      case Expr.TryCatch(exp, rules, tpe, purity, loc) =>
        val e = visitExp(exp)
        val rs = rules map {
          case CatchRule(sym, clazz, body) =>
            val b = visitExp(body)
            CatchRule(sym, clazz, b)
        }
        Expr.TryCatch(e, rs, tpe, purity, loc)

      case Expr.TryWith(exp, effUse, rules, tpe, purity, loc) =>
        // TODO AE do we need to do something here?
        val e = visitExp(exp)
        val rs = rules map {
          case HandlerRule(sym, fparams, body) =>
            val fs = fparams.map(visitFormalParam)
            val b = visitExp(body)
            HandlerRule(sym, fs, b)
        }
        Expr.TryWith(e, effUse, rs, tpe, purity, loc)

      case Expr.Do(op, exps, tpe, purity, loc) =>
        val es = exps.map(visitExp)
        Expr.Do(op, es, tpe, purity, loc)

      case Expr.NewObject(name, clazz, tpe, purity, methods0, loc) =>
        val methods = methods0.map(visitJvmMethod)
        Expr.NewObject(name, clazz, tpe, purity, methods, loc)

    }

    def visitFormalParam(fparam: FormalParam): FormalParam = fparam match {
      case FormalParam(sym, mod, tpe, loc) =>
        subst.get(sym) match {
          case None => FormalParam(sym, mod, tpe, loc)
          case Some(newSym) => FormalParam(newSym, mod, tpe, loc)
        }
    }

    def visitJvmMethod(method: JvmMethod)(implicit flix: Flix): JvmMethod = method match {
      case JvmMethod(ident, fparams0, exp, retTpe, purity, loc) =>
        val fparams = fparams0.map(visitFormalParam)
        JvmMethod(ident, fparams, applySubst(exp, subst), retTpe, purity, loc)
    }

    visitExp(e0)
  }

}
