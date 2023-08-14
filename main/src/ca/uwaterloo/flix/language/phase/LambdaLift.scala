/*
 * Copyright 2015-2016 Ming-Ho Yee
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
import ca.uwaterloo.flix.language.ast.Ast.BoundBy
import ca.uwaterloo.flix.language.ast.{Ast, AtomicOp, LiftedAst, MonoType, Purity, SimplifiedAst, Symbol}
import ca.uwaterloo.flix.util.InternalCompilerException

import scala.collection.mutable

object LambdaLift {

  /**
    * Mutable map of top level definitions.
    */
  private type TopLevel = mutable.Map[Symbol.DefnSym, LiftedAst.Def]

  /**
    * Performs lambda lifting on the given AST `root`.
    */
  def run(root: SimplifiedAst.Root)(implicit flix: Flix): LiftedAst.Root = flix.phase("LambdaLift") {
    // A mutable map to hold lambdas that are lifted to the top level.
    val m: TopLevel = mutable.Map.empty

    val newDefs = root.defs.map {
      case (sym, decl) => sym -> liftDef(decl, m)
    }

    val newEnums = root.enums.map {
      case (sym, enum0) => sym -> visitEnum(enum0)
    }

    LiftedAst.Root(
      newDefs ++ m,
      newEnums,
      root.entryPoint,
      root.sources
    )
  }

  /**
    * Performs lambda lifting on the given definition `def0`.
    */
  private def liftDef(def0: SimplifiedAst.Def, m: TopLevel)(implicit flix: Flix): LiftedAst.Def = def0 match {
    case SimplifiedAst.Def(ann, mod, sym, fparams, exp, tpe, _, loc) =>
      val fs = fparams.map(visitFormalParam)
      val e = liftExp(exp, sym, m)
      LiftedAst.Def(ann, mod, sym, Nil, fs, e, tpe, e.purity, loc)
  }

  /**
    * Translates the given simplified enum declaration `enum0` into a lifted enum declaration.
    */
  private def visitEnum(enum0: SimplifiedAst.Enum): LiftedAst.Enum = enum0 match {
    case SimplifiedAst.Enum(ann, mod, sym, cases, tpe, loc) =>
      val cs = cases.map {
        case (tag, SimplifiedAst.Case(caseSym, caseTpe, loc)) => tag -> LiftedAst.Case(caseSym, caseTpe, loc)
      }
      LiftedAst.Enum(ann, mod, sym, cs, tpe, loc)
  }

  /**
    * Performs lambda lifting on the given expression `exp0` occurring with the given symbol `sym0`.
    */
  private def liftExp(exp0: SimplifiedAst.Expr, sym0: Symbol.DefnSym, m: TopLevel)(implicit flix: Flix): LiftedAst.Expr = {
    /**
      * Performs closure conversion and lambda lifting on the given expression `exp0`.
      */
    def visitExp(e: SimplifiedAst.Expr): LiftedAst.Expr = e match {
      case SimplifiedAst.Expr.Cst(cst, tpe, loc) => LiftedAst.Expr.Cst(cst, tpe, loc)

      case SimplifiedAst.Expr.Var(sym, tpe, loc) => LiftedAst.Expr.Var(sym, tpe, loc)

      case SimplifiedAst.Expr.LambdaClosure(cparams, fparams, freeVars, exp, tpe, loc) =>
        // Recursively lift the inner expression.
        val liftedExp = visitExp(exp)

        // Generate a fresh symbol for the new lifted definition.
        val freshSymbol = Symbol.freshDefnSym(sym0)

        // Construct annotations and modifiers for the fresh definition.
        val ann = Ast.Annotations.Empty
        val mod = Ast.Modifiers(Ast.Modifier.Synthetic :: Nil)

        // Construct the closure parameters
        val cs = if (cparams.isEmpty)
          List(LiftedAst.FormalParam(Symbol.freshVarSym("_lift", BoundBy.FormalParam, loc), Ast.Modifiers.Empty, MonoType.Unit, loc))
        else cparams.map(visitFormalParam)

        // Construct the formal parameters.
        val fs = fparams.map(visitFormalParam)

        // Construct a new definition.
        val defTpe = tpe.result
        val defn = LiftedAst.Def(ann, mod, freshSymbol, cs, fs, liftedExp, defTpe, liftedExp.purity, loc)

        // Add the new definition to the map of lifted definitions.
        m += (freshSymbol -> defn)

        // Construct the closure args.
        val closureArgs = if (freeVars.isEmpty)
          List(LiftedAst.Expr.Cst(Ast.Constant.Unit, MonoType.Unit, loc))
        else freeVars.map {
          case SimplifiedAst.FreeVar(sym, tpe) => LiftedAst.Expr.Var(sym, tpe, sym.loc)
        }

        // Construct the closure expression.
        LiftedAst.Expr.ApplyAtomic(AtomicOp.Closure(freshSymbol), closureArgs, tpe, Purity.Pure, loc)

      case SimplifiedAst.Expr.ApplyAtomic(op, exps, tpe, purity, loc) =>
        val es = exps map visitExp
        LiftedAst.Expr.ApplyAtomic(op, es, tpe, purity, loc)

      case SimplifiedAst.Expr.ApplyClo(exp, exps, tpe, purity, loc) =>
        val e = visitExp(exp)
        val es = exps map visitExp
        LiftedAst.Expr.ApplyClo(e, es, Ast.CallType.NonTailCall, tpe, purity, loc)

      case SimplifiedAst.Expr.ApplyDef(sym, exps, tpe, purity, loc) =>
        val es = exps map visitExp
        LiftedAst.Expr.ApplyDef(sym, es, Ast.CallType.NonTailCall, tpe, purity, loc)

      case SimplifiedAst.Expr.IfThenElse(exp1, exp2, exp3, tpe, purity, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        val e3 = visitExp(exp3)
        LiftedAst.Expr.IfThenElse(e1, e2, e3, tpe, purity, loc)

      case SimplifiedAst.Expr.Branch(exp, branches, tpe, purity, loc) =>
        val e = visitExp(exp)
        val bs = branches map {
          case (sym, br) => sym -> visitExp(br)
        }
        LiftedAst.Expr.Branch(e, bs, tpe, purity, loc)

      case SimplifiedAst.Expr.JumpTo(sym, tpe, purity, loc) =>
        LiftedAst.Expr.JumpTo(sym, tpe, purity, loc)

      case SimplifiedAst.Expr.Let(sym, exp1, exp2, tpe, purity, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        LiftedAst.Expr.Let(sym, e1, e2, tpe, purity, loc)

      case SimplifiedAst.Expr.LetRec(varSym, exp1, exp2, tpe, purity, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        e1 match {
          case LiftedAst.Expr.ApplyAtomic(AtomicOp.Closure(defSym), closureArgs, _, _, _) =>
            val index = closureArgs.indexWhere {
              case LiftedAst.Expr.Var(sym, _, _) => varSym == sym
              case _ => false
            }
            if (index == -1) {
              // function never calls itself
              LiftedAst.Expr.Let(varSym, e1, e2, tpe, purity, loc)
            } else
              LiftedAst.Expr.LetRec(varSym, index, defSym, e1, e2, tpe, purity, loc)

          case _ => throw InternalCompilerException(s"Unexpected expression: '$e1'.", loc)
        }

      case SimplifiedAst.Expr.Scope(sym, exp, tpe, purity, loc) =>
        val e = visitExp(exp)
        LiftedAst.Expr.Scope(sym, e, tpe, purity, loc)

      case SimplifiedAst.Expr.TryCatch(exp, rules, tpe, purity, loc) =>
        val e = visitExp(exp)
        val rs = rules map {
          case SimplifiedAst.CatchRule(sym, clazz, body) =>
            val b = visitExp(body)
            LiftedAst.CatchRule(sym, clazz, b)
        }
        LiftedAst.Expr.TryCatch(e, rs, tpe, purity, loc)

      case SimplifiedAst.Expr.TryWith(exp, effUse, rules, tpe, purity, loc) =>
        val e = visitExp(exp)
        val rs = rules map {
          case SimplifiedAst.HandlerRule(sym, fparams, body) =>
            val fps = fparams.map(visitFormalParam)
            val b = visitExp(body)
            LiftedAst.HandlerRule(sym, fps, b)
        }
        LiftedAst.Expr.TryWith(e, effUse, rs, tpe, purity, loc)

      case SimplifiedAst.Expr.Do(op, exps, tpe, purity, loc) =>
        val es = exps.map(visitExp)
        LiftedAst.Expr.Do(op, es, tpe, purity, loc)

      case SimplifiedAst.Expr.Resume(exp, tpe, loc) =>
        val e = visitExp(exp)
        LiftedAst.Expr.Resume(e, tpe, loc)

      case SimplifiedAst.Expr.NewObject(name, clazz, tpe, purity, methods0, loc) =>
        val methods = methods0.map(visitJvmMethod)
        LiftedAst.Expr.NewObject(name, clazz, tpe, purity, methods, loc)

      case SimplifiedAst.Expr.Def(_, _, loc) => throw InternalCompilerException(s"Unexpected expression.", loc)
      case SimplifiedAst.Expr.Lambda(_, _, _, loc) => throw InternalCompilerException(s"Unexpected expression.", loc)
      case SimplifiedAst.Expr.Apply(_, _, _, _, loc) => throw InternalCompilerException(s"Unexpected expression.", loc)
    }

    def visitJvmMethod(method: SimplifiedAst.JvmMethod): LiftedAst.JvmMethod = method match {
      case SimplifiedAst.JvmMethod(ident, fparams0, exp, retTpe, purity, loc) =>
        val fparams = fparams0 map visitFormalParam
        LiftedAst.JvmMethod(ident, fparams, visitExp(exp), retTpe, purity, loc)
    }

    visitExp(exp0)
  }

  /**
    * Translates the given simplified formal parameter `fparam` into a lifted formal parameter.
    */
  private def visitFormalParam(fparam: SimplifiedAst.FormalParam): LiftedAst.FormalParam = fparam match {
    case SimplifiedAst.FormalParam(sym, mod, tpe, loc) => LiftedAst.FormalParam(sym, mod, tpe, loc)
  }

}
