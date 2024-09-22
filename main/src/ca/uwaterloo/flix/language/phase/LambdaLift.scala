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
import ca.uwaterloo.flix.language.ast.shared.{Constant, Scope}
import ca.uwaterloo.flix.language.ast.{Ast, AtomicOp, LiftedAst, MonoType, Purity, SimplifiedAst, Symbol}
import ca.uwaterloo.flix.language.dbg.AstPrinter._
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps}

import java.util.concurrent.ConcurrentLinkedQueue
import scala.jdk.CollectionConverters._

object LambdaLift {

  // We are safe to use the top scope everywhere because we do not use unification in this or future phases.
  private implicit val S: Scope = Scope.Top

  /**
    * Performs lambda lifting on the given AST `root`.
    */
  def run(root: SimplifiedAst.Root)(implicit flix: Flix): LiftedAst.Root = flix.phase("LambdaLift") {
    implicit val ctx: SharedContext = SharedContext(new ConcurrentLinkedQueue())

    val defs = ParOps.parMapValues(root.defs)(visitDef)
    val effects = ParOps.parMapValues(root.effects)(visitEffect)

    // Add lifted defs from the shared context to the existing defs.
    val newDefs = ctx.liftedDefs.asScala.foldLeft(defs) {
      case (macc, (sym, defn)) => macc + (sym -> defn)
    }

    LiftedAst.Root(newDefs, effects, root.entryPoint, root.reachable, root.sources)
  }

  private def visitDef(def0: SimplifiedAst.Def)(implicit ctx: SharedContext, flix: Flix): LiftedAst.Def = def0 match {
    case SimplifiedAst.Def(ann, mod, sym, fparams, exp, tpe, _, loc) =>
      val fs = fparams.map(visitFormalParam)
      val e = visitExp(exp)(sym, ctx, flix)
      LiftedAst.Def(ann, mod, sym, Nil, fs, e, tpe, loc)
  }

  private def visitEffect(effect: SimplifiedAst.Effect): LiftedAst.Effect = effect match {
    case SimplifiedAst.Effect(ann, mod, sym, ops0, loc) =>
      val ops = ops0.map(visitOp)
      LiftedAst.Effect(ann, mod, sym, ops, loc)
  }

  private def visitOp(op: SimplifiedAst.Op): LiftedAst.Op = op match {
    case SimplifiedAst.Op(sym, ann, mod, fparams0, tpe, purity, loc) =>
      val fparams = fparams0.map(visitFormalParam)
      LiftedAst.Op(sym, ann, mod, fparams, tpe, purity, loc)
  }

  private def visitExp(e: SimplifiedAst.Expr)(implicit sym0: Symbol.DefnSym, ctx: SharedContext, flix: Flix): LiftedAst.Expr = e match {
    case SimplifiedAst.Expr.Cst(cst, tpe, loc) => LiftedAst.Expr.Cst(cst, tpe, loc)

    case SimplifiedAst.Expr.Var(sym, tpe, loc) => LiftedAst.Expr.Var(sym, tpe, loc)

    case SimplifiedAst.Expr.LambdaClosure(cparams, fparams, freeVars, exp, tpe, loc) =>
      val arrowTpe = tpe match {
        case t: MonoType.Arrow => t
        case _ => throw InternalCompilerException(s"Lambda has unexpected type: $tpe", loc)
      }

      // Recursively lift the inner expression.
      val liftedExp = visitExp(exp)

      // Generate a fresh symbol for the new lifted definition.
      val freshSymbol = Symbol.freshDefnSym(sym0)

      // Construct annotations and modifiers for the fresh definition.
      val ann = Ast.Annotations.Empty
      val mod = Ast.Modifiers(Ast.Modifier.Synthetic :: Nil)

      // Construct the closure parameters
      val cs = if (cparams.isEmpty) {
        List(LiftedAst.FormalParam(Symbol.freshVarSym("_lift", BoundBy.FormalParam, loc), Ast.Modifiers.Empty, MonoType.Unit, loc))
      } else cparams.map(visitFormalParam)

      // Construct the formal parameters.
      val fs = fparams.map(visitFormalParam)

      // Construct a new definition.
      val defTpe = arrowTpe.result
      val defn = LiftedAst.Def(ann, mod, freshSymbol, cs, fs, liftedExp, defTpe, loc)

      // Add the new definition to the map of lifted definitions.
      ctx.liftedDefs.add(freshSymbol -> defn)

      // Construct the closure args.
      val closureArgs = if (freeVars.isEmpty)
        List(LiftedAst.Expr.Cst(Constant.Unit, MonoType.Unit, loc))
      else freeVars.map {
        case SimplifiedAst.FreeVar(sym, tpe) => LiftedAst.Expr.Var(sym, tpe, sym.loc)
      }

      // Construct the closure expression.
      LiftedAst.Expr.ApplyAtomic(AtomicOp.Closure(freshSymbol), closureArgs, arrowTpe, Purity.Pure, loc)

    case SimplifiedAst.Expr.ApplyAtomic(op, exps, tpe, purity, loc) =>
      val es = exps map visitExp
      LiftedAst.Expr.ApplyAtomic(op, es, tpe, purity, loc)

    case SimplifiedAst.Expr.ApplyClo(exp, exps, tpe, purity, loc) =>
      val e = visitExp(exp)
      val es = exps map visitExp
      LiftedAst.Expr.ApplyClo(e, es, tpe, purity, loc)

    case SimplifiedAst.Expr.ApplyDef(sym, exps, tpe, purity, loc) =>
      val es = exps map visitExp
      LiftedAst.Expr.ApplyDef(sym, es, tpe, purity, loc)

    case SimplifiedAst.Expr.IfThenElse(exp1, exp2, exp3, tpe, purity, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val e3 = visitExp(exp3)
      LiftedAst.Expr.IfThenElse(e1, e2, e3, tpe, purity, loc)

    case SimplifiedAst.Expr.Stm(exp1, exp2, tpe, purity, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      LiftedAst.Expr.Stm(e1, e2, tpe, purity, loc)

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

    case SimplifiedAst.Expr.NewObject(name, clazz, tpe, purity, methods0, loc) =>
      val methods = methods0.map(visitJvmMethod)
      LiftedAst.Expr.NewObject(name, clazz, tpe, purity, methods, loc)

    case SimplifiedAst.Expr.Def(_, _, loc) => throw InternalCompilerException(s"Unexpected expression.", loc)

    case SimplifiedAst.Expr.Lambda(_, _, _, loc) => throw InternalCompilerException(s"Unexpected expression.", loc)

    case SimplifiedAst.Expr.Apply(_, _, _, _, loc) => throw InternalCompilerException(s"Unexpected expression.", loc)
  }

  private def visitJvmMethod(method: SimplifiedAst.JvmMethod)(implicit sym0: Symbol.DefnSym, ctx: SharedContext, flix: Flix): LiftedAst.JvmMethod = method match {
    case SimplifiedAst.JvmMethod(ident, fparams0, exp, retTpe, purity, loc) =>
      val fparams = fparams0 map visitFormalParam
      LiftedAst.JvmMethod(ident, fparams, visitExp(exp), retTpe, purity, loc)
  }


  private def visitFormalParam(fparam: SimplifiedAst.FormalParam): LiftedAst.FormalParam = fparam match {
    case SimplifiedAst.FormalParam(sym, mod, tpe, loc) => LiftedAst.FormalParam(sym, mod, tpe, loc)
  }

  /**
    * A context shared across threads.
    *
    * We use a concurrent (non-blocking) linked queue to ensure thread-safety.
    */
  private case class SharedContext(liftedDefs: ConcurrentLinkedQueue[(Symbol.DefnSym, LiftedAst.Def)])

}
