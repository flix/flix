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
import ca.uwaterloo.flix.language.ast.shared.*
import ca.uwaterloo.flix.language.ast.{AtomicOp, LiftedAst, MonoType, Purity, SimplifiedAst, Symbol}
import ca.uwaterloo.flix.language.dbg.AstPrinter.*
import ca.uwaterloo.flix.util.collection.MapOps
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps}

import java.util.concurrent.ConcurrentLinkedQueue
import scala.jdk.CollectionConverters.*

object LambdaLift {

  // We are safe to use the top scope everywhere because we do not use unification in this or future phases.
  private implicit val S: Scope = Scope.Top

  /**
    * Performs lambda lifting on the given AST `root`.
    */
  def run(root: SimplifiedAst.Root)(implicit flix: Flix): LiftedAst.Root = flix.phase("LambdaLift") {
    implicit val sctx: SharedContext = SharedContext.mk()

    val defs = ParOps.parMapValues(root.defs)(visitDef)
    val enums = ParOps.parMapValues(root.enums)(visitEnum)
    val structs = ParOps.parMapValues(root.structs)(visitStruct)
    val effects = ParOps.parMapValues(root.effects)(visitEffect)

    // Add lifted defs from the shared context to the existing defs.
    val newDefs = sctx.liftedDefs.asScala.foldLeft(defs) {
      case (macc, (sym, defn)) => macc + (sym -> defn)
    }

    LiftedAst.Root(newDefs, enums, structs, effects, root.mainEntryPoint, root.entryPoints, root.sources)
  }

  private def visitDef(def0: SimplifiedAst.Def)(implicit sctx: SharedContext, flix: Flix): LiftedAst.Def = def0 match {
    case SimplifiedAst.Def(ann, mod, sym, fparams, exp, tpe, _, loc) =>
      val fs = fparams.map(visitFormalParam)
      val e = visitExp(exp)(sym, Map.empty, sctx, flix)
      LiftedAst.Def(ann, mod, sym, Nil, fs, e, tpe, loc)
  }

  private def visitEnum(enum0: SimplifiedAst.Enum): LiftedAst.Enum = enum0 match {
    case SimplifiedAst.Enum(ann, mod, sym, tparams0, cases0, loc) =>
      val tparams = tparams0.map(param => LiftedAst.TypeParam(param.name, param.sym, param.loc))
      val cases = MapOps.mapValues(cases0)(visitEnumCase)
      LiftedAst.Enum(ann, mod, sym, tparams, cases, loc)
  }

  private def visitEnumCase(caze: SimplifiedAst.Case): LiftedAst.Case = caze match {
    case SimplifiedAst.Case(sym, tpes, loc) => LiftedAst.Case(sym, tpes, loc)
  }

  private def visitStruct(struct0: SimplifiedAst.Struct): LiftedAst.Struct = struct0 match {
    case SimplifiedAst.Struct(ann, mod, sym, tparams0, fields0, loc) =>
      val tparams = tparams0.map(param => LiftedAst.TypeParam(param.name, param.sym, param.loc))
      val fields = fields0.map(visitStructField)
      LiftedAst.Struct(ann, mod, sym, tparams, fields, loc)
  }

  private def visitStructField(field: SimplifiedAst.StructField): LiftedAst.StructField = field match {
    case SimplifiedAst.StructField(sym, tpe, loc) => LiftedAst.StructField(sym, tpe, loc)
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

  private def visitExp(e: SimplifiedAst.Expr)(implicit sym0: Symbol.DefnSym, liftedLocalDefs: Map[Symbol.VarSym, Symbol.DefnSym], sctx: SharedContext, flix: Flix): LiftedAst.Expr = e match {
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
      val ann = Annotations.Empty
      val mod = Modifiers(Modifier.Synthetic :: Nil)

      // Construct the closure parameters
      val cs = if (cparams.isEmpty) {
        List(LiftedAst.FormalParam(Symbol.freshVarSym("_lift", BoundBy.FormalParam, loc), Modifiers.Empty, MonoType.Unit, loc))
      } else cparams.map(visitFormalParam)

      // Construct the formal parameters.
      val fs = fparams.map(visitFormalParam)

      // Construct a new definition.
      val defTpe = arrowTpe.result
      val defn = LiftedAst.Def(ann, mod, freshSymbol, cs, fs, liftedExp, defTpe, loc)

      // Add the new definition to the map of lifted definitions.
      sctx.liftedDefs.add(freshSymbol -> defn)

      // Construct the closure args.
      val closureArgs = if (freeVars.isEmpty)
        List(LiftedAst.Expr.Cst(Constant.Unit, MonoType.Unit, loc))
      else freeVars.map {
        case SimplifiedAst.FreeVar(sym, tpe) => LiftedAst.Expr.Var(sym, tpe, sym.loc)
      }

      // Construct the closure expression.
      LiftedAst.Expr.ApplyAtomic(AtomicOp.Closure(freshSymbol), closureArgs, arrowTpe, Purity.Pure, loc)

    case SimplifiedAst.Expr.ApplyAtomic(op, exps, tpe, purity, loc) =>
      val es = exps.map(visitExp)
      LiftedAst.Expr.ApplyAtomic(op, es, tpe, purity, loc)

    case SimplifiedAst.Expr.ApplyClo(exp1, exp2, tpe, purity, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      LiftedAst.Expr.ApplyClo(e1, e2, tpe, purity, loc)

    case SimplifiedAst.Expr.ApplyDef(sym, exps, tpe, purity, loc) =>
      val es = exps.map(visitExp)
      LiftedAst.Expr.ApplyDef(sym, es, tpe, purity, loc)

    case SimplifiedAst.Expr.ApplyLocalDef(sym, exps, tpe, purity, loc) =>
      val es = exps.map(visitExp)
      val newDefnSym = liftedLocalDefs.get(sym)
      newDefnSym match {
        case Some(defnSym) => LiftedAst.Expr.ApplyDef(defnSym, es, tpe, purity, loc)
        case None => throw InternalCompilerException(s"unable to find lifted def for local def $sym", loc)
      }

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

    case SimplifiedAst.Expr.LocalDef(sym, fparams, exp1, exp2, _, _, loc) =>
      val freshDefnSym = Symbol.freshDefnSym(sym0)
      val updatedLiftedLocalDefs = liftedLocalDefs + (sym -> freshDefnSym)
      // It is **very import** we add the mapping `sym -> freshDefnSym` to liftedLocalDefs
      // before visiting the body since exp1 may contain recursive calls to `sym`
      // so they need to be substituted for `freshDefnSym` in `exp1` which
      // `visitExp` handles for us.
      val body = visitExp(exp1)(sym0, updatedLiftedLocalDefs, sctx, flix)
      val ann = Annotations.Empty
      val mod = Modifiers(Modifier.Synthetic :: Nil)
      val fps = fparams.map(visitFormalParam)
      val defTpe = exp1.tpe
      val liftedDef = LiftedAst.Def(ann, mod, freshDefnSym, List.empty, fps, body, defTpe, loc.asSynthetic)
      sctx.liftedDefs.add(freshDefnSym -> liftedDef)
      visitExp(exp2)(sym0, updatedLiftedLocalDefs, sctx, flix) // LocalDef node is erased here

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

    case SimplifiedAst.Expr.Lambda(_, _, _, loc) => throw InternalCompilerException(s"Unexpected expression.", loc)

  }

  private def visitJvmMethod(method: SimplifiedAst.JvmMethod)(implicit sym0: Symbol.DefnSym, liftedLocalDefs: Map[Symbol.VarSym, Symbol.DefnSym], sctx: SharedContext, flix: Flix): LiftedAst.JvmMethod = method match {
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

  private object SharedContext {

    /**
      * Returns a fresh shared context.
      */
    def mk(): SharedContext = SharedContext(new ConcurrentLinkedQueue())
  }

}
