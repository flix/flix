/*
 * Copyright 2016 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{Purity, Symbol}
import ca.uwaterloo.flix.language.ast.ReducedAst.*
import ca.uwaterloo.flix.language.ast.shared.ExpPosition
import ca.uwaterloo.flix.language.dbg.AstPrinter.DebugReducedAst
import ca.uwaterloo.flix.util.ParOps
import ca.uwaterloo.flix.util.collection.MapOps

/**
  * The TailPos phase identifies function calls and try-with expressions that are in tail position,
  * and marks tail-recursive calls.
  *
  * Specifically, it replaces [[Expr.ApplyDef]] AST nodes with [[Expr.ApplySelfTail]] AST nodes
  * when the [[Expr.ApplyDef]] node calls the enclosing function and occurs in tail position.
  *
  * Otherwise, it adds [[ExpPosition.Tail]] to function calls and try-with expressions in tail
  * position.
  *
  * For correctness it is assumed that all calls in the given AST have [[ExpPosition.NonTail]]
  * and there are no [[Expr.ApplySelfTail]] nodes present.
  */
object TailPos {

  /** Identifies expressions in tail position in `root`. */
  def run(root: Root)(implicit flix: Flix): Root = flix.phase("TailPos") {
    val defns = ParOps.parMapValues(root.defs)(defn => flix.profile(defn.sym, defn.loc)(visitDef(defn)))
    val clos = ParOps.parMapValues(root.clos)(clo => flix.profile(clo.sym, clo.loc)(visitClo(clo)))
    root.copy(defs = defns, clos = clos)
  }

  /** Identifies expressions in tail position in `defn`. */
  private def visitDef(defn: Def): Def = {
    defn.copy(exp = visitExp(defn.exp)(defn.sym, defn.exp.purity))
  }

  /** Identifies expressions in tail position in `clo`. */
  private def visitClo(clo: Clo): Clo = {
    clo.copy(exp = visitExp(clo.exp)(clo.sym, clo.exp.purity))
  }

  /**
    * Introduces expressions in tail position in `exp0`.
    *
    * Replaces every [[Expr.ApplyDef]] that calls the enclosing function and occurs in tail
    * position with [[Expr.ApplySelfTail]].
    *
    * The enclosing function or closure has symbol `sym0` and its body has purity `purity0`.
    */
  private def visitExp(exp0: Expr)(implicit sym0: Symbol.DefnSym, purity0: Purity): Expr = exp0 match {
    case Expr.Let(sym, exp1, exp2, loc) =>
      // `exp2` is in tail position.
      val e2 = visitExp(exp2)
      Expr.Let(sym, exp1, e2, loc)

    case Expr.Stm(exps, exp, loc) =>
      // `exp` is in tail position.
      val e = visitExp(exp)
      Expr.Stm(exps, e, loc)

    case Expr.IfThenElse(exp1, exp2, exp3, tpe, purity, loc) =>
      // The branches are in tail position.
      val e2 = visitExp(exp2)
      val e3 = visitExp(exp3)
      Expr.IfThenElse(exp1, e2, e3, tpe, purity, loc)

    case Expr.Branch(e0, br0, tpe, purity, loc) =>
      // Each branch is in tail position.
      val br = MapOps.mapValues(br0)(visitExp)
      Expr.Branch(e0, br, tpe, purity, loc)

    case Expr.Switch(e0, enumSym, cases0, default0, tpe, purity, loc) =>
      // Each case and default is in tail position.
      val cs = cases0.map { case (sym, body) => (sym, visitExp(body)) }
      val d = visitExp(default0)
      Expr.Switch(e0, enumSym, cs, d, tpe, purity, loc)

    case Expr.ApplyClo(exp, exps, _, tpe, purity, loc) =>
      // Mark expression as tail position.
      Expr.ApplyClo(exp, exps, ExpPosition.Tail, tpe, purity, loc)

    case Expr.ApplyDef(sym, exps, _, tpe, purity, loc) =>
      // Check whether this is a self recursive call.
      if (sym0 != sym) {
        // Mark expression as tail position.
        Expr.ApplyDef(sym, exps, ExpPosition.Tail, tpe, purity, loc)
      } else if (!Purity.isControlPure(purity0)) {
        // Self-recursive tail call in a control-impure function. Do NOT rewrite to
        // ApplySelfTail: that optimization mutates the enclosing function object's
        // arg/pc/lparam fields and jumps to the entry label. When the function may
        // suspend, the function object is captured (via `copy`) as a continuation
        // Frame in `newFrame`. Multi-shot resumption re-invokes that same Frame
        // instance, so the mutations performed by the first resume corrupt the
        // snapshot used by subsequent resumes -- in particular, setting pc back to 0
        // makes the next resume miss its suspension point entirely. See issue
        // https://github.com/flix/flix/issues/12765.
        Expr.ApplyDef(sym, exps, ExpPosition.Tail, tpe, purity, loc)
      } else {
        // Self recursive tail call.
        Expr.ApplySelfTail(sym, exps, tpe, purity, loc)
      }

    case Expr.RunWith(exp, effUse, rules, _, tpe, purity, loc) =>
      // Mark expression as tail position.
      Expr.RunWith(exp, effUse, rules, ExpPosition.Tail, tpe, purity, loc)

    // Expressions that do not need ExpPosition marking and do not have sub-expression in tail
    // position.
    case Expr.ApplyOp(_, _, _, _, _) => exp0
    case Expr.ApplyAtomic(_, _, _, _, _) => exp0
    case Expr.ApplySelfTail(_, _, _, _, _) => exp0
    case Expr.Cst(_, _) => exp0
    case Expr.JumpTo(_, _, _, _) => exp0
    case Expr.NewObject(_, _, _, _, _, _, _) => exp0
    case Expr.Region(_, _, _, _, _) => exp0
    case Expr.TryCatch(_, _, _, _, _) => exp0
    case Expr.Var(_, _, _) => exp0
  }

}
