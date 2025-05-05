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
import ca.uwaterloo.flix.language.ast.ReducedAst.*
import ca.uwaterloo.flix.language.ast.{MonoType, Symbol}
import ca.uwaterloo.flix.language.dbg.AstPrinter.DebugReducedAst
import ca.uwaterloo.flix.util.ParOps

/**
  * Assigns stack offsets to each variable binder in the program by mutating the symbols.
  *
  * On the JVM, each method has a local variable array, and each variable is referenced by a 0-based
  * offset. The first slots are used for parameters. 64-bit types require two consecutive slots.
  * Thus, the n-th variable may not necessarily be the n-th slot. This phase computes the specific
  * offsets used by each formal parameter and local variable.
  *
  * This is an offset and not an index, since the first slot might actuallu be taken by the instance
  * object if the code is generated in an instance function.
  */
object VarOffsets {

  /** Assigns a stack offset to each variable binder in the program. */
  def run(root: Root)(implicit flix: Flix): Root = flix.phase("VarOffsets") {
    ParOps.parMapValues(root.defs)(visitDef)

    root
  }

  /** Assigns stack offsets to `defn`. */
  private def visitDef(defn: Def): Unit = {
    var offset = 0
    for (FormalParam(sym, _, tpe, _) <- defn.cparams ++ defn.fparams) {
      offset += setStackOffset(sym, tpe, offset)
    }

    visitExp(defn.expr, offset)
  }

  /** Assigns stack offsets to `exp0` and returns the next available stack offset. */
  private def visitExp(exp0: Expr, offset0: Int): Int = exp0 match {
    case Expr.Cst(_, _, _) =>
      offset0

    case Expr.Var(_, _, _) =>
      offset0

    case Expr.ApplyAtomic(_, exps, _, _, _) =>
      visitExps(exps, offset0)

    case Expr.ApplyClo(exp1, exp2, _, _, _, _) =>
      var offset = offset0
      offset = visitExp(exp1, offset)
      visitExp(exp2, offset)

    case Expr.ApplyDef(_, args, _, _, _, _) =>
      visitExps(args, offset0)

    case Expr.ApplySelfTail(_, args, _, _, _) =>
      visitExps(args, offset0)

    case Expr.IfThenElse(exp1, exp2, exp3, _, _, _) =>
      var offset = offset0
      offset = visitExp(exp1, offset)
      offset = visitExp(exp2, offset)
      visitExp(exp3, offset)

    case Expr.Branch(exp, branches, _, _, _) =>
      var offset = offset0
      offset = visitExp(exp, offset)
      visitExps(branches.values.toList, offset)

    case Expr.JumpTo(_, _, _, _) =>
      offset0

    case Expr.Let(sym, exp1, exp2, _, _, _) =>
      var offset = offset0
      offset = setStackOffset(sym, exp1.tpe, offset)
      offset = visitExp(exp1, offset)
      visitExp(exp2, offset)

    case Expr.Stmt(exp1, exp2, _, _, _) =>
      var offset = offset0
      offset = visitExp(exp1, offset)
      visitExp(exp2, offset)

    case Expr.Scope(sym, exp, _, _, _) =>
      var offset = offset0
      offset = setStackOffset(sym, MonoType.Region, offset)
      visitExp(exp, offset)

    case Expr.TryCatch(exp, rules, _, _, _) =>
      var offset = offset0
      offset = visitExp(exp, offset)
      for (CatchRule(sym, _, body) <- rules) {
        offset = setStackOffset(sym, MonoType.Object, offset)
        offset = visitExp(body, offset)
      }
      offset

    case Expr.RunWith(exp, _, _, _, _, _, _) =>
      // The expressions in RunWith are not executed here (concretely they're always closures) and
      // should not have var offsets here. They don't contain binders so visiting them does nothing.
      visitExp(exp, offset0)

    case Expr.Do(_, exps, _, _, _) =>
      visitExps(exps, offset0)

    case Expr.NewObject(_, _, _, _, _, _) =>
      // The expressions in NewObject are not executed here (concretely they're always closures) and
      // should not have var offsets here. They don't contain binders so visiting them does nothing.
      offset0

  }

  /** Assigns stack offsets to `exps` and returns the next available stack offset. */
  private def visitExps(exps: List[Expr], offset0: Int): Int = {
    var offset = offset0
    for (exp <- exps) {
      offset = visitExp(exp, offset)
    }
    offset
  }

  /** Assigns stack offset to `sym` and returns the next available stack offset. */
  private def setStackOffset(sym: Symbol.VarSym, tpe: MonoType, offset: Int): Int = {
    // Set the stack offset for the symbol.
    sym.setStackOffset(offset)

    // Compute the next free stack offset.
    offset + getStackSize(tpe)
  }

  /** Returns the stack slots used by `tpe`. */
  private def getStackSize(tpe: MonoType): Int = tpe match {
    case MonoType.Float64 | MonoType.Int64 => 2
    case _ => 1
  }

}
