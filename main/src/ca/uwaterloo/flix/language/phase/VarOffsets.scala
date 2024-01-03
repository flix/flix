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
import ca.uwaterloo.flix.language.ast.ReducedAst._
import ca.uwaterloo.flix.language.ast.{MonoType, Symbol}
import ca.uwaterloo.flix.util.ParOps

import scala.annotation.tailrec

/**
  * Assigns stack offsets to each variable symbol in the program.
  *
  * On the JVM, each method has a local variable array, and each variable is referenced by a 0-based offset. The first
  * few slots in the array are initialized to the values of the parameters. Normally each value takes up a single
  * slot, but longs and doubles require two consecutive slots. Thus, the n-th variable may not necessarily be the
  * n-th slot. This phase computes the specific offsets used by each formal parameter and local variable.
  */
object VarOffsets {

  /**
    * Assigns a stack offset to each variable symbol in the program.
    */
  def run(root: Root)(implicit flix: Flix): Root = flix.phase("VarOffsets") {
    ParOps.parMapValues(root.defs)(visitDef)

    root
  }

  /**
    * Assigns stack offsets to the given definition.
    *
    * Returns Unit since the variable symbols are mutated to store their stack offsets.
    */
  private def visitDef(defn: Def): Unit = {
    // Compute the stack offset for each formal parameter.
    var offset = 0
    for (FormalParam(sym, _, tpe, _) <- (defn.cparams ++ defn.fparams)) {
      offset += setStackOffset(sym, tpe, offset)
    }

    // Compute stack offset for the body.
    visitStm(defn.stmt, offset)
  }

  private def visitStm(stmt: Stmt, i0: Int): Int = stmt match {
    case Stmt.Ret(e, _, _) => visitExp(e, i0)
  }

  private def visitExp(e0: Expr, i0: Int): Int = e0 match {
    case Expr.Cst(_, _, _) => i0

    case Expr.Var(_, _, _) => i0

    case Expr.ApplyAtomic(_, exps, _, _, _) =>
      visitExps(exps, i0)

    case Expr.ApplyClo(exp, args, _, _, _, _) =>
      val i = visitExp(exp, i0)
      visitExps(args, i)

    case Expr.ApplyDef(_, args, _, _, _, _) =>
      visitExps(args, i0)

    case Expr.ApplySelfTail(_, _, args, _, _, _) =>
      visitExps(args, i0)

    case Expr.IfThenElse(exp1, exp2, exp3, _, _, _) =>
      val i1 = visitExp(exp1, i0)
      val i2 = visitExp(exp2, i1)
      visitExp(exp3, i2)

    case Expr.Branch(exp, branches, _, _, _) =>
      val i1 = visitExp(exp, i0)
      visitExps(branches.values.toList, i1)

    case Expr.JumpTo(_, _, _, _) =>
      i0

    case Expr.Let(sym, exp1, exp2, _, _, _) =>
      val i1 = setStackOffset(sym, exp1.tpe, i0)
      val i2 = visitExp(exp1, i1)
      visitExp(exp2, i2)

    case Expr.LetRec(varSym, _, _, exp1, exp2, _, _, _) =>
      val i1 = setStackOffset(varSym, exp1.tpe, i0)
      val i2 = visitExp(exp1, i1)
      visitExp(exp2, i2)

    case Expr.Scope(sym, exp, _, _, _) =>
      val i1 = setStackOffset(sym, MonoType.Unit, i0)
      visitExp(exp, i1)

    case Expr.TryCatch(exp, rules, _, _, _) =>
      val i1 = visitExp(exp, i0)
      rules.foldLeft(i1) {
        case (i2, CatchRule(sym, _, exp)) =>
          val i3 = setStackOffset(sym, MonoType.Object, i2)
          visitExp(exp, i3)
      }

    case Expr.TryWith(exp, _, _, _, _, _) =>
      // The expressions in TryWith are not executed here (concretely they're
      // always closures) and should not have var offsets here.
      // They don't contain binders so visiting them does nothing.
      visitExp(exp, i0)

    case Expr.Do(_, exps, _, _, _) =>
      visitExps(exps, i0)

    case Expr.NewObject(_, _, _, _, _, _, _) =>
      // The expressions in NewObject are not executed here (concretely they're
      // always closures) and should not have var offsets here.
      // They don't contain binders so visiting them does nothing.
      i0

  }

  @tailrec
  private def visitExps(es: List[Expr], i0: Int): Int = es match {
    case Nil => i0
    case x :: xs =>
      val i2 = visitExp(x, i0)
      visitExps(xs, i2)
  }

  /**
    * Assigns `sym` its offset and returns the next available stack offset.
    *
    * Note: Uses mutation.
    */
  private def setStackOffset(sym: Symbol.VarSym, tpe: MonoType, i0: Int): Int = {
    // Set the stack offset for the symbol.
    sym.setStackOffset(i0)

    // Compute the next free stack offset.
    i0 + getStackSize(tpe)
  }

  /**
    * Returns the stack size used by the given type.
    *
    * A double or float uses two slots on the stack.
    * Everything else uses one slot.
    */
  private def getStackSize(tpe: MonoType): Int = tpe match {
    case MonoType.Float64 | MonoType.Int64 => 2
    case _ => 1
  }

}
