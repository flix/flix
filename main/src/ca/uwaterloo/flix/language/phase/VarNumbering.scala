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
import ca.uwaterloo.flix.language.ast.ControlAst._
import ca.uwaterloo.flix.language.ast.{Symbol, Type, TypeConstructor}

import scala.annotation.tailrec

/**
  * Assigns stack offsets to each variable symbol in the program.
  *
  * On the JVM, each method has a local variable array, and each variable is referenced by a 0-based offset. The first
  * few slots in the array are initialized to the values of the parameters. Normally each value takes up a single
  * slot, but longs and doubles require two consecutive slots. Thus, the n-th variable may not necessarily be the
  * n-th slot. This phase computes the specific offsets used by each formal parameter and local variable.
  */
object VarNumbering {

  /**
    * Assigns a stack offset to each variable symbol in the program.
    */
  def run(root: Root)(implicit flix: Flix): Root = flix.phase("VarNumbering") {
    // Compute stack offset for each definition.
    for ((_, defn) <- root.defs) {
      number(defn)
    }

    root
  }

  /**
    * Assigns stack offsets to the given definition.
    *
    * Returns Unit since the variable symbols are mutated to store their stack offsets.
    */
  def number(defn: Def): Unit = {
    /**
      * Returns the next available stack offset.
      *
      * @param e0 the current expression.
      * @param i0 the current stack offset.
      */
    def visitExp(e0: Expression, i0: Int): Int = e0 match {
      case Expression.Cst(_, _, _) => i0

      case Expression.Var(_, _, _) => i0

      case Expression.Closure(_, args, _, _) =>
        visitExps(args, i0)

      case Expression.ApplyAtomic(_, exps, _, _, _) =>
        visitExps(exps, i0)

      case Expression.ApplyClo(exp, args, _, _, _) =>
        val i = visitExp(exp, i0)
        visitExps(args, i)

      case Expression.ApplyDef(_, args, _, _, _) =>
        visitExps(args, i0)

      case Expression.ApplyCloTail(exp, args, _, _, _) =>
        val i = visitExp(exp, i0)
        visitExps(args, i)

      case Expression.ApplyDefTail(_, args, _, _, _) =>
        visitExps(args, i0)

      case Expression.ApplySelfTail(_, _, args, _, _, _) =>
        visitExps(args, i0)

      case Expression.IfThenElse(exp1, exp2, exp3, _, _, _) =>
        val i1 = visitExp(exp1, i0)
        val i2 = visitExp(exp2, i1)
        visitExp(exp3, i2)

      case Expression.Branch(exp, branches, _, _, _) =>
        val i1 = visitExp(exp, i0)
        visitExps(branches.values.toList, i1)

      case Expression.JumpTo(_, _, _, _) =>
        i0

      case Expression.Let(sym, exp1, exp2, _, _, _) =>
        val i1 = visitSymbolAssignment(sym, exp1.tpe, i0)
        val i2 = visitExp(exp1, i1)
        visitExp(exp2, i2)

      case Expression.LetRec(varSym, _, _, exp1, exp2, _, _, _) =>
        val i1 = visitSymbolAssignment(varSym, exp1.tpe, i0)
        val i2 = visitExp(exp1, i1)
        visitExp(exp2, i2)

      case Expression.Region(_, _) =>
        i0

      case Expression.Scope(sym, exp, _, _, _) =>
        val i1 = visitSymbolAssignment(sym, Type.Unit, i0)
        visitExp(exp, i1)

      case Expression.ScopeExit(exp1, exp2, _, _, _) =>
        val i1 = visitExp(exp1, i0)
        visitExp(exp2, i1)

      case Expression.Is(_, exp, _, _) =>
        visitExp(exp, i0)

      case Expression.Tag(_, exp, _, _, _) =>
        visitExp(exp, i0)

      case Expression.Untag(_, exp, _, _, _) =>
        visitExp(exp, i0)

      case Expression.Index(exp, _, _, _, _) =>
        visitExp(exp, i0)

      case Expression.Tuple(elms, _, _, _) =>
        visitExps(elms, i0)

      case Expression.RecordEmpty(_, _) =>
        i0

      case Expression.RecordSelect(base, _, _, _, _) =>
        visitExp(base, i0)

      case Expression.RecordExtend(_, value, rest, _, _, _) =>
        val i1 = visitExp(value, i0)
        val i2 = visitExp(rest, i1)
        i2

      case Expression.RecordRestrict(_, rest, _, _, _) =>
        visitExp(rest, i0)

      case Expression.ArrayLit(elms, _, _) =>
        visitExps(elms, i0)

      case Expression.ArrayNew(elm, len, _, _) =>
        val i1 = visitExp(elm, i0)
        visitExp(len, i1)

      case Expression.ArrayLoad(base, index, _, _) =>
        val i1 = visitExp(base, i0)
        visitExp(index, i1)

      case Expression.ArrayStore(base, index, elm, _, _) =>
        val i1 = visitExp(base, i0)
        val i2 = visitExp(index, i1)
        visitExp(elm, i2)

      case Expression.ArrayLength(base, _, _, _) =>
        visitExp(base, i0)

      case Expression.Ref(exp, _, _) =>
        visitExp(exp, i0)

      case Expression.Deref(exp, _, _) =>
        visitExp(exp, i0)

      case Expression.Assign(exp1, exp2, _, _) =>
        val i1 = visitExp(exp1, i0)
        visitExp(exp2, i1)

      case Expression.InstanceOf(exp, _, _) =>
        visitExp(exp, i0)

      case Expression.Cast(exp, _, _, _) =>
        visitExp(exp, i0)

      case Expression.TryCatch(exp, rules, _, _, _) =>
        val i1 = visitExp(exp, i0)
        val i2 = i1 + 1
        for (CatchRule(sym, _, _) <- rules) {
          // NB: We reuse the same stack offset for each exception.
          sym.setStackOffset(i1)
        }
        visitExps(rules.map(_.exp), i2)

      case Expression.InvokeConstructor(_, args, _, _, _) =>
        visitExps(args, i0)

      case Expression.InvokeMethod(_, exp, args, _, _, _) =>
        val i1 = visitExp(exp, i0)
        visitExps(args, i1)

      case Expression.InvokeStaticMethod(_, args, _, _, _) =>
        visitExps(args, i0)

      case Expression.GetField(_, exp, _, _, _) =>
        visitExp(exp, i0)

      case Expression.PutField(_, exp1, exp2, _, _, _) =>
        val i1 = visitExp(exp1, i0)
        visitExp(exp2, i1)

      case Expression.GetStaticField(_, _, _, _) =>
        i0

      case Expression.PutStaticField(_, exp, _, _, _) =>
        visitExp(exp, i0)

      case Expression.NewObject(_, _, _, _, _, _) =>
        // TODO - think about this after we've worked out what's going on in lambda lifting for NewObject
        i0

      case Expression.Spawn(exp1, exp2, _, _) =>
        val i1 = visitExp(exp1, i0)
        visitExp(exp2, i1)

      case Expression.Lazy(exp, _, _) =>
        visitExp(exp, i0)

      case Expression.Force(exp, _, _) =>
        visitExp(exp, i0)

      case Expression.HoleError(_, _, _) =>
        i0

      case Expression.MatchError(_, _) =>
        i0
    }

    /**
      * Returns the next available stack offset.
      */
    @tailrec
    def visitExps(es: List[Expression], i0: Int): Int = es match {
      case Nil => i0
      case x :: xs =>
        val i2 = visitExp(x, i0)
        visitExps(xs, i2)
    }

    // Compute the stack offset for each formal parameter.
    var offset = 0
    for (FormalParam(sym, _, tpe, _) <- defn.fparams) {
      offset += visitSymbolAssignment(sym, tpe, offset)
    }

    // Compute stack offset for the body.
    visitExp(defn.exp, offset)
  }

  /**
    * assigns `sym` its offset and returns the next available stack offset.
    */
  private def visitSymbolAssignment(sym: Symbol.VarSym, tpe: Type, i0: Int): Int = {
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
  private def getStackSize(tpe: Type): Int = tpe.typeConstructor match {
    case Some(TypeConstructor.Int64) | Some(TypeConstructor.Float64) => 2
    case _ => 1
  }

}
