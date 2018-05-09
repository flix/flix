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
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.SimplifiedAst._
import ca.uwaterloo.flix.language.ast.{SimplifiedAst, Type}
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.{InternalCompilerException, Validation}

/**
  * Assigns stack offsets to each variable symbol in the program.
  *
  * On the JVM, each method has a local variable array, and each variable is referenced by a 0-based offset. The first
  * few slots in the array are initialized to the values of the parameters. Normally each value takes up a single
  * slot, but longs and doubles require two consecutive slots. Thus, the n-th variable may not necessarily be the
  * n-th slot. This phase computes the specific offsets used by each formal parameter and local variable.
  */
object VarNumbering extends Phase[SimplifiedAst.Root, SimplifiedAst.Root] {

  /**
    * Assigns a stack offset to each variable symbol in the program.
    */
  def run(root: SimplifiedAst.Root)(implicit flix: Flix): Validation[SimplifiedAst.Root, CompilationError] = {
    val t = System.nanoTime()

    // Compute stack offset for each definition.
    for ((sym, defn) <- root.defs) {
      number(defn)
    }

    // Compute offset for each constraint.
    for (strata <- root.strata) {
      for (constraint <- strata.constraints) {
        number(constraint)
      }
    }

    val e = System.nanoTime() - t
    root.copy(time = root.time.copy(varNumbering = e)).toSuccess
  }

  /**
    * Assigns stack offsets to the given definition.
    *
    * Returns Unit since the variable symbols are mutated to store their stack offsets.
    */
  def number(defn: SimplifiedAst.Def): Unit = {
    /**
      * Returns the next available stack offset.
      *
      * @param e0 the current expression.
      * @param i0 the current stack offset.
      */
    def visitExp(e0: Expression, i0: Int): Int = e0 match {
      case Expression.Unit => i0
      case Expression.True => i0
      case Expression.False => i0
      case Expression.Char(lit) => i0
      case Expression.Float32(lit) => i0
      case Expression.Float64(lit) => i0
      case Expression.Int8(lit) => i0
      case Expression.Int16(lit) => i0
      case Expression.Int32(lit) => i0
      case Expression.Int64(lit) => i0
      case Expression.BigInt(lit) => i0
      case Expression.Str(lit) => i0
      case Expression.Var(sym, tpe, loc) => i0
      case Expression.Def(sym, tpe, loc) => i0
      case Expression.Eff(sym, tpe, loc) => i0
      case Expression.Closure(ref, freeVars, tpe, loc) => i0
      case Expression.ApplyClo(exp, args, tpe, loc) =>
        val i = visitExp(exp, i0)
        visitExps(args, i)
      case Expression.ApplyDef(sym, args, tpe, loc) => visitExps(args, i0)
      case Expression.ApplyEff(sym, args, tpe, loc) => visitExps(args, i0)
      case Expression.ApplyCloTail(exp, args, tpe, loc) =>
        val i = visitExp(exp, i0)
        visitExps(args, i)
      case Expression.ApplyDefTail(sym, args, tpe, loc) => visitExps(args, i0)
      case Expression.ApplyEffTail(sym, args, tpe, loc) => visitExps(args, i0)
      case Expression.ApplySelfTail(sym, formals, args, tpe, loc) => visitExps(args, i0)
      case Expression.Unary(sop, op, exp, tpe, loc) => visitExp(exp, i0)
      case Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
        val i1 = visitExp(exp1, i0)
        visitExp(exp2, i1)

      case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
        val i1 = visitExp(exp1, i0)
        val i2 = visitExp(exp2, i1)
        visitExp(exp3, i2)

      case Expression.Branch(exp, branches, tpe, loc) =>
        val i1 = visitExp(exp, i0)
        visitExps(branches.values.toList, i1)

      case Expression.JumpTo(sym, tpe, loc) =>
        i0

      case Expression.Let(sym, exp1, exp2, tpe, loc) =>
        // Set the stack offset for the symbol.
        sym.setStackOffset(i0)

        // Compute the next free stack offset.
        val i1 = i0 + getStackSize(exp1.tpe)

        // Visit the let-bound value expression.
        val i2 = visitExp(exp1, i1)

        // Visit the let-body expression.
        visitExp(exp2, i2)

      case Expression.LetRec(sym, exp1, exp2, tpe, loc) =>
        // Set the stack offset for the symbol.
        sym.setStackOffset(i0)

        // Compute the next free stack offset.
        val i1 = i0 + getStackSize(exp1.tpe)

        // Visit the let-bound value expression.
        val i2 = visitExp(exp1, i1)

        // Visit the let-body expression.
        visitExp(exp2, i2)

      case Expression.Is(sym, tag, exp, loc) => visitExp(exp, i0)
      case Expression.Tag(enum, tag, exp, tpe, loc) => visitExp(exp, i0)
      case Expression.Untag(sym, tag, exp, tpe, loc) => visitExp(exp, i0)
      case Expression.Index(exp, index, tpe, loc) => visitExp(exp, i0)
      case Expression.Tuple(elms, tpe, loc) => visitExps(elms, i0)

      case Expression.ArrayLit(elms, tpe, loc) => visitExps(elms, i0)

      case Expression.ArrayNew(elm, len, tpe, loc) =>
        val i1 = visitExp(elm, i0)
        visitExp(len, i1)

      case Expression.ArrayLoad(base, index, tpe, loc) =>
        val i1 = visitExp(base, i0)
        visitExp(index, i1)

      case Expression.ArrayStore(base, index, elm, tpe, loc) =>
        val i1 = visitExp(base, i0)
        val i2 = visitExp(index, i1)
        visitExp(elm, i2)

      case Expression.ArrayLength(base, tpe, loc) => visitExp(base, i0)

      case Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) =>
        val i1 = visitExp(base, i0)
        val i2 = visitExp(beginIndex, i1)
        visitExp(endIndex, i2)

      case Expression.Ref(exp, tpe, loc) => visitExp(exp, i0)
      case Expression.Deref(exp, tpe, loc) => visitExp(exp, i0)
      case Expression.Assign(exp1, exp2, tpe, loc) =>
        val i1 = visitExp(exp1, i0)
        visitExp(exp2, i1)
      case Expression.HandleWith(exp, bindings, tpe, loc) =>
        val i1 = visitExp(exp, i0)
        visitExps(bindings.map(_.exp), i1)
      case Expression.Existential(params, exp, loc) => visitExp(exp, i0)
      case Expression.Universal(params, exp, loc) => visitExp(exp, i0)
      case Expression.NativeConstructor(constructor, args, tpe, loc) => visitExps(args, i0)
      case Expression.NativeField(field, tpe, loc) => i0
      case Expression.NativeMethod(method, args, tpe, loc) => visitExps(args, i0)

      case Expression.UserError(tpe, loc) => i0
      case Expression.HoleError(sym, tpe, eff, loc) => i0
      case Expression.MatchError(tpe, loc) => i0
      case Expression.SwitchError(tpe, loc) => i0

      case Expression.Lambda(args, body, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${e0.getClass.getSimpleName}'.")
      case Expression.LambdaClosure(lambda, freeVars, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${e0.getClass.getSimpleName}'.")
      case Expression.Apply(exp, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${e0.getClass.getSimpleName}'.")
    }

    /**
      * Returns the next available stack offset.
      */
    def visitExps(es: List[Expression], i: Int): Int = es match {
      case Nil => i
      case x :: xs =>
        val i2 = visitExp(x, i)
        visitExps(xs, i2)
    }

    // Compute the stack offset for each formal parameter.
    var offset = 0
    for (FormalParam(sym, mod, tpe, loc) <- defn.fparams) {
      // Set the stack offset for the symbol.
      sym.setStackOffset(offset)

      // Update the next available stack offset.
      offset += getStackSize(tpe)
    }

    // Compute stack offset for the body.
    visitExp(defn.exp, offset)
  }

  /**
    * Assign an offset to each constraint parameter in the given constraint `c`.
    */
  def number(c: Constraint): Unit = {
    var offset = 0
    for (cparam <- c.cparams) {
      cparam match {
        case ConstraintParam.HeadParam(sym, tpe, loc) =>
          sym.setStackOffset(offset)
          offset = offset + 1
        case ConstraintParam.RuleParam(sym, tpe, loc) =>
          sym.setStackOffset(offset)
          offset = offset + 1
      }
    }
  }

  /**
    * Returns the stack size used by the given type.
    *
    * A double or float uses two slots on the stack.
    * Everything else uses one slot.
    */
  private def getStackSize(tpe: Type): Int = tpe match {
    case Type.Int64 | Type.Float64 => 2
    case _ => 1
  }

}
