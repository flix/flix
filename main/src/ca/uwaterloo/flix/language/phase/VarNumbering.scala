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
import ca.uwaterloo.flix.language.ast.LiftedAst._
import ca.uwaterloo.flix.language.ast.{Type, TypeConstructor}
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.{InternalCompilerException, Validation}

import scala.annotation.tailrec

/**
  * Assigns stack offsets to each variable symbol in the program.
  *
  * On the JVM, each method has a local variable array, and each variable is referenced by a 0-based offset. The first
  * few slots in the array are initialized to the values of the parameters. Normally each value takes up a single
  * slot, but longs and doubles require two consecutive slots. Thus, the n-th variable may not necessarily be the
  * n-th slot. This phase computes the specific offsets used by each formal parameter and local variable.
  */
object VarNumbering extends Phase[Root, Root] {

  /**
    * Assigns a stack offset to each variable symbol in the program.
    */
  def run(root: Root)(implicit flix: Flix): Validation[Root, CompilationError] = flix.phase("VarNumbering") {
    // Compute stack offset for each definition.
    for ((sym, defn) <- root.defs) {
      number(defn)
    }

    root.toSuccess
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
      case Expression.Unit => i0

      case Expression.Null(_) => i0

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

      case Expression.Closure(ref, freeVars, tpe, loc) => i0

      case Expression.ApplyClo(exp, args, tpe, loc) =>
        val i = visitExp(exp, i0)
        visitExps(args, i)

      case Expression.ApplyDef(sym, args, tpe, loc) => visitExps(args, i0)

      case Expression.ApplyCloTail(exp, args, tpe, loc) =>
        val i = visitExp(exp, i0)
        visitExps(args, i)

      case Expression.ApplyDefTail(sym, args, tpe, loc) => visitExps(args, i0)

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

      case Expression.Is(sym, tag, exp, loc) => visitExp(exp, i0)

      case Expression.Tag(enum, tag, exp, tpe, loc) => visitExp(exp, i0)

      case Expression.Untag(sym, tag, exp, tpe, loc) => visitExp(exp, i0)

      case Expression.Index(exp, index, tpe, loc) => visitExp(exp, i0)

      case Expression.Tuple(elms, tpe, loc) => visitExps(elms, i0)

      case Expression.RecordEmpty(tpe, loc) => i0

      case Expression.RecordSelect(base, _, tpe, loc) =>
        visitExp(base, i0)

      case Expression.RecordExtend(_, value, rest, tpe, loc) =>
        val i1 = visitExp(value, i0)
        val i2 = visitExp(rest, i1)
        i2

      case Expression.RecordRestrict(_, rest, tpe, loc) =>
        visitExp(rest, i0)

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

      case Expression.ArraySlice(base, startIndex, endIndex, tpe, loc) =>
        val i1 = visitExp(base, i0)
        val i2 = visitExp(startIndex, i1)
        visitExp(endIndex, i2)

      case Expression.Ref(exp, tpe, loc) => visitExp(exp, i0)

      case Expression.Deref(exp, tpe, loc) => visitExp(exp, i0)

      case Expression.Assign(exp1, exp2, tpe, loc) =>
        val i1 = visitExp(exp1, i0)
        visitExp(exp2, i1)

      case Expression.Existential(params, exp, loc) => visitExp(exp, i0)

      case Expression.Universal(params, exp, loc) => visitExp(exp, i0)

      case Expression.Cast(exp, tpe, loc) => visitExp(exp, i0)

      case Expression.TryCatch(exp, rules, tpe, loc) =>
        val i1 = visitExp(exp, i0)
        val i2 = i1 + 1
        for (CatchRule(sym, clazz, body) <- rules) {
          // NB: We reuse the same stack offset for each exception.
          sym.setStackOffset(i1)
        }
        visitExps(rules.map(_.exp), i2)

      case Expression.InvokeConstructor(constructor, args, tpe, loc) => visitExps(args, i0)

      case Expression.InvokeMethod(method, exp, args, tpe, loc) =>
        val i1 = visitExp(exp, i0)
        visitExps(args, i1)

      case Expression.InvokeStaticMethod(method, args, tpe, loc) => visitExps(args, i0)

      case Expression.GetField(field, exp, tpe, loc) => visitExp(exp, i0)

      case Expression.PutField(field, exp1, exp2, tpe, loc) =>
        val i1 = visitExp(exp1, i0)
        visitExp(exp2, i1)

      case Expression.GetStaticField(field, tpe, loc) => i0

      case Expression.PutStaticField(field, exp, tpe, loc) => visitExp(exp, i0)

      case Expression.NewChannel(exp, tpe, loc) =>
        visitExp(exp, i0)

      case Expression.GetChannel(exp, tpe, loc) =>
        visitExp(exp, i0)

      case Expression.PutChannel(exp1, exp2, tpe, loc) =>
        val i1 = visitExp(exp1, i0)
        visitExp(exp2, i1)

      case Expression.SelectChannel(rules, default, tpe, loc) =>
        var currentOffset = i0
        for (r <- rules) {
          // Set the stack offset for the symbol.
          r.sym.setStackOffset(currentOffset)

          // Compute the next free stack offset.
          val elementType = r.chan.tpe.typeArguments.head
          currentOffset = currentOffset + getStackSize(elementType)

          // Visit the channel expression of the rule.
          currentOffset = visitExp(r.chan, currentOffset)

          // Visit the expression of the rule.
          currentOffset = visitExp(r.exp, currentOffset)
        }
        default.map(visitExp(_, currentOffset)).getOrElse(currentOffset)

      case Expression.Spawn(exp, tpe, loc) =>
        visitExp(exp, i0)

      case Expression.Lazy(exp, tpe, loc) =>
        visitExp(exp, i0)

      case Expression.Force(exp, tpe, loc) =>
        visitExp(exp, i0)

      case Expression.FixpointConstraintSet(cs, tpe, loc) =>
        for (c <- cs) {
          visitConstraint(c)
        }
        i0

      case Expression.FixpointCompose(exp1, exp2, tpe, loc) =>
        val i1 = visitExp(exp1, i0)
        visitExp(exp2, i1)

      case Expression.FixpointSolve(exp, stf, tpe, loc) => visitExp(exp, i0)

      case Expression.FixpointProject(pred, exp, tpe, loc) =>
        visitExp(exp, i0)

      case Expression.FixpointEntails(exp1, exp2, tpe, loc) =>
        val i1 = visitExp(exp1, i0)
        visitExp(exp2, i1)

      case Expression.FixpointFold(pred, exp1, exp2, exp3, tpe, loc) =>
        val i1 = visitExp(exp1, i0)
        val i2 = visitExp(exp2, i1)
        visitExp(exp3, i2)

      case Expression.HoleError(sym, tpe, loc) => i0

      case Expression.MatchError(tpe, loc) => i0
    }

    /**
      * Returns the next available stack offset.
      */
    @tailrec
    def visitExps(es: List[Expression], i: Int): Int = es match {
      case Nil => i
      case x :: xs =>
        val i2 = visitExp(x, i)
        visitExps(xs, i2)
    }

    /**
      * Returns the next available stack offset.
      */
    def visitConstraint(c: Constraint): Unit = {
      // Assign a number to each constraint parameters.
      // These are unrelated to the true stack offsets.
      for ((cparam, index) <- c.cparams.zipWithIndex) {
        cparam match {
          case ConstraintParam.HeadParam(sym, _, _) => sym.setStackOffset(index)
          case ConstraintParam.RuleParam(sym, _, _) => sym.setStackOffset(index)
        }
      }
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
