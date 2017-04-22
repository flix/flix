/*
 * Copyright 2017 Magnus Madsen
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
import ca.uwaterloo.flix.language.ast.{BinaryOperator, SimplifiedAst, Symbol}
import ca.uwaterloo.flix.language.ast.SimplifiedAst.Expression
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

/**
  * The Optimization phase performs intra-procedural optimizations.
  *
  * Specifically,
  *
  * - Elimination of run-time tag checks for Unit.
  * - Elimination of run-time tag checks of singleton-valued enums.
  * - Elimination of dead branches (e.g. if (true) e1 else e2).
  * - Copy propagation (e.g. let z = w; let y = z; let x = y; x -> w)
  * - Redundant branching (e.g. if(c1, if(c2, e2, e3), e3) -> if (c1 && c2, e2, e3))
  */
object Optimizer extends Phase[SimplifiedAst.Root, SimplifiedAst.Root] {

  /**
    * Returns an optimized version of the given AST `root`.
    */
  def run(root: SimplifiedAst.Root)(implicit flix: Flix): Validation[SimplifiedAst.Root, CompilationError] = {

    /**
      * A set containing all singleton-valued enums.
      */
    val singleCaseEnums: Set[Symbol.EnumSym] = root.enums.filter{case (sym, defn) => defn.cases.size <= 1}.keySet


    /**
      * Optimizes the given Expression.Binary `e0` by recursively optimizing its subexpressions.
      */
    def optimizeBinaryExp(e0: Expression.Binary): (Expression, Boolean) = e0 match {
      case Expression.Binary(op, exp1, exp2, tpe, loc) =>
        val (e1, b1) = optimizeExp(exp1)
        val (e2, b2) = optimizeExp(exp2)
        (Expression.Binary(op, e1, e2, tpe, loc), b1 || b2)
    }

    /**
      * Applies `optimizeExp` to each element in `es` and returns the results in a List.
      */
    def optimizeExps(es: List[Expression]): List[(Expression, Boolean)] = es.map(optimizeExp)

    /**
      * Returns a pair `(exp, bool)` where:
      *
      * - `exp` is the optimized version of the given Expression `e0`
      *
      * - `bool` is true iff some optimization was performed.
      */
    def optimizeExp(e0: Expression): (Expression, Boolean) = e0 match {
      case Expression.Unit => (Expression.Unit, false)
      case Expression.True => (Expression.True, false)
      case Expression.False => (Expression.False, false)
      case Expression.Char(lit) => (Expression.Char(lit), false)
      case Expression.Float32(lit) => (Expression.Float32(lit), false)
      case Expression.Float64(lit) => (Expression.Float64(lit), false)
      case Expression.Int8(lit) => (Expression.Int8(lit), false)
      case Expression.Int16(lit) => (Expression.Int16(lit), false)
      case Expression.Int32(lit) => (Expression.Int32(lit), false)
      case Expression.Int64(lit) => (Expression.Int64(lit), false)
      case Expression.BigInt(lit) => (Expression.BigInt(lit), false)
      case Expression.Str(lit) => (Expression.Str(lit), false)
      case Expression.LoadBool(exp, offset) =>
        val (e, b) = optimizeExp(exp)
        (Expression.LoadBool(e, offset), b)
      case Expression.LoadInt8(exp, offset) =>
        val (e, b) = optimizeExp(exp)
        (Expression.LoadInt8(e, offset), b)
      case Expression.LoadInt16(exp, offset) =>
        val (e, b) = optimizeExp(exp)
        (Expression.LoadInt16(e, offset), b)
      case Expression.LoadInt32(exp, offset) =>
        val (e, b) = optimizeExp(exp)
        (Expression.LoadInt32(e, offset), b)
      case Expression.StoreBool(exp, offset, v) =>
        val (e1, b1) = optimizeExp(exp)
        val (e2, b2) = optimizeExp(v)
        (Expression.StoreBool(e1, offset, e2), b1 || b2)
      case Expression.StoreInt8(exp, offset, v) =>
        val (e1, b1) = optimizeExp(exp)
        val (e2, b2) = optimizeExp(v)
        (Expression.StoreInt8(e1, offset, e2), b1 || b2)
      case Expression.StoreInt16(exp, offset, v) =>
        val (e1, b1) = optimizeExp(exp)
        val (e2, b2) = optimizeExp(v)
        (Expression.StoreInt16(e1, offset, e2), b1 || b2)
      case Expression.StoreInt32(exp, offset, v) =>
        val (e1, b1) = optimizeExp(exp)
        val (e2, b2) = optimizeExp(v)
        (Expression.StoreInt32(e1, offset, e2), b1 || b2)
      case Expression.Var(sym, tpe, loc) => (Expression.Var(sym, tpe, loc), false)
      case Expression.Ref(sym, tpe, loc) => (Expression.Ref(sym, tpe, loc), false)
      case Expression.Lambda(args, body, tpe, loc) =>
        val (e, b) = optimizeExp(body)
        (Expression.Lambda(args, e, tpe, loc), b)
      case Expression.Hook(hook, tpe, loc) => (Expression.Hook(hook, tpe, loc), false)
      case Expression.MkClosure(lambda, freeVars, tpe, loc) =>
        val (e, b) = optimizeExp(lambda)
        (Expression.MkClosure(e.asInstanceOf[Expression.Lambda], freeVars, tpe, loc), b)
      case Expression.MkClosureRef(ref, freeVars, tpe, loc) =>
        val (e, b) = optimizeExp(ref)
        (Expression.MkClosureRef(e.asInstanceOf[Expression.Ref], freeVars, tpe, loc), b)
      case Expression.ApplyRef(sym, args, tpe, loc) =>
        val (es, bs) = optimizeExps(args).unzip
        (Expression.ApplyRef(sym, es, tpe, loc), bs.exists(identity))
      case Expression.ApplyTail(sym, formals, actuals, tpe, loc) =>
        val (es, bs) = optimizeExps(actuals).unzip
        (Expression.ApplyTail(sym, formals, es, tpe, loc), bs.exists(identity))
      case Expression.ApplyHook(hook, args, tpe, loc) =>
        val (es, bs) = optimizeExps(args).unzip
        (Expression.ApplyHook(hook, es, tpe, loc), bs.exists(identity))
      case Expression.Apply(exp, args, tpe, loc) =>
        val (e, b) = optimizeExp(exp)
        val (es, bs) = optimizeExps(args).unzip
        (Expression.Apply(e, es, tpe, loc), bs.exists(identity) || b)
      case Expression.Unary(op, exp, tpe, loc) =>
        val (e, b) = optimizeExp(exp)
        (Expression.Unary(op, e, tpe, loc), b)
      case Expression.Binary(op, exp1, exp2, tpe, loc) =>
        // Elimination of run-time tag checks of singleton-valued enums.
        (op, exp1, exp2) match {
          case (BinaryOperator.Equal, Expression.Tag(sym, tag, exp, tp, lc), _) =>
            if (singleCaseEnums.contains(sym) && exp == Expression.Unit) (Expression.True, true)
            else optimizeBinaryExp(e0.asInstanceOf[Expression.Binary])
          case (BinaryOperator.Equal, _, Expression.Tag(sym, tag, exp, tp, lc)) =>
            if (singleCaseEnums.contains(sym) && exp == Expression.Unit) (Expression.True, true)
            else optimizeBinaryExp(e0.asInstanceOf[Expression.Binary])
          case _ => optimizeBinaryExp(e0.asInstanceOf[Expression.Binary])
        }
      case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
        // Elimination of dead branches (e.g. if (true) e1 else e2).
        exp1 match {
          case Expression.True => (optimizeExp(exp2)._1, true)
          case Expression.False => (optimizeExp(exp3)._1, true)
          case _ =>
            val (e1, b1) = optimizeExp(exp1)
            val (e2, b2) = optimizeExp(exp2)
            val (e3, b3) = optimizeExp(exp3)
            (Expression.IfThenElse(e1, e2, e3, tpe, loc), b1 || b2 || b3)
        }
      case Expression.Let(sym, exp1, exp2, tpe, loc) =>
        val (e1, b1) = optimizeExp(exp1)
        val (e2, b2) = optimizeExp(exp2)
        (Expression.Let(sym, e1, e2, tpe, loc), b1 || b2)
      case Expression.Is(exp, tag, loc) =>
        val (e, b) = optimizeExp(exp)
        (Expression.Is(e, tag, loc), b)
      case Expression.Tag(sym, tag, exp, tpe, loc) =>
        val (e, b) = optimizeExp(exp)
        (Expression.Tag(sym, tag, e, tpe, loc), b)
      case Expression.Untag(tag, exp, tpe, loc) =>
        val (e, b) = optimizeExp(exp)
        (Expression.Untag(tag, e, tpe, loc), b)
      case Expression.Index(base, offset, tpe, loc) =>
        val (e, b) = optimizeExp(base)
        (Expression.Index(e, offset, tpe, loc), b)
      case Expression.Tuple(elms, tpe, loc) =>
        val (es, bs) = optimizeExps(elms).unzip
        (Expression.Tuple(es, tpe, loc), bs.exists(identity))
      case Expression.Existential(fparam, exp, loc) =>
        val (e, b) = optimizeExp(exp)
        (Expression.Existential(fparam, e, loc), b)
      case Expression.Universal(fparam, exp, loc) =>
        val (e, b) = optimizeExp(exp)
        (Expression.Universal(fparam, e, loc), b)
      case Expression.NativeField(field, tpe, loc) => (Expression.NativeField(field, tpe, loc), false)
      case Expression.NativeMethod(method, args, tpe, loc) =>
        val (es, bs) = optimizeExps(args).unzip
        (Expression.NativeMethod(method, es, tpe, loc), bs.exists(identity))
      case Expression.UserError(tpe, loc) => (Expression.UserError(tpe, loc), false)
      case Expression.MatchError(tpe, loc) => (Expression.MatchError(tpe, loc), false)
      case Expression.SwitchError(tpe, loc) => (Expression.SwitchError(tpe, loc), false)
    }

    def optimizeDefns(ds: Map[Symbol.DefnSym, SimplifiedAst.Definition.Constant]):
      Map[Symbol.DefnSym, SimplifiedAst.Definition.Constant] = {

      var defns = ds
      var changing = true

      while (changing) {
        val (optDefns, bs) = defns.map {
          case (sym, defn) =>
            val (e, b) = optimizeExp(defn.exp)
            ((sym, defn.copy(exp = e)), b)
        }.unzip
        changing = bs.exists(identity)
        defns = optDefns.toMap
      }

      defns
    }

    // Start the timer.
    val t = System.nanoTime()

    val definitions = optimizeDefns(root.definitions)

    // Calculate the elapsed time.
    val e = System.nanoTime() - t

    // Reassemble the AST.
    root.copy(
      definitions = definitions,
      time = root.time.copy(optimizer = e)
    ).toSuccess
  }
}
