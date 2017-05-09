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
      * Returns `true` if and only if `enum` is a singleton-valued enum.
      */
    def isSingleCaseEnum(enum: Symbol.EnumSym): Boolean = root.enums.filter{case (sym, defn) => defn.cases.size <= 1}.contains(enum)

    /**
      * Performs a single pass optimization on each element in `es`.
      *
      * That is, applies `optimize` to each element in `es` and returns the results in a List.
      */
    def optimizeExps(es: List[Expression]): List[Expression] = es.map(optimize)

    /**
      * Performs a single pass optimization on the given Expression `e0`.
      */
    def optimize(e0: Expression): Expression = e0 match {
      case Expression.Unit => Expression.Unit
      case Expression.True => Expression.True
      case Expression.False => Expression.False
      case Expression.Char(lit) => Expression.Char(lit)
      case Expression.Float32(lit) => Expression.Float32(lit)
      case Expression.Float64(lit) => Expression.Float64(lit)
      case Expression.Int8(lit) => Expression.Int8(lit)
      case Expression.Int16(lit) => Expression.Int16(lit)
      case Expression.Int32(lit) => Expression.Int32(lit)
      case Expression.Int64(lit) => Expression.Int64(lit)
      case Expression.BigInt(lit) => Expression.BigInt(lit)
      case Expression.Str(lit) => Expression.Str(lit)
      case Expression.LoadBool(exp, offset) => Expression.LoadBool(optimize(exp), offset)
      case Expression.LoadInt8(exp, offset) => Expression.LoadInt8(optimize(exp), offset)
      case Expression.LoadInt16(exp, offset) => Expression.LoadInt16(optimize(exp), offset)
      case Expression.LoadInt32(exp, offset) => Expression.LoadInt32(optimize(exp), offset)
      case Expression.StoreBool(exp, offset, v) => Expression.StoreBool(optimize(exp), offset, optimize(v))
      case Expression.StoreInt8(exp, offset, v) => Expression.StoreInt8(optimize(exp), offset, optimize(v))
      case Expression.StoreInt16(exp, offset, v) => Expression.StoreInt16(optimize(exp), offset, optimize(v))
      case Expression.StoreInt32(exp, offset, v) => Expression.StoreInt32(optimize(exp), offset, optimize(v))
      case Expression.Var(sym, tpe, loc) => Expression.Var(sym, tpe, loc)
      case Expression.Ref(sym, tpe, loc) => Expression.Ref(sym, tpe, loc)
      case Expression.Lambda(args, body, tpe, loc) => Expression.Lambda(args, optimize(body), tpe, loc)
      case Expression.Hook(hook, tpe, loc) => Expression.Hook(hook, tpe, loc)
      case Expression.MkClosure(lambda, freeVars, tpe, loc) =>
        Expression.MkClosure(optimize(lambda).asInstanceOf[Expression.Lambda], freeVars, tpe, loc)
      case Expression.MkClosureRef(ref, freeVars, tpe, loc) =>
        Expression.MkClosureRef(optimize(ref).asInstanceOf[Expression.Ref], freeVars, tpe, loc)
      case Expression.ApplyRef(sym, args, tpe, loc) => Expression.ApplyRef(sym, optimizeExps(args), tpe, loc)
      case Expression.ApplyTail(sym, formals, actuals, tpe, loc) =>
        Expression.ApplyTail(sym, formals, optimizeExps(actuals), tpe, loc)
      case Expression.ApplyHook(hook, args, tpe, loc) => Expression.ApplyHook(hook, optimizeExps(args), tpe, loc)
      case Expression.Apply(exp, args, tpe, loc) => Expression.Apply(optimize(exp), optimizeExps(args), tpe, loc)
      case Expression.Unary(op, exp, tpe, loc) => Expression.Unary(op, optimize(exp), tpe, loc)
      case Expression.Binary(op, exp1, exp2, tpe, loc) =>
        // Elimination of run-time tag checks of singleton-valued enums.
        val e1 = optimize(exp1)
        val e2 = optimize(exp2)
        (op, e1, e2) match {
          case (BinaryOperator.Equal, Expression.Tag(sym, tag, exp, tp, lc), _) =>
            if (isSingleCaseEnum(sym) && exp == Expression.Unit) Expression.True
            else Expression.Binary(op, e1, e2, tpe, loc)
          case (BinaryOperator.Equal, _, Expression.Tag(sym, tag, exp, tp, lc)) =>
            if (isSingleCaseEnum(sym) && exp == Expression.Unit) Expression.True
            else Expression.Binary(op, e1, e2, tpe, loc)
          case _ => Expression.Binary(op, e1, e2, tpe, loc)
        }
      case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
        // Elimination of dead branches (e.g. if (true) e1 else e2).
        val e1 = optimize(exp1)
        e1 match {
          case Expression.True => optimize(exp2)
          case Expression.False => optimize(exp3)
          case _ =>
            val e2 = optimize(exp2)
            val e3 = optimize(exp3)
            Expression.IfThenElse(e1, e2, e3, tpe, loc)
        }
      case Expression.Let(sym, exp1, exp2, tpe, loc) => Expression.Let(sym, optimize(exp1), optimize(exp2), tpe, loc)
      case Expression.Is(exp, tag, loc) => Expression.Is(optimize(exp), tag, loc)
      case Expression.Tag(sym, tag, exp, tpe, loc) => Expression.Tag(sym, tag, optimize(exp), tpe, loc)
      case Expression.Untag(tag, exp, tpe, loc) => Expression.Untag(tag, optimize(exp), tpe, loc)
      case Expression.Index(base, offset, tpe, loc) => Expression.Index(optimize(base), offset, tpe, loc)
      case Expression.Tuple(elms, tpe, loc) => Expression.Tuple(optimizeExps(elms), tpe, loc)
      case Expression.Existential(fparam, exp, loc) => Expression.Existential(fparam, optimize(exp), loc)
      case Expression.Universal(fparam, exp, loc) => Expression.Universal(fparam, optimize(exp), loc)
      case Expression.NativeConstructor(constructor, args, tpe, loc) =>
        Expression.NativeConstructor(constructor, optimizeExps(args), tpe, loc)
      case Expression.NativeField(field, tpe, loc) => Expression.NativeField(field, tpe, loc)
      case Expression.NativeMethod(method, args, tpe, loc) =>
        Expression.NativeMethod(method, optimizeExps(args), tpe, loc)
      case Expression.UserError(tpe, loc) => Expression.UserError(tpe, loc)
      case Expression.MatchError(tpe, loc) => Expression.MatchError(tpe, loc)
      case Expression.SwitchError(tpe, loc) => Expression.SwitchError(tpe, loc)
    }

    // Start the timer.
    val t = System.nanoTime()

    // Optimize expressions in the definitions.
    val optDefinitions = root.definitions.mapValues(defn => defn.copy(exp = optimize(defn.exp)))

    // Calculate the elapsed time.
    val e = System.nanoTime() - t

    // Reassemble the AST.
    root.copy(
      definitions = optDefinitions,
      time = root.time.copy(optimizer = e)
    ).toSuccess
  }
}
