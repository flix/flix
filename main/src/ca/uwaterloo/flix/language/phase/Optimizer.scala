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
import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.language.ast.SimplifiedAst
import ca.uwaterloo.flix.language.ast.SimplifiedAst._
import ca.uwaterloo.flix.language.debug.PrettyPrinter
import ca.uwaterloo.flix.util.{InternalCompilerException, Validation}
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.vt._

/**
  * The Optimization phase performs intra-procedural optimizations.
  *
  * Specifically,
  *
  * - Elimination of dead branches (e.g. if (true) e1 else e2).
  * - Copy propagation (e.g. let z = w; let y = z; let x = y; x -> w)
  * - Propagates closures.
  */
object Optimizer extends Phase[SimplifiedAst.Root, SimplifiedAst.Root] {

  /**
    * Returns an optimized version of the given AST `root`.
    */
  def run(root: SimplifiedAst.Root)(implicit flix: Flix): Validation[SimplifiedAst.Root, CompilationError] = {

    /**
      * Performs intra-procedural optimization on the given expression `exp0` and substitution map `env0`.
      */
    def visitExp(exp0: Expression, env0: Map[Symbol.VarSym, Symbol.VarSym]): Expression = exp0 match {
      // 
      // Literal Expressions.
      //
      case Expression.Unit => exp0
      case Expression.True => exp0
      case Expression.False => exp0
      case Expression.Char(lit) => exp0
      case Expression.Float32(lit) => exp0
      case Expression.Float64(lit) => exp0
      case Expression.Int8(lit) => exp0
      case Expression.Int16(lit) => exp0
      case Expression.Int32(lit) => exp0
      case Expression.Int64(lit) => exp0
      case Expression.BigInt(lit) => exp0
      case Expression.Str(lit) => exp0

      //
      // Variable Expressions.
      //
      case Expression.Var(sym, tpe, loc) =>
        // Lookup to see if the variable should be replaced by a copy.
        env0.get(sym) match {
          case None => Expression.Var(sym, tpe, loc)
          case Some(srcSym) => Expression.Var(srcSym, tpe, loc)
        }

      //
      // Def Expressions.
      //
      case Expression.Def(sym, tpe, loc) => exp0

      //
      // Closure Expressions.
      //
      case Expression.Closure(exp, freeVars, tpe, loc) =>
        val e = visitExp(exp, env0)
        val fvs = freeVars map {
          case FreeVar(sym, varType) => FreeVar(env0.getOrElse(sym, sym), varType)
        }
        Expression.Closure(e.asInstanceOf[Expression.Def], fvs, tpe, loc)

      //
      // ApplyClo Expressions.
      //
      case Expression.ApplyClo(exp, args, tpe, loc) =>
        val as = args map (visitExp(_, env0))
        Expression.ApplyClo(exp, as, tpe, loc)

      //
      // ApplyDef Expressions.
      //
      case Expression.ApplyDef(sym, args, tpe, loc) =>
        val as = args map (visitExp(_, env0))
        Expression.ApplyDef(sym, as, tpe, loc)

      //
      // ApplyCloTail Expressions.
      //
      case Expression.ApplyCloTail(exp, args, tpe, loc) =>
        val e = visitExp(exp, env0)
        val as = args map (visitExp(_, env0))
        Expression.ApplyClo(e, as, tpe, loc)

      //
      // ApplyDefTail Expressions.
      //
      case Expression.ApplyDefTail(sym, args, tpe, loc) =>
        val as = args map (visitExp(_, env0))
        Expression.ApplyDef(sym, as, tpe, loc)

      //
      // ApplySelfTail Expressions.
      //
      case Expression.ApplySelfTail(sym, formals, actuals, tpe, loc) =>
        val as = actuals map (visitExp(_, env0))
        Expression.ApplySelfTail(sym, formals, as, tpe, loc)

      //
      // ApplyHook Expressions.
      //
      case Expression.ApplyHook(hook, args, tpe, loc) =>
        val as = args map (visitExp(_, env0))
        Expression.ApplyHook(hook, as, tpe, loc)

      //
      // Unary Expressions.
      //
      case Expression.Unary(sop, op, exp, tpe, loc) =>
        val e = visitExp(exp, env0)
        Expression.Unary(sop, op, e, tpe, loc)

      //
      // Binary Expressions.
      //
      case Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
        val e1 = visitExp(exp1, env0)
        val e2 = visitExp(exp2, env0)
        Expression.Binary(sop, op, e1, e2, tpe, loc)

      //
      // If-then-else Expressions.
      //
      case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
        // Eliminate dead branches, if possible.
        val cond = visitExp(exp1, env0)
        val consequent = visitExp(exp2, env0)
        val alternative = visitExp(exp3, env0)
        cond match {
          case Expression.True => consequent
          case Expression.False => alternative
          case _ => Expression.IfThenElse(cond, consequent, alternative, tpe, loc)
        }

      //
      // Block Expressions.
      //
      case Expression.Block(branches, default, tpe, loc) =>
        val br = branches map {
          case (sym, exp) => sym -> visitExp(exp, env0)
        }
        Expression.Block(br, default, tpe, loc)

      //
      // Jump Expressions.
      //
      case Expression.Jump(sym, tpe, loc) =>
        Expression.Jump(sym, tpe, loc)

      //
      // Let Expressions.
      //
      case Expression.Let(sym, exp1, exp2, tpe, loc) =>
        // Visit the value expression.
        val e1 = visitExp(exp1, env0)

        // Check for copy propagation: let x = y; e ~~> e[x -> y]
        e1 match {
          case Expression.Var(srcSym, _, _) =>
            // The srcSym might itself originate from some other symbol.
            val originalSym = env0.getOrElse(srcSym, srcSym)
            visitExp(exp2, env0 + (sym -> originalSym))
          case _ =>
            val e2 = visitExp(exp2, env0)
            Expression.Let(sym, e1, e2, tpe, loc)
        }

      //
      // LetRec Expressions.
      //
      case Expression.LetRec(sym, exp1, exp2, tpe, loc) =>
        val e1 = visitExp(exp1, env0)
        val e2 = visitExp(exp2, env0)
        Expression.LetRec(sym, e1, e2, tpe, loc)

      //
      // Is Expressions.
      //
      case Expression.Is(sym, tag, exp, loc) =>
        val e = visitExp(exp, env0)
        Expression.Is(sym, tag, e, loc)

      //
      // Tag Expressions.
      //
      case Expression.Tag(sym, tag, exp, tpe, loc) =>
        val e = visitExp(exp, env0)
        Expression.Tag(sym, tag, e, tpe, loc)

      //
      // Untag Expressions.
      //
      case Expression.Untag(sym, tag, exp, tpe, loc) =>
        val e = visitExp(exp, env0)
        Expression.Untag(sym, tag, e, tpe, loc)

      //
      // Index Expressions.
      //
      case Expression.Index(base, offset, tpe, loc) =>
        val b = visitExp(base, env0)
        Expression.Index(b, offset, tpe, loc)

      //
      // Tuple Expressions.
      //
      case Expression.Tuple(elms, tpe, loc) =>
        val es = elms map (visitExp(_, env0))
        Expression.Tuple(es, tpe, loc)

      //
      // Reference Expressions.
      //
      case Expression.Ref(exp, tpe, loc) =>
        val e = visitExp(exp, env0)
        Expression.Ref(e, tpe, loc)

      //
      // Dereference Expressions.
      //
      case Expression.Deref(exp, tpe, loc) =>
        val e = visitExp(exp, env0)
        Expression.Deref(e, tpe, loc)

      //
      // Assign Expressions.
      //
      case Expression.Assign(exp1, exp2, tpe, loc) =>
        val e1 = visitExp(exp1, env0)
        val e2 = visitExp(exp2, env0)
        Expression.Assign(e1, e2, tpe, loc)

      //
      // Existential Expressions.
      //
      case Expression.Existential(fparam, exp, loc) =>
        val e = visitExp(exp, env0)
        Expression.Existential(fparam, e, loc)

      //
      // Universal Expressions.
      //
      case Expression.Universal(fparam, exp, loc) =>
        val e = visitExp(exp, env0)
        Expression.Universal(fparam, e, loc)

      //
      // Native Constructor.
      //
      case Expression.NativeConstructor(constructor, args, tpe, loc) =>
        val as = args map (visitExp(_, env0))
        Expression.NativeConstructor(constructor, as, tpe, loc)

      //
      // Native Field.
      //
      case Expression.NativeField(field, tpe, loc) => exp0

      //
      // Native Method.
      //
      case Expression.NativeMethod(method, args, tpe, loc) =>
        val as = args map (visitExp(_, env0))
        Expression.NativeMethod(method, as, tpe, loc)

      //
      // Error Expressions.
      //
      case Expression.UserError(tpe, loc) => exp0
      case Expression.MatchError(tpe, loc) => exp0
      case Expression.SwitchError(tpe, loc) => exp0

      //
      // Unexpected Expressions.
      //
      case Expression.LambdaClosure(lambda, freeVars, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
      case Expression.Lambda(args, body, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
      case Expression.Hook(hook, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
      case Expression.Apply(exp, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
    }

    // Visit every definition in the program.
    val defs = root.defs.map {
      case (sym, defn) => sym -> defn.copy(exp = visitExp(defn.exp, Map.empty))
    }

    // Reassemble the ast root.
    val result = root.copy(defs = defs)

    // Print the ast if debugging is enabled.
    if (flix.options.debug) {
      println(PrettyPrinter.Simplified.fmtRoot(result).fmt(TerminalContext.AnsiTerminal))
    }

    result.toSuccess
  }

}
