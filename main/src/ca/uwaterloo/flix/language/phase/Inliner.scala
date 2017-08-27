/*
 * Copyright 2017 Magnus Madsen, Ifaz Kabir
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
import ca.uwaterloo.flix.language.{CompilationError, GenSym}
import ca.uwaterloo.flix.language.ast.SimplifiedAst.Expression
import ca.uwaterloo.flix.language.ast.{SimplifiedAst, Symbol}
import ca.uwaterloo.flix.util.{InternalCompilerException, Validation}
import ca.uwaterloo.flix.util.Validation._

/**
  * The inlining phase performs careful inlining of select functions based on heuristics.
  *
  * - A function of zero arguments is always inlined.
  * - A function of one boolean argument is always inlined.
  * - A function of two boolean arguments is always inlined.
  * - A function with a small "heuristic score" is always inlined.
  */
object Inliner extends Phase[SimplifiedAst.Root, SimplifiedAst.Root] {

  /**
    * Performs inlining on the given AST `root`.
    */
  def run(root: SimplifiedAst.Root)(implicit flix: Flix): Validation[SimplifiedAst.Root, CompilationError] = {
    implicit val genSym: GenSym = flix.genSym

    val t = System.nanoTime()

    val definitions = root.defs

    /**
      * Computes the score of each function definition
      */
    val scores = definitions.foldLeft(Map.empty[Symbol.DefnSym, Int]) {
      case (macc, (sym, constant)) =>
        macc + (sym -> score(constant))
    }

    /**
      * Transforms expressions inside definitions into optimized definitions.
      */
    val inlinedDefinitions = definitions.foldLeft(Map.empty[Symbol.DefnSym, SimplifiedAst.Def]) {
      case (macc, (sym, constant)) =>
        val newExp = inline(definitions, scores, constant.exp)
        macc + (sym -> constant.copy(exp = newExp))
    }

    val e = System.nanoTime() - t
    root.copy(defs = inlinedDefinitions, time = root.time.copy(inliner = e)).toSuccess
    //root.toSuccess
  }

  /**
    * Takes in a expression that has already been alpha-renamed and a list of args and let binds the arguments
    */
  def letBindArgs(exp: Expression, args: List[(Symbol.VarSym, Expression)]): Expression = {
    args match {
      case (sym, exp1) :: xs =>
        val exp2 = letBindArgs(exp, xs)
        Expression.Let(sym, exp1, exp2, exp2.tpe, exp1.loc)
      case Nil =>
        exp
    }
  }

  /**
    * Traverses through `exp` and performs inlining using
    * definitions and sizes from `scores`
    */
  def inline(definitions: Map[Symbol.DefnSym, SimplifiedAst.Def], scores: Map[Symbol.DefnSym, Int], exp: Expression)(implicit genSym: GenSym): Expression = {
    def visit = inline(definitions, scores, _: Expression)

    exp match {
      case Expression.ApplyClo(exp1, args, tpe, loc) =>
        Expression.ApplyClo(visit(exp1), args.map(visit), tpe, loc)

      /* Inline application */
      case Expression.ApplyDef(sym, args, tpe, loc) =>
        //inline arguments
        val args1 = args.map(visit)
        definitions(sym) match {
          case SimplifiedAst.Def(_, _, _, formals, exp1, _, _, _) =>
            if (scores(sym) <= MaxScore) {
              // Inline the body of the function
              val sub = formals.map(f => f.sym -> Symbol.freshVarSym(f.sym)).toMap
              val bindings = formals.map(f => sub(f.sym)).zip(args1)
              letBindArgs(renameAndSubstitute(exp1, sub), bindings)
            }
            else {
              // Do not inline the body -- score is too high
              Expression.ApplyDef(sym, args1, tpe, loc)
            }
        }

      case Expression.ApplyCloTail(exp1, args, tpe, loc) =>
        // Do not inline tail calls.
        Expression.ApplyCloTail(visit(exp1), args.map(visit), tpe, loc)

      case Expression.ApplyDefTail(sym, args, tpe, loc) =>
        // Do not inline tail calls.
        Expression.ApplyDefTail(sym, args, tpe, loc)

      /* Inline inside expression */
      case Expression.Closure(ref, freeVars, tpe, loc) =>
        Expression.Closure(ref, freeVars, tpe, loc)
      case Expression.Unit => exp
      case Expression.True => exp
      case Expression.False => exp
      case Expression.Char(_) => exp
      case Expression.Float32(_) => exp
      case Expression.Float64(_) => exp
      case Expression.Int8(_) => exp
      case Expression.Int16(_) => exp
      case Expression.Int32(_) => exp
      case Expression.Int64(_) => exp
      case Expression.BigInt(_) => exp
      case Expression.Str(_) => exp
      case Expression.Var(_, _, _) => exp
      case Expression.Def(_, _, _) => exp
      case Expression.Lambda(args, body, tpe, loc) =>
        Expression.Lambda(args, visit(body), tpe, loc)
      case Expression.Hook(_, _, _) => exp
      case Expression.ApplySelfTail(sym, formals, actuals, tpe, loc) =>
        Expression.ApplySelfTail(sym, formals, actuals.map(visit), tpe, loc)
      case Expression.ApplyHook(hook, args, tpe, loc) =>
        Expression.ApplyHook(hook, args.map(visit), tpe, loc)
      case Expression.Unary(sop, op, exp1, tpe, loc) =>
        Expression.Unary(sop, op, visit(exp1), tpe, loc)
      case Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
        Expression.Binary(sop, op, visit(exp1), visit(exp2), tpe, loc)
      case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
        Expression.IfThenElse(visit(exp1), visit(exp2), visit(exp3), tpe, loc)
      case Expression.Let(sym, exp1, exp2, tpe, loc) =>
        Expression.Let(sym, visit(exp1), visit(exp2), tpe, loc)
      case Expression.LetRec(sym, exp1, exp2, tpe, loc) => None
        Expression.LetRec(sym, visit(exp1), visit(exp2), tpe, loc)
      case Expression.Is(sym, tag, exp1, loc) =>
        Expression.Is(sym, tag, visit(exp1), loc)
      case Expression.Tag(sym, tag, exp1, tpe, loc) =>
        Expression.Tag(sym, tag, visit(exp1), tpe, loc)
      case Expression.Untag(sym, tag, exp1, tpe, loc) =>
        Expression.Untag(sym, tag, visit(exp1), tpe, loc)
      case Expression.Index(base, offset, tpe, loc) =>
        Expression.Index(visit(base), offset, tpe, loc)
      case Expression.Tuple(elms, tpe, loc) =>
        Expression.Tuple(elms.map(visit), tpe, loc)
      case Expression.Ref(exp1, tpe, loc) =>
        Expression.Ref(visit(exp1), tpe, loc)
      case Expression.Deref(exp1, tpe, loc) =>
        Expression.Deref(visit(exp1), tpe, loc)
      case Expression.Assign(exp1, exp2, tpe, loc) =>
        Expression.Assign(visit(exp1), visit(exp2), tpe, loc)
      case Expression.Existential(fparam, exp1, loc) =>
        Expression.Existential(fparam, visit(exp1), loc)
      case Expression.Universal(fparam, exp1, loc) =>
        Expression.Universal(fparam, visit(exp1), loc)
      case Expression.NativeConstructor(constructor, args, tpe, loc) =>
        Expression.NativeConstructor(constructor, args.map(visit), tpe, loc)
      case Expression.NativeField(_, _, _) => exp
      case Expression.NativeMethod(method, args, tpe, loc) =>
        Expression.NativeMethod(method, args.map(visit), tpe, loc)
      case Expression.UserError(_, _) => exp
      case Expression.MatchError(_, _) => exp
      case Expression.SwitchError(_, _) => exp
      /* Error */
      case Expression.LambdaClosure(_, _, _, _) => throw InternalCompilerException(s"Unexpected expression: '${exp.getClass.getSimpleName}'.")
      case Expression.Apply(exp1, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp.getClass.getSimpleName}'.")

    }
  }

  /**
    * Performs alpha-renaming of an expression
    */
  def renameAndSubstitute(exp: Expression, sub: Map[Symbol.VarSym, Symbol.VarSym])(implicit genSym: GenSym): Expression = exp match {
    case Expression.Unit => exp
    case Expression.True => exp
    case Expression.False => exp
    case Expression.Char(_) => exp
    case Expression.Float32(_) => exp
    case Expression.Float64(_) => exp
    case Expression.Int8(_) => exp
    case Expression.Int16(_) => exp
    case Expression.Int32(_) => exp
    case Expression.Int64(_) => exp
    case Expression.BigInt(_) => exp
    case Expression.Str(_) => exp
    case Expression.Var(sym, tpe, loc) => Expression.Var(sub(sym), tpe, loc)
    case Expression.Def(_, _, _) => exp
    case Expression.Lambda(args, body, tpe, loc) =>
      val formals = args.map(f => f.copy(sym = Symbol.freshVarSym(f.sym)))
      val sub1 = args.map(f => f.sym).zip(formals.map(f => f.sym)).foldLeft(sub) { case (macc, (from, to)) => macc + (from -> to) }
      Expression.Lambda(args = formals, renameAndSubstitute(body, sub1), tpe, loc)
    case Expression.Hook(_, _, _) => exp
    case Expression.Closure(ref, freeVars, tpe, loc) =>
      Expression.Closure(ref, freeVars.map(fv => fv.copy(sym = sub(fv.sym))), tpe, loc)
    case Expression.ApplyClo(exp1, args, tpe, loc) =>
      Expression.ApplyClo(renameAndSubstitute(exp1, sub), args.map(renameAndSubstitute(_, sub)), tpe, loc)
    case Expression.ApplyDef(sym, args, tpe, loc) =>
      Expression.ApplyDef(sym, args.map(renameAndSubstitute(_, sub)), tpe, loc)
    case Expression.ApplyCloTail(exp1, args, tpe, loc) =>
      Expression.ApplyCloTail(renameAndSubstitute(exp1, sub), args.map(renameAndSubstitute(_, sub)), tpe, loc)
    case Expression.ApplyDefTail(sym, args, tpe, loc) =>
      Expression.ApplyDefTail(sym, args.map(renameAndSubstitute(_, sub)), tpe, loc)
    case Expression.ApplySelfTail(sym, formals, actuals, tpe, loc) =>
      Expression.ApplySelfTail(sym, formals, actuals.map(renameAndSubstitute(_, sub)), tpe, loc)
    case Expression.ApplyHook(hook, args, tpe, loc) =>
      Expression.ApplyHook(hook, args.map(renameAndSubstitute(_, sub)), tpe, loc)
    case Expression.Unary(sop, op, exp1, tpe, loc) =>
      Expression.Unary(sop, op, renameAndSubstitute(exp1, sub), tpe, loc)
    case Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
      Expression.Binary(sop, op, renameAndSubstitute(exp1, sub), renameAndSubstitute(exp2, sub), tpe, loc)
    case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
      Expression.IfThenElse(renameAndSubstitute(exp1, sub), renameAndSubstitute(exp2, sub), renameAndSubstitute(exp3, sub), tpe, loc)
    case Expression.Let(sym, exp1, exp2, tpe, loc) =>
      val newSym = Symbol.freshVarSym(sym)
      val sub1 = sub + (sym -> newSym)
      Expression.Let(newSym, renameAndSubstitute(exp1, sub1), renameAndSubstitute(exp2, sub1), tpe, loc)
    case Expression.LetRec(sym, exp1, exp2, tpe, loc) =>
      val newSym = Symbol.freshVarSym(sym)
      val sub1 = sub + (sym -> newSym)
      Expression.LetRec(newSym, renameAndSubstitute(exp1, sub1), renameAndSubstitute(exp2, sub1), tpe, loc)
    case Expression.Is(sym, tag, exp1, loc) =>
      Expression.Is(sym, tag, renameAndSubstitute(exp1, sub), loc)
    case Expression.Tag(sym, tag, exp1, tpe, loc) =>
      Expression.Tag(sym, tag, renameAndSubstitute(exp1, sub), tpe, loc)
    case Expression.Untag(sym, tag, exp1, tpe, loc) =>
      Expression.Untag(sym, tag, renameAndSubstitute(exp1, sub), tpe, loc)
    case Expression.Index(base, offset, tpe, loc) =>
      Expression.Index(renameAndSubstitute(base, sub), offset, tpe, loc)
    case Expression.Tuple(elms, tpe, loc) =>
      Expression.Tuple(elms.map(renameAndSubstitute(_, sub)), tpe, loc)
    case Expression.Ref(exp1, tpe, loc) =>
      Expression.Ref(renameAndSubstitute(exp1, sub), tpe, loc)
    case Expression.Deref(exp1, tpe, loc) =>
      Expression.Deref(renameAndSubstitute(exp1, sub), tpe, loc)
    case Expression.Assign(exp1, exp2, tpe, loc) =>
      Expression.Assign(renameAndSubstitute(exp1, sub), renameAndSubstitute(exp2, sub), tpe, loc)
    case Expression.Existential(fparam, exp1, loc) =>
      val newFparam = fparam.copy(sym = Symbol.freshVarSym(fparam.sym))
      val sub1 = sub + (fparam.sym -> newFparam.sym)
      Expression.Existential(newFparam, renameAndSubstitute(exp1, sub1), loc)
    case Expression.Universal(fparam, exp1, loc) =>
      val newFparam = fparam.copy(sym = Symbol.freshVarSym(fparam.sym))
      val sub1 = sub + (fparam.sym -> newFparam.sym)
      Expression.Universal(newFparam, renameAndSubstitute(exp1, sub1), loc)
    case Expression.NativeConstructor(constructor, args, tpe, loc) =>
      Expression.NativeConstructor(constructor, args.map(renameAndSubstitute(_, sub)), tpe, loc)
    case Expression.NativeField(_, _, _) => exp
    case Expression.NativeMethod(method, args, tpe, loc) =>
      Expression.NativeMethod(method, args.map(renameAndSubstitute(_, sub)), tpe, loc)
    case Expression.UserError(_, _) => exp
    case Expression.MatchError(_, _) => exp
    case Expression.SwitchError(_, _) => exp

    case Expression.LambdaClosure(_, _, _, _) => throw InternalCompilerException(s"Unexpected expression: '${exp.getClass.getSimpleName}'.")
    case Expression.Apply(exp1, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp.getClass.getSimpleName}'.")
  }

  /**
    * The maximum score of a function for it to be eligible for inlining.
    */
  val MaxScore = 25

  /**
    * Returns the score of the given function definition `defn`.
    *
    * The score of an expression is computed as:
    *
    * - Every expression is worth one point (plus the sum of its children), except:
    * - A literal is worth zero points.
    * - A UserError, MatchError, or SwitchError is worth zero points.
    * - An if-then-else statement is worth the value of the condition plus two times the sum of the consequent and alternative.
    */
  def score(definitions: SimplifiedAst.Def): Int = exprScore(definitions.exp)

  /**
    * Returns the score of the given expression `exp`.
    */
  def exprScore(exp0: Expression): Int = {
    exp0 match {
      case Expression.Unit => 0
      case Expression.True => 0
      case Expression.False => 0
      case Expression.Char(_) => 0
      case Expression.Float32(_) => 0
      case Expression.Float64(_) => 0
      case Expression.Int8(_) => 0
      case Expression.Int16(_) => 0
      case Expression.Int32(_) => 0
      case Expression.Int64(_) => 0
      case Expression.BigInt(_) => 0
      case Expression.Str(_) => 0
      case Expression.Var(_, _, _) => 1
      case Expression.Def(_, _, _) => 1
      case Expression.Lambda(args, body, _, _) => 1 + args.length + exprScore(body)
      case Expression.Hook(_, _, _) => 1
      case Expression.Closure(ref, freeVars, _, _) => 1 + freeVars.length + exprScore(ref)
      case Expression.ApplyClo(exp, args, _, _) => 1 + exprScore(exp) + args.map(exprScore).sum
      case Expression.ApplyDef(sym, args, _, _) => 1 + args.map(exprScore).sum
      case Expression.ApplySelfTail(sym, formals, actuals, _, _) =>
        // Not to be inlined
        MaxScore + 1 + formals.length + actuals.map(exprScore).sum
      case Expression.ApplyHook(hook, args, _, _) => 1 + args.map(exprScore).sum
      case Expression.Unary(sop, op, exp1, _, _) => 1 + exprScore(exp1)
      case Expression.Binary(sop, op, exp1, exp2, _, _) => 1 + exprScore(exp1) + exprScore(exp2)
      case Expression.IfThenElse(exp1, exp2, exp3, _, _) => exprScore(exp1) + (2 * (exprScore(exp2) + exprScore(exp3)))
      case Expression.Let(sym, exp1, exp2, _, _) => 1 + exprScore(exp1) + exprScore(exp2)
      case Expression.LetRec(sym, exp1, exp2, _, _) => 1 + exprScore(exp1) + exprScore(exp2)
      case Expression.Is(sym, tag, exp1, loc) => 1 + exprScore(exp1)
      case Expression.Tag(sym, tag, exp1, _, _) => 1 + exprScore(exp1)
      case Expression.Untag(sym, tag, exp1, _, _) => 1 + exprScore(exp1)
      case Expression.Index(base, offset, _, _) => 2
      case Expression.Tuple(elms, _, _) => 1 + elms.map(exprScore).sum
      case Expression.Ref(exp1, _, _) => 1 + exprScore(exp1)
      case Expression.Deref(exp1, _, _) => 1 + exprScore(exp1)
      case Expression.Assign(exp1, exp2, _, _) => 1 + exprScore(exp1) + exprScore(exp2)
      case Expression.Existential(fparam, exp1, loc) => 2 + exprScore(exp1)
      case Expression.Universal(fparam, exp1, loc) => 2 + exprScore(exp1)
      case Expression.NativeConstructor(constructor, args, _, _) => 2 + args.map(exprScore).sum
      case Expression.NativeField(field, _, _) => 2
      case Expression.NativeMethod(method, args, _, _) => 2 + args.map(exprScore).sum
      case Expression.UserError(_, _) => 0
      case Expression.MatchError(_, _) => 0
      case Expression.SwitchError(_, _) => 0
      case Expression.LambdaClosure(_, _, _, _) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
      case Expression.Apply(_, _, _, _) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")

    }
  }
}
