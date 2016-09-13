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

import ca.uwaterloo.flix.language.ast.{Name, SimplifiedAst, Type}
import ca.uwaterloo.flix.util.InternalCompilerException

import scala.collection.mutable

object ClosureConv {

  /**
    * Performs closure conversion on the given expression `e`.
    */
  def convert(exp: SimplifiedAst.Expression)(implicit genSym: GenSym): SimplifiedAst.Expression = exp match {
    case SimplifiedAst.Expression.Unit => exp
    case SimplifiedAst.Expression.True => exp
    case SimplifiedAst.Expression.False => exp
    case SimplifiedAst.Expression.Char(lit) => exp
    case SimplifiedAst.Expression.Float32(lit) => exp
    case SimplifiedAst.Expression.Float64(lit) => exp
    case SimplifiedAst.Expression.Int8(lit) => exp
    case SimplifiedAst.Expression.Int16(lit) => exp
    case SimplifiedAst.Expression.Int32(lit) => exp
    case SimplifiedAst.Expression.Int64(lit) => exp
    case SimplifiedAst.Expression.BigInt(lit) => exp
    case SimplifiedAst.Expression.Str(lit) => exp
    case SimplifiedAst.Expression.LoadBool(n, o) => exp
    case SimplifiedAst.Expression.LoadInt8(b, o) => exp
    case SimplifiedAst.Expression.LoadInt16(b, o) => exp
    case SimplifiedAst.Expression.LoadInt32(b, o) => exp
    case SimplifiedAst.Expression.StoreBool(b, o, v) => exp
    case SimplifiedAst.Expression.StoreInt8(b, o, v) => exp
    case SimplifiedAst.Expression.StoreInt16(b, o, v) => exp
    case SimplifiedAst.Expression.StoreInt32(b, o, v) => exp
    case SimplifiedAst.Expression.Var(ident, o, tpe, loc) => exp

    case e: SimplifiedAst.Expression.Ref =>
      // If we encounter a Ref that has a lambda type (and is not being called in an Apply),
      // i.e. the Ref will evaluate to a lambda, we replace it with a MkClosureRef. Otherwise we leave it alone.
      e.tpe match {
        case t: Type.Lambda => SimplifiedAst.Expression.MkClosureRef(e, List.empty, t, e.loc)
        case _ => e
      }

    case SimplifiedAst.Expression.Lambda(args, body, tpe, loc) =>
      // Convert lambdas to closures. This is the main part of the `convert` function.
      // Closure conversion happens as follows:

      // First, we collect the free variables in the lambda expression.
      // NB: We pass the lambda expression (instead of its body) to account for bound arguments.
      val freeVars = freeVariables(exp).toList

      // We prepend the free variables to the arguments list. Thus all variables within the lambda body will be treated
      // uniformly. The implementation will supply values for the free variables, without any effort from the caller.
      val newArgs = freeVars.map { case (n, t) => SimplifiedAst.FormalArg(n, t) } ++ args

      // Update the lambda type.
      val argTpes = freeVars.map(_._2) ++ tpe.args
      val newTpe = Type.Lambda(argTpes, tpe.retTpe)

      // We rewrite the lambda with its new arguments list and new body, with any nested lambdas also converted.
      val lambda = SimplifiedAst.Expression.Lambda(newArgs, convert(body), newTpe, loc)

      // At this point, `lambda` is the original lambda expression, but with all free variables converted to new
      // arguments, prepended to the original arguments list. Additionally, any lambdas within the body have also been
      // closure converted.

      // We return a MkClosure node, which contains `lambda` (rewritten to have extra arguments so there are no more
      // free variables) as well as the cached `freeVars`. The closure will actually be created at run time, where the
      // values for the free variables are bound and stored in the closure structure. When the closure is called, the
      // bound values are passed as arguments.
      // Note that MkClosure keeps the old lambda type.
      // In a later phase, we will lift the lambda to a top-level definition.
      SimplifiedAst.Expression.MkClosure(lambda, freeVars.map(v => SimplifiedAst.FreeVar(v._1, -1, v._2)), tpe, loc)

    case SimplifiedAst.Expression.Hook(hook, tpe, loc) =>
      // Wrap the hook inside a lambda, so we can create a closure.
      val args = hook.tpe.args.map { t => SimplifiedAst.FormalArg(genSym.fresh2("arg"), t) }
      val hookArgs = args.map { f => SimplifiedAst.Expression.Var(f.ident, -1, f.tpe, loc) }
      val body = SimplifiedAst.Expression.ApplyHook(hook, hookArgs, hook.tpe.retTpe, loc)
      val lambda = SimplifiedAst.Expression.Lambda(args, body, hook.tpe, loc)

      // Closure convert the lambda.
      convert(lambda)
    case SimplifiedAst.Expression.MkClosure(lambda, freeVars, tpe, loc) =>
      throw InternalCompilerException(s"Illegal expression during closure conversion: '$exp'.")
    case SimplifiedAst.Expression.MkClosureRef(ref, freeVars, tpe, loc) =>
      throw InternalCompilerException(s"Illegal expression during closure conversion: '$exp'.")
    case SimplifiedAst.Expression.ApplyRef(name, args, tpe, loc) =>
      throw InternalCompilerException(s"Illegal expression during closure conversion: '$exp'.")

    case SimplifiedAst.Expression.Apply(e, args, tpe, loc) =>
      // We're trying to call some expression `e`. If `e` is a Ref, then it's a top-level function, so we directly call
      // it with ApplyRef. We remove the Ref node and don't recurse on it to avoid creating a closure.
      // We do something similar if `e` is a Hook, where we transform Apply to ApplyHook.
      e match {
        case SimplifiedAst.Expression.Ref(name, _, _) => SimplifiedAst.Expression.ApplyRef(name, args.map(convert), tpe, loc)
        case SimplifiedAst.Expression.Hook(hook, _, _) => SimplifiedAst.Expression.ApplyHook(hook, args.map(convert), tpe, loc)
        case _ => SimplifiedAst.Expression.Apply(convert(e), args.map(convert), tpe, loc)
      }
    case SimplifiedAst.Expression.ApplyTail(name, formals, actuals, tpe, loc) => exp
    case SimplifiedAst.Expression.ApplyHook(hook, args, tpe, loc) => exp

    case SimplifiedAst.Expression.Unary(op, e, tpe, loc) =>
      SimplifiedAst.Expression.Unary(op, convert(e), tpe, loc)
    case SimplifiedAst.Expression.Binary(op, e1, e2, tpe, loc) =>
      SimplifiedAst.Expression.Binary(op, convert(e1), convert(e2), tpe, loc)
    case SimplifiedAst.Expression.IfThenElse(e1, e2, e3, tpe, loc) =>
      SimplifiedAst.Expression.IfThenElse(convert(e1), convert(e2), convert(e3), tpe, loc)
    case SimplifiedAst.Expression.Let(ident, offset, e1, e2, tpe, loc) =>
      SimplifiedAst.Expression.Let(ident, offset, convert(e1), convert(e2), tpe, loc)
    case SimplifiedAst.Expression.CheckTag(tag, e, loc) =>
      SimplifiedAst.Expression.CheckTag(tag, convert(e), loc)
    case SimplifiedAst.Expression.GetTagValue(tag, e, tpe, loc) =>
      SimplifiedAst.Expression.GetTagValue(tag, convert(e), tpe, loc)
    case SimplifiedAst.Expression.Tag(enum, tag, e, tpe, loc) =>
      SimplifiedAst.Expression.Tag(enum, tag, convert(e), tpe, loc)
    case SimplifiedAst.Expression.GetTupleIndex(e, offset, tpe, loc) =>
      SimplifiedAst.Expression.GetTupleIndex(convert(e), offset, tpe, loc)
    case SimplifiedAst.Expression.Tuple(elms, tpe, loc) =>
      SimplifiedAst.Expression.Tuple(elms.map(convert), tpe, loc)
    case SimplifiedAst.Expression.CheckNil(e, loc) =>
      SimplifiedAst.Expression.CheckNil(convert(e), loc)
    case SimplifiedAst.Expression.CheckCons(e, loc) =>
      SimplifiedAst.Expression.CheckCons(convert(e), loc)
    case SimplifiedAst.Expression.FSet(elms, tpe, loc) =>
      SimplifiedAst.Expression.FSet(elms.map(convert), tpe, loc)
    case SimplifiedAst.Expression.Existential(params, e, loc) =>
      SimplifiedAst.Expression.Existential(params, convert(e), loc)
    case SimplifiedAst.Expression.Universal(params, e, loc) =>
      SimplifiedAst.Expression.Universal(params, convert(e), loc)
    case SimplifiedAst.Expression.UserError(tpe, loc) => exp
    case SimplifiedAst.Expression.MatchError(tpe, loc) => exp
    case SimplifiedAst.Expression.SwitchError(tpe, loc) => exp
  }

  /**
    * Returns the free variables in the given expression `exp`.
    * Does a left-to-right traversal of the AST, collecting free variables in order, in a LinkedHashSet.
    */
  def freeVariables(e: SimplifiedAst.Expression): mutable.LinkedHashSet[(Name.Ident, Type)] = e match {
    case SimplifiedAst.Expression.Unit => mutable.LinkedHashSet.empty
    case SimplifiedAst.Expression.True => mutable.LinkedHashSet.empty
    case SimplifiedAst.Expression.False => mutable.LinkedHashSet.empty
    case SimplifiedAst.Expression.Char(lit) => mutable.LinkedHashSet.empty
    case SimplifiedAst.Expression.Float32(lit) => mutable.LinkedHashSet.empty
    case SimplifiedAst.Expression.Float64(lit) => mutable.LinkedHashSet.empty
    case SimplifiedAst.Expression.Int8(lit) => mutable.LinkedHashSet.empty
    case SimplifiedAst.Expression.Int16(lit) => mutable.LinkedHashSet.empty
    case SimplifiedAst.Expression.Int32(lit) => mutable.LinkedHashSet.empty
    case SimplifiedAst.Expression.Int64(lit) => mutable.LinkedHashSet.empty
    case SimplifiedAst.Expression.BigInt(lit) => mutable.LinkedHashSet.empty
    case SimplifiedAst.Expression.Str(lit) => mutable.LinkedHashSet.empty
    case SimplifiedAst.Expression.LoadBool(n, o) => mutable.LinkedHashSet.empty
    case SimplifiedAst.Expression.LoadInt8(b, o) => mutable.LinkedHashSet.empty
    case SimplifiedAst.Expression.LoadInt16(b, o) => mutable.LinkedHashSet.empty
    case SimplifiedAst.Expression.LoadInt32(b, o) => mutable.LinkedHashSet.empty
    case SimplifiedAst.Expression.StoreBool(b, o, v) => mutable.LinkedHashSet.empty
    case SimplifiedAst.Expression.StoreInt8(b, o, v) => mutable.LinkedHashSet.empty
    case SimplifiedAst.Expression.StoreInt16(b, o, v) => mutable.LinkedHashSet.empty
    case SimplifiedAst.Expression.StoreInt32(b, o, v) => mutable.LinkedHashSet.empty
    case SimplifiedAst.Expression.Var(ident, o, tpe, loc) => mutable.LinkedHashSet((ident, tpe))
    case SimplifiedAst.Expression.Ref(name, tpe, loc) => mutable.LinkedHashSet.empty
    case SimplifiedAst.Expression.Lambda(args, body, tpe, loc) =>
      val bound = args.map(_.ident.name)
      freeVariables(body).filterNot { v => bound.contains(v._1.name) }
    case SimplifiedAst.Expression.Hook(hook, tpe, loc) => mutable.LinkedHashSet.empty
    case SimplifiedAst.Expression.MkClosure(lambda, freeVars, tpe, loc) =>
      throw InternalCompilerException(s"Unexpected expression: '$e'.")
    case SimplifiedAst.Expression.MkClosureRef(ref, freeVars, tpe, loc) =>
      throw InternalCompilerException(s"Unexpected expression: '$e'.")
    case SimplifiedAst.Expression.ApplyRef(name, args, tpe, loc) => mutable.LinkedHashSet.empty ++ args.flatMap(freeVariables)
    case SimplifiedAst.Expression.ApplyHook(hook, args, tpe, loc) => mutable.LinkedHashSet.empty ++ args.flatMap(freeVariables)
    case SimplifiedAst.Expression.ApplyTail(name, formals, actuals, tpe, loc) =>  mutable.LinkedHashSet.empty ++ actuals.flatMap(freeVariables)
    case SimplifiedAst.Expression.Apply(exp, args, tpe, loc) =>
      freeVariables(exp) ++ args.flatMap(freeVariables)
    case SimplifiedAst.Expression.Unary(op, exp, tpe, loc) => freeVariables(exp)
    case SimplifiedAst.Expression.Binary(op, exp1, exp2, tpe, loc) =>
      freeVariables(exp1) ++ freeVariables(exp2)
    case SimplifiedAst.Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
      freeVariables(exp1) ++ freeVariables(exp2) ++ freeVariables(exp3)
    case SimplifiedAst.Expression.Let(ident, offset, exp1, exp2, tpe, loc) =>
      val bound = ident.name
      freeVariables(exp1) ++ freeVariables(exp2).filterNot { v => bound == v._1.name }
    case SimplifiedAst.Expression.CheckTag(tag, exp, loc) => freeVariables(exp)
    case SimplifiedAst.Expression.GetTagValue(tag, exp, tpe, loc) => freeVariables(exp)
    case SimplifiedAst.Expression.Tag(enum, tag, exp, tpe, loc) => freeVariables(exp)
    case SimplifiedAst.Expression.GetTupleIndex(base, offset, tpe, loc) => freeVariables(base)
    case SimplifiedAst.Expression.Tuple(elms, tpe, loc) => mutable.LinkedHashSet.empty ++ elms.flatMap(freeVariables)
    case SimplifiedAst.Expression.CheckNil(exp, loc) => freeVariables(exp)
    case SimplifiedAst.Expression.CheckCons(exp, loc) => freeVariables(exp)
    case SimplifiedAst.Expression.FSet(elms, tpe, loc) => mutable.LinkedHashSet.empty ++ elms.flatMap(freeVariables)
    case SimplifiedAst.Expression.Existential(params, exp, loc) =>
      val bound = params.map(_.ident.name)
      freeVariables(exp).filterNot { v => bound.contains(v._1.name) }
    case SimplifiedAst.Expression.Universal(params, exp, loc) =>
      val bound = params.map(_.ident.name)
      freeVariables(exp).filterNot { v => bound.contains(v._1.name) }
    case SimplifiedAst.Expression.UserError(tpe, loc) => mutable.LinkedHashSet.empty
    case SimplifiedAst.Expression.MatchError(tpe, loc) => mutable.LinkedHashSet.empty
    case SimplifiedAst.Expression.SwitchError(tpe, loc) => mutable.LinkedHashSet.empty
  }

}
