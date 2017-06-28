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

import ca.uwaterloo.flix.language.GenSym
import ca.uwaterloo.flix.language.ast.SimplifiedAst.Expression
import ca.uwaterloo.flix.language.ast.{Ast, SimplifiedAst, SourceLocation, Symbol, Type}
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

    case SimplifiedAst.Expression.Var(sym, tpe, loc) => exp

    case e: SimplifiedAst.Expression.Def =>
      // If we encounter a Ref that has a lambda type (and is not being called in an Apply),
      // i.e. the Ref will evaluate to a lambda, we replace it with a MkClosureRef. Otherwise we leave it alone.
      e.tpe match {
        case t@Type.Apply(Type.Arrow(_), _) => SimplifiedAst.Expression.MkClosureRef(e, List.empty, t, e.loc)
        case _ => e
      }

    case SimplifiedAst.Expression.Lambda(args, body, tpe, loc) =>
      // Retrieve the type of the function.
      val Type.Apply(Type.Arrow(l), ts) = tpe
      val (targs, tresult) = (ts.take(l - 1), ts.last)

      // Convert lambdas to closures. This is the main part of the `convert` function.
      // Closure conversion happens as follows:

      // First, we collect the free variables in the lambda expression.
      // NB: We pass the lambda expression (instead of its body) to account for bound arguments.
      val freeVars = freeVariables(exp).toList

      // We prepend the free variables to the arguments list. Thus all variables within the lambda body will be treated
      // uniformly. The implementation will supply values for the free variables, without any effort from the caller.
      // We introduce new symbols for each introduced parameter and replace their occurence in the body.
      val subst = mutable.Map.empty[Symbol.VarSym, Symbol.VarSym]
      val newArgs = freeVars.map {
        case (oldSym, ptype) =>
          val newSym = Symbol.freshVarSym(oldSym)
          subst += (oldSym -> newSym)
          SimplifiedAst.FormalParam(newSym, Ast.Modifiers.Empty, ptype, SourceLocation.Unknown)
      } ++ args

      // Update the lambda type.
      val argTpes = freeVars.map(_._2) ++ targs
      val newTpe = Type.mkArrow(argTpes, tresult)

      // We rewrite the lambda with its new arguments list and new body, with any nested lambdas also converted.
      val lambda = SimplifiedAst.Expression.Lambda(newArgs, convert(replace(body, subst.toMap)), newTpe, loc)

      // At this point, `lambda` is the original lambda expression, but with all free variables converted to new
      // arguments, prepended to the original arguments list. Additionally, any lambdas within the body have also been
      // closure converted.

      // We return a MkClosure node, which contains `lambda` (rewritten to have extra arguments so there are no more
      // free variables) as well as the cached `freeVars`. The closure will actually be created at run time, where the
      // values for the free variables are bound and stored in the closure structure. When the closure is called, the
      // bound values are passed as arguments.
      // Note that MkClosure keeps the old lambda type.
      // In a later phase, we will lift the lambda to a top-level definition.
      SimplifiedAst.Expression.MkClosure(lambda, freeVars.map(v => SimplifiedAst.FreeVar(v._1, v._2)), tpe, loc)

    case SimplifiedAst.Expression.Hook(hook, tpe, loc) =>
      // Retrieve the type of the function.
      val Type.Apply(Type.Arrow(l), ts) = tpe
      val (targs, tresult) = (ts.take(l - 1), ts.last)

      // Wrap the hook inside a lambda, so we can create a closure.
      val args = targs.map { t => SimplifiedAst.FormalParam(Symbol.freshVarSym("hookArg"), Ast.Modifiers.Empty, t, SourceLocation.Unknown) }
      val hookArgs = args.map { f => SimplifiedAst.Expression.Var(f.sym, f.tpe, loc) }
      val body = SimplifiedAst.Expression.ApplyHook(hook, hookArgs, tresult, loc)
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
        case SimplifiedAst.Expression.Def(name, _, _) => SimplifiedAst.Expression.ApplyRef(name, args.map(convert), tpe, loc)
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
    case SimplifiedAst.Expression.Let(sym, e1, e2, tpe, loc) =>
      SimplifiedAst.Expression.Let(sym, convert(e1), convert(e2), tpe, loc)
    case SimplifiedAst.Expression.LetRec(sym, e1, e2, tpe, loc) =>
      SimplifiedAst.Expression.LetRec(sym, convert(e1), convert(e2), tpe, loc)
    case SimplifiedAst.Expression.Is(sym, tag, e, loc) =>
      SimplifiedAst.Expression.Is(sym, tag, convert(e), loc)
    case SimplifiedAst.Expression.Tag(enum, tag, e, tpe, loc) =>
      SimplifiedAst.Expression.Tag(enum, tag, convert(e), tpe, loc)
    case SimplifiedAst.Expression.Untag(sym, tag, e, tpe, loc) =>
      SimplifiedAst.Expression.Untag(sym, tag, convert(e), tpe, loc)
    case SimplifiedAst.Expression.Index(e, offset, tpe, loc) =>
      SimplifiedAst.Expression.Index(convert(e), offset, tpe, loc)
    case SimplifiedAst.Expression.Tuple(elms, tpe, loc) =>
      SimplifiedAst.Expression.Tuple(elms.map(convert), tpe, loc)
    case SimplifiedAst.Expression.Existential(params, e, loc) =>
      SimplifiedAst.Expression.Existential(params, convert(e), loc)
    case SimplifiedAst.Expression.Universal(params, e, loc) =>
      SimplifiedAst.Expression.Universal(params, convert(e), loc)
    case SimplifiedAst.Expression.NativeConstructor(constructor, args, tpe, loc) => exp
    case SimplifiedAst.Expression.NativeField(field, tpe, loc) => exp
    case SimplifiedAst.Expression.NativeMethod(method, args, tpe, loc) => exp
    case SimplifiedAst.Expression.UserError(tpe, loc) => exp
    case SimplifiedAst.Expression.MatchError(tpe, loc) => exp
    case SimplifiedAst.Expression.SwitchError(tpe, loc) => exp
  }

  /**
    * Returns the free variables in the given expression `exp`.
    * Does a left-to-right traversal of the AST, collecting free variables in order, in a LinkedHashSet.
    */
  // TODO: Use immutable, but sorted data structure?
  def freeVariables(e: SimplifiedAst.Expression): mutable.LinkedHashSet[(Symbol.VarSym, Type)] = e match {
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
    case SimplifiedAst.Expression.Var(sym, tpe, loc) => mutable.LinkedHashSet((sym, tpe))
    case SimplifiedAst.Expression.Def(name, tpe, loc) => mutable.LinkedHashSet.empty
    case SimplifiedAst.Expression.Lambda(args, body, tpe, loc) =>
      val bound = args.map(_.sym)
      freeVariables(body).filterNot { v => bound.contains(v._1) }
    case SimplifiedAst.Expression.Hook(hook, tpe, loc) => mutable.LinkedHashSet.empty
    case SimplifiedAst.Expression.MkClosure(lambda, freeVars, tpe, loc) =>
      throw InternalCompilerException(s"Unexpected expression: '$e'.")
    case SimplifiedAst.Expression.MkClosureRef(ref, freeVars, tpe, loc) =>
      throw InternalCompilerException(s"Unexpected expression: '$e'.")
    case SimplifiedAst.Expression.ApplyRef(name, args, tpe, loc) => mutable.LinkedHashSet.empty ++ args.flatMap(freeVariables)
    case SimplifiedAst.Expression.ApplyHook(hook, args, tpe, loc) => mutable.LinkedHashSet.empty ++ args.flatMap(freeVariables)
    case SimplifiedAst.Expression.ApplyTail(name, formals, actuals, tpe, loc) => mutable.LinkedHashSet.empty ++ actuals.flatMap(freeVariables)
    case SimplifiedAst.Expression.Apply(exp, args, tpe, loc) =>
      freeVariables(exp) ++ args.flatMap(freeVariables)
    case SimplifiedAst.Expression.Unary(op, exp, tpe, loc) => freeVariables(exp)
    case SimplifiedAst.Expression.Binary(op, exp1, exp2, tpe, loc) =>
      freeVariables(exp1) ++ freeVariables(exp2)
    case SimplifiedAst.Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
      freeVariables(exp1) ++ freeVariables(exp2) ++ freeVariables(exp3)
    case SimplifiedAst.Expression.Let(sym, exp1, exp2, tpe, loc) =>
      val bound = sym
      freeVariables(exp1) ++ freeVariables(exp2).filterNot { v => bound == v._1 }
    case SimplifiedAst.Expression.LetRec(sym, exp1, exp2, tpe, loc) =>
      val bound = sym
      (freeVariables(exp1) ++ freeVariables(exp2)).filterNot { v => bound == v._1 }
    case SimplifiedAst.Expression.Is(sym, tag, exp, loc) => freeVariables(exp)
    case SimplifiedAst.Expression.Untag(sym, tag, exp, tpe, loc) => freeVariables(exp)
    case SimplifiedAst.Expression.Tag(enum, tag, exp, tpe, loc) => freeVariables(exp)
    case SimplifiedAst.Expression.Index(base, offset, tpe, loc) => freeVariables(base)
    case SimplifiedAst.Expression.Tuple(elms, tpe, loc) => mutable.LinkedHashSet.empty ++ elms.flatMap(freeVariables)
    case SimplifiedAst.Expression.Existential(fparam, exp, loc) =>
      freeVariables(exp).filterNot { v => v._1 == fparam.sym }
    case SimplifiedAst.Expression.Universal(fparam, exp, loc) =>
      freeVariables(exp).filterNot { v => v._1 == fparam.sym }
    case SimplifiedAst.Expression.NativeConstructor(constructor, args, tpe, loc) => mutable.LinkedHashSet.empty ++ args.flatMap(freeVariables)
    case SimplifiedAst.Expression.NativeField(field, tpe, loc) => mutable.LinkedHashSet.empty
    case SimplifiedAst.Expression.NativeMethod(method, args, tpe, loc) => mutable.LinkedHashSet.empty ++ args.flatMap(freeVariables)
    case SimplifiedAst.Expression.UserError(tpe, loc) => mutable.LinkedHashSet.empty
    case SimplifiedAst.Expression.MatchError(tpe, loc) => mutable.LinkedHashSet.empty
    case SimplifiedAst.Expression.SwitchError(tpe, loc) => mutable.LinkedHashSet.empty
  }

  /**
    * Applies the given substitution map `subst` to the given expression `e`.
    */
  private def replace(e0: Expression, subst: Map[Symbol.VarSym, Symbol.VarSym]): Expression = {
    def visit(e: Expression): Expression = e match {
      case Expression.Unit => e
      case Expression.True => e
      case Expression.False => e
      case Expression.Char(lit) => e
      case Expression.Float32(lit) => e
      case Expression.Float64(lit) => e
      case Expression.Int8(lit) => e
      case Expression.Int16(lit) => e
      case Expression.Int32(lit) => e
      case Expression.Int64(lit) => e
      case Expression.BigInt(lit) => e
      case Expression.Str(lit) => e
      case Expression.Var(sym, tpe, loc) => subst.get(sym) match {
        case None => Expression.Var(sym, tpe, loc)
        case Some(newSym) => Expression.Var(newSym, tpe, loc)
      }
      case Expression.Def(name, tpe, loc) => e
      case Expression.Lambda(fparams, exp, tpe, loc) =>
        val fs = fparams.map(fparam => replace(fparam, subst))
        val e = visit(exp)
        Expression.Lambda(fs, e, tpe, loc)
      case Expression.Hook(hook, tpe, loc) => e
      case Expression.MkClosureRef(ref, freeVars, tpe, loc) => e
      case Expression.MkClosure(exp, freeVars, tpe, loc) =>
        val e = visit(exp).asInstanceOf[Expression.Lambda]
        Expression.MkClosure(e, freeVars, tpe, loc)
      case Expression.ApplyRef(sym, args, tpe, loc) =>
        val as = args map visit
        Expression.ApplyRef(sym, as, tpe, loc)
      case Expression.ApplyTail(sym, fparams, args, tpe, loc) =>
        val as = args map visit
        Expression.ApplyTail(sym, fparams, as, tpe, loc)
      case Expression.ApplyHook(hook, args, tpe, loc) =>
        val as = args map visit
        Expression.ApplyHook(hook, as, tpe, loc)
      case Expression.Apply(exp, args, tpe, loc) =>
        val e = visit(exp)
        val as = args map visit
        Expression.Apply(e, as, tpe, loc)
      case Expression.Unary(op, exp, tpe, loc) =>
        val e = visit(exp)
        Expression.Unary(op, e, tpe, loc)
      case Expression.Binary(op, exp1, exp2, tpe, loc) =>
        val e1 = visit(exp1)
        val e2 = visit(exp2)
        Expression.Binary(op, e1, e2, tpe, loc)
      case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
        val e1 = visit(exp1)
        val e2 = visit(exp2)
        val e3 = visit(exp3)
        Expression.IfThenElse(e1, e2, e3, tpe, loc)
      case Expression.Let(sym, exp1, exp2, tpe, loc) =>
        val e1 = visit(exp1)
        val e2 = visit(exp2)
        subst.get(sym) match {
          case None => Expression.Let(sym, e1, e2, tpe, loc)
          case Some(newSym) => Expression.Let(newSym, e1, e2, tpe, loc)
        }
      case Expression.LetRec(sym, exp1, exp2, tpe, loc) =>
        val e1 = visit(exp1)
        val e2 = visit(exp2)
        subst.get(sym) match {
          case None => Expression.LetRec(sym, e1, e2, tpe, loc)
          case Some(newSym) => Expression.LetRec(newSym, e1, e2, tpe, loc)
        }
      case Expression.Is(sym, tag, exp, loc) =>
        val e = visit(exp)
        Expression.Is(sym, tag, e, loc)
      case Expression.Untag(sym, tag, exp, tpe, loc) =>
        val e = visit(exp)
        Expression.Untag(sym, tag, e, tpe, loc)
      case Expression.Tag(enum, tag, exp, tpe, loc) =>
        val e = visit(exp)
        Expression.Tag(enum, tag, e, tpe, loc)
      case Expression.Index(exp, offset, tpe, loc) =>
        val e = visit(exp)
        Expression.Index(e, offset, tpe, loc)
      case Expression.Tuple(elms, tpe, loc) =>
        val es = elms map visit
        Expression.Tuple(es, tpe, loc)
      case Expression.Existential(fparam, exp, loc) =>
        val fs = replace(fparam, subst)
        val e = visit(exp)
        Expression.Existential(fs, e, loc)
      case Expression.Universal(fparam, exp, loc) =>
        val fs = replace(fparam, subst)
        val e = visit(exp)
        Expression.Universal(fs, e, loc)
      case Expression.NativeConstructor(constructor, args, tpe, loc) =>
        val es = args map visit
        Expression.NativeConstructor(constructor, es, tpe, loc)
      case Expression.NativeField(field, tpe, loc) => e
      case Expression.NativeMethod(method, args, tpe, loc) =>
        val es = args map visit
        Expression.NativeMethod(method, es, tpe, loc)
      case Expression.UserError(tpe, loc) => e
      case Expression.MatchError(tpe, loc) => e
      case Expression.SwitchError(tpe, loc) => e
    }

    visit(e0)
  }

  /**
    * Applies the given substitution map `subst` to the given formal parameters `fs`.
    */
  private def replace(fparam: SimplifiedAst.FormalParam, subst: Map[Symbol.VarSym, Symbol.VarSym]): SimplifiedAst.FormalParam = fparam match {
    case SimplifiedAst.FormalParam(sym, mod, tpe, loc) =>
      subst.get(sym) match {
        case None => SimplifiedAst.FormalParam(sym, mod, tpe, loc)
        case Some(newSym) => SimplifiedAst.FormalParam(newSym, mod, tpe, loc)
      }
  }

}
