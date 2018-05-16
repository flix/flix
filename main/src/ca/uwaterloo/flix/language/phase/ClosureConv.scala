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
import ca.uwaterloo.flix.language.ast.SimplifiedAst.{Expression, HandlerBinding}
import ca.uwaterloo.flix.language.ast.{Ast, SimplifiedAst, SourceLocation, Symbol, Type}
import ca.uwaterloo.flix.util.InternalCompilerException

import scala.collection.mutable

object ClosureConv {

  /**
    * Performs closure conversion on the given expression `e`.
    */
  def convert(exp0: SimplifiedAst.Expression)(implicit genSym: GenSym): SimplifiedAst.Expression = exp0 match {
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

    case Expression.Var(sym, tpe, loc) => exp0

    case Expression.Def(sym, tpe, loc) =>
      // The Def expression did not occur in an Apply expression.
      // We must create a closure, without free variables, of the definition symbol.
      Expression.Closure(sym, List.empty, tpe, loc)

    case Expression.Eff(sym, tpe, loc) => ??? // TODO

    case Expression.Lambda(args, body, tpe, loc) =>
      // Retrieve the type of the function.
      val ts = tpe.typeArguments
      val (targs, tresult) = (ts.init, ts.last)

      // Convert lambdas to closures. This is the main part of the `convert` function.
      // Closure conversion happens as follows:

      // First, we collect the free variables in the lambda expression.
      // NB: We pass the lambda expression (instead of its body) to account for bound arguments.
      val freeVars = freeVariables(exp0).toList

      // We prepend the free variables to the arguments list. Thus all variables within the lambda body will be treated
      // uniformly. The implementation will supply values for the free variables, without any effort from the caller.
      // We introduce new symbols for each introduced parameter and replace their occurrence in the body.
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
      val lambda = Expression.Lambda(newArgs, convert(replace(body, subst.toMap)), newTpe, loc)

      // At this point, `lambda` is the original lambda expression, but with all free variables converted to new
      // arguments, prepended to the original arguments list. Additionally, any lambdas within the body have also been
      // closure converted.

      // We return a MkClosure node, which contains `lambda` (rewritten to have extra arguments so there are no more
      // free variables) as well as the cached `freeVars`. The closure will actually be created at run time, where the
      // values for the free variables are bound and stored in the closure structure. When the closure is called, the
      // bound values are passed as arguments.
      // Note that MkClosure keeps the old lambda type.
      // In a later phase, we will lift the lambda to a top-level definition.
      Expression.LambdaClosure(lambda, freeVars.map(v => SimplifiedAst.FreeVar(v._1, v._2)), tpe, loc)

    case Expression.Apply(e, args, tpe, loc) =>
      // We're trying to call some expression `e`. If `e` is a Ref, then it's a top-level function, so we directly call
      // it with ApplyRef. We remove the Ref node and don't recurse on it to avoid creating a closure.
      // We do something similar if `e` is a Hook, where we transform Apply to ApplyHook.
      e match {
        case Expression.Def(sym, _, _) => Expression.ApplyDef(sym, args.map(convert), tpe, loc)
        case Expression.Eff(sym, _, _) => Expression.ApplyEff(sym, args.map(convert), tpe, loc)
        case _ => Expression.ApplyClo(convert(e), args.map(convert), tpe, loc)
      }


    case Expression.Unary(sop, op, e, tpe, loc) =>
      Expression.Unary(sop, op, convert(e), tpe, loc)

    case Expression.Binary(sop, op, e1, e2, tpe, loc) =>
      Expression.Binary(sop, op, convert(e1), convert(e2), tpe, loc)

    case Expression.IfThenElse(e1, e2, e3, tpe, loc) =>
      Expression.IfThenElse(convert(e1), convert(e2), convert(e3), tpe, loc)

    case Expression.Branch(exp, branches, tpe, loc) =>
      val e = convert(exp)
      val bs = branches map {
        case (sym, br) => sym -> convert(br)
      }
      Expression.Branch(e, bs, tpe, loc)

    case Expression.JumpTo(sym, tpe, loc) =>
      Expression.JumpTo(sym, tpe, loc)

    case Expression.Let(sym, e1, e2, tpe, loc) =>
      Expression.Let(sym, convert(e1), convert(e2), tpe, loc)

    case Expression.LetRec(sym, e1, e2, tpe, loc) =>
      Expression.LetRec(sym, convert(e1), convert(e2), tpe, loc)

    case Expression.Is(sym, tag, e, loc) =>
      Expression.Is(sym, tag, convert(e), loc)

    case Expression.Tag(enum, tag, e, tpe, loc) =>
      Expression.Tag(enum, tag, convert(e), tpe, loc)

    case Expression.Untag(sym, tag, e, tpe, loc) =>
      Expression.Untag(sym, tag, convert(e), tpe, loc)

    case Expression.Index(e, offset, tpe, loc) =>
      Expression.Index(convert(e), offset, tpe, loc)

    case Expression.Tuple(elms, tpe, loc) =>
      Expression.Tuple(elms.map(convert), tpe, loc)

    case Expression.ArrayLit(elms, tpe, loc) =>
      Expression.ArrayLit(elms.map(convert), tpe, loc)

    case Expression.ArrayNew(elm, len, tpe, loc) =>
      val e1 = convert(elm)
      val e2 = convert(len)
      Expression.ArrayNew(e1, e2, tpe, loc)

    case Expression.ArrayLoad(exp1, exp2, tpe, loc) =>
      val e1 = convert(exp1)
      val e2 = convert(exp2)
      Expression.ArrayLoad(e1, e2, tpe, loc)

    case Expression.ArrayStore(exp1, exp2, exp3, tpe, loc) =>
      val e1 = convert(exp1)
      val e2 = convert(exp2)
      val e3 = convert(exp3)
      Expression.ArrayStore(e1, e2, e3, tpe, loc)

    case Expression.ArrayLength(exp, tpe, loc) =>
      val e = convert(exp)
      Expression.ArrayLength(e, tpe, loc)

    case Expression.ArraySlice(exp1, exp2, exp3, tpe, loc) =>
      val e1 = convert(exp1)
      val e2 = convert(exp2)
      val e3 = convert(exp3)
      Expression.ArraySlice(e1, e2, e3, tpe, loc)

    case Expression.Ref(exp, tpe, loc) =>
      val e = convert(exp)
      Expression.Ref(e, tpe, loc)

    case Expression.Deref(exp, tpe, loc) =>
      val e = convert(exp)
      Expression.Deref(e, tpe, loc)

    case Expression.Assign(exp1, exp2, tpe, loc) =>
      val e1 = convert(exp1)
      val e2 = convert(exp2)
      Expression.Assign(e1, e2, tpe, loc)

    case Expression.HandleWith(exp, bindings, tpe, loc) =>
      val e = convert(exp)
      val bs = bindings map {
        case HandlerBinding(sym, handler) => HandlerBinding(sym, convert(handler))
      }
      Expression.HandleWith(e, bs, tpe, loc)

    case Expression.Existential(params, e, loc) =>
      Expression.Existential(params, convert(e), loc)
    case Expression.Universal(params, e, loc) =>
      Expression.Universal(params, convert(e), loc)

    case Expression.NativeConstructor(constructor, args, tpe, loc) => exp0
    case Expression.NativeField(field, tpe, loc) => exp0
    case Expression.NativeMethod(method, args, tpe, loc) => exp0

    case Expression.UserError(tpe, loc) => exp0
    case Expression.HoleError(sym, tpe, eff, loc) => exp0
    case Expression.MatchError(tpe, loc) => exp0
    case Expression.SwitchError(tpe, loc) => exp0

    case Expression.Closure(ref, freeVars, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
    case Expression.LambdaClosure(lambda, freeVars, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
    case Expression.ApplyClo(e, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
    case Expression.ApplyDef(name, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
    case Expression.ApplyEff(name, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
    case Expression.ApplyCloTail(e, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
    case Expression.ApplyDefTail(name, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
    case Expression.ApplyEffTail(name, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
    case Expression.ApplySelfTail(name, formals, actuals, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
  }

  /**
    * Returns the free variables in the given expression `exp`.
    * Does a left-to-right traversal of the AST, collecting free variables in order, in a LinkedHashSet.
    */
  // TODO: Use immutable, but sorted data structure?
  def freeVariables(exp0: SimplifiedAst.Expression): mutable.LinkedHashSet[(Symbol.VarSym, Type)] = exp0 match {
    case Expression.Unit => mutable.LinkedHashSet.empty
    case Expression.True => mutable.LinkedHashSet.empty
    case Expression.False => mutable.LinkedHashSet.empty
    case Expression.Char(lit) => mutable.LinkedHashSet.empty
    case Expression.Float32(lit) => mutable.LinkedHashSet.empty
    case Expression.Float64(lit) => mutable.LinkedHashSet.empty
    case Expression.Int8(lit) => mutable.LinkedHashSet.empty
    case Expression.Int16(lit) => mutable.LinkedHashSet.empty
    case Expression.Int32(lit) => mutable.LinkedHashSet.empty
    case Expression.Int64(lit) => mutable.LinkedHashSet.empty
    case Expression.BigInt(lit) => mutable.LinkedHashSet.empty
    case Expression.Str(lit) => mutable.LinkedHashSet.empty
    case Expression.Var(sym, tpe, loc) => mutable.LinkedHashSet((sym, tpe))
    case Expression.Def(sym, tpe, loc) => mutable.LinkedHashSet.empty
    case Expression.Eff(sym, tpe, loc) => mutable.LinkedHashSet.empty
    case Expression.Lambda(args, body, tpe, loc) =>
      val bound = args.map(_.sym)
      freeVariables(body).filterNot { v => bound.contains(v._1) }
    case Expression.Apply(exp, args, tpe, loc) =>
      freeVariables(exp) ++ args.flatMap(freeVariables)
    case Expression.Unary(sop, op, exp, tpe, loc) => freeVariables(exp)
    case Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
      freeVariables(exp1) ++ freeVariables(exp2)
    case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
      freeVariables(exp1) ++ freeVariables(exp2) ++ freeVariables(exp3)
    case Expression.Branch(exp, branches, tpe, loc) =>
      mutable.LinkedHashSet.empty ++ freeVariables(exp) ++ (branches flatMap {
        case (sym, br) => freeVariables(br)
      })
    case Expression.JumpTo(sym, tpe, loc) => mutable.LinkedHashSet.empty
    case Expression.Let(sym, exp1, exp2, tpe, loc) =>
      val bound = sym
      freeVariables(exp1) ++ freeVariables(exp2).filterNot { v => bound == v._1 }
    case Expression.LetRec(sym, exp1, exp2, tpe, loc) =>
      val bound = sym
      (freeVariables(exp1) ++ freeVariables(exp2)).filterNot { v => bound == v._1 }
    case Expression.Is(sym, tag, exp, loc) => freeVariables(exp)
    case Expression.Untag(sym, tag, exp, tpe, loc) => freeVariables(exp)
    case Expression.Tag(enum, tag, exp, tpe, loc) => freeVariables(exp)
    case Expression.Index(base, offset, tpe, loc) => freeVariables(base)
    case Expression.Tuple(elms, tpe, loc) => mutable.LinkedHashSet.empty ++ elms.flatMap(freeVariables)
    case Expression.ArrayLit(elms, tpe, loc) => mutable.LinkedHashSet.empty ++ elms.flatMap(freeVariables)
    case Expression.ArrayNew(elm, len, tpe, loc) => freeVariables(elm) ++ freeVariables(len)
    case Expression.ArrayLoad(base, index, tpe, loc) => freeVariables(base) ++ freeVariables(index)
    case Expression.ArrayStore(base, index, elm, tpe, loc) => freeVariables(base) ++ freeVariables(index) ++ freeVariables(elm)
    case Expression.ArrayLength(base, tpe, loc) => freeVariables(base)
    case Expression.ArraySlice(base ,beginIndex, endIndex, tpe, loc) => freeVariables(base) ++ freeVariables(beginIndex) ++ freeVariables(endIndex)
    case Expression.Ref(exp, tpe, loc) => freeVariables(exp)
    case Expression.Deref(exp, tpe, loc) => freeVariables(exp)
    case Expression.Assign(exp1, exp2, tpe, loc) => freeVariables(exp1) ++ freeVariables(exp2)
    case Expression.HandleWith(exp, bindings, tpe, loc) => freeVariables(exp) ++ bindings.flatMap(b => freeVariables(b.exp))
    case Expression.Existential(fparam, exp, loc) =>
      freeVariables(exp).filterNot { v => v._1 == fparam.sym }
    case Expression.Universal(fparam, exp, loc) =>
      freeVariables(exp).filterNot { v => v._1 == fparam.sym }
    case Expression.NativeConstructor(constructor, args, tpe, loc) => mutable.LinkedHashSet.empty ++ args.flatMap(freeVariables)
    case Expression.NativeField(field, tpe, loc) => mutable.LinkedHashSet.empty
    case Expression.NativeMethod(method, args, tpe, loc) => mutable.LinkedHashSet.empty ++ args.flatMap(freeVariables)

    case Expression.UserError(tpe, loc) => mutable.LinkedHashSet.empty
    case Expression.HoleError(sym, tpe, eff, loc) => mutable.LinkedHashSet.empty
    case Expression.MatchError(tpe, loc) => mutable.LinkedHashSet.empty
    case Expression.SwitchError(tpe, loc) => mutable.LinkedHashSet.empty

    case Expression.LambdaClosure(lambda, freeVars, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
    case Expression.Closure(ref, freeVars, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
    case Expression.ApplyClo(exp, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
    case Expression.ApplyDef(sym, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
    case Expression.ApplyEff(sym, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
    case Expression.ApplyCloTail(sym, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
    case Expression.ApplyDefTail(sym, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
    case Expression.ApplyEffTail(sym, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
    case Expression.ApplySelfTail(sym, formals, actuals, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
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
      case Expression.Def(sym, tpe, loc) => e
      case Expression.Eff(sym, tpe, loc) => e
      case Expression.Lambda(fparams, exp, tpe, loc) =>
        val fs = fparams.map(fparam => replace(fparam, subst))
        val e = visit(exp)
        Expression.Lambda(fs, e, tpe, loc)
      case Expression.Closure(ref, freeVars, tpe, loc) => e
      case Expression.LambdaClosure(exp, freeVars, tpe, loc) =>
        val e = visit(exp).asInstanceOf[Expression.Lambda]
        Expression.LambdaClosure(e, freeVars, tpe, loc)
      case Expression.ApplyClo(exp, args, tpe, loc) =>
        val e = visit(exp)
        val as = args map visit
        Expression.ApplyClo(e, as, tpe, loc)
      case Expression.ApplyDef(sym, args, tpe, loc) =>
        val as = args map visit
        Expression.ApplyDef(sym, as, tpe, loc)
      case Expression.ApplyEff(sym, args, tpe, loc) =>
        val as = args map visit
        Expression.ApplyEff(sym, as, tpe, loc)
      case Expression.Apply(exp, args, tpe, loc) =>
        val e = visit(exp)
        val as = args map visit
        Expression.Apply(e, as, tpe, loc)
      case Expression.Unary(sop, op, exp, tpe, loc) =>
        val e = visit(exp)
        Expression.Unary(sop, op, e, tpe, loc)
      case Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
        val e1 = visit(exp1)
        val e2 = visit(exp2)
        Expression.Binary(sop, op, e1, e2, tpe, loc)
      case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
        val e1 = visit(exp1)
        val e2 = visit(exp2)
        val e3 = visit(exp3)
        Expression.IfThenElse(e1, e2, e3, tpe, loc)
      case Expression.Branch(exp, branches, tpe, loc) =>
        val e = visit(exp)
        val bs = branches map {
          case (sym, br) => sym -> visit(br)
        }
        Expression.Branch(e, bs, tpe, loc)
      case Expression.JumpTo(sym, tpe, loc) =>
        Expression.JumpTo(sym, tpe, loc)
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
      case Expression.ArrayLit(elms, tpe, loc) =>
        val es = elms map visit
        Expression.ArrayLit(es, tpe, loc)
      case Expression.ArrayNew(elm, len, tpe,loc) =>
        val e = visit(elm)
        val ln = visit(len)
        Expression.ArrayNew(e, ln, tpe, loc)
      case Expression.ArrayLoad(base, index, tpe, loc) =>
        val b = visit(base)
        val i = visit(index)
        Expression.ArrayLoad(b, i, tpe, loc)
      case Expression.ArrayStore(base, index, elm, tpe, loc) =>
        val b = visit(base)
        val i = visit(index)
        val e = visit(elm)
        Expression.ArrayStore(b, i, e, tpe, loc )
      case Expression.ArrayLength(base, tpe, loc) =>
        val b = visit(base)
        Expression.ArrayLength(b, tpe, loc)
      case Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) =>
        val b = visit(base)
        val i1 = visit(beginIndex)
        val i2 = visit(endIndex)
        Expression.ArraySlice(b, i1, i2, tpe, loc)
      case Expression.Ref(exp, tpe, loc) =>
        val e = visit(exp)
        Expression.Ref(e, tpe, loc)
      case Expression.Deref(exp, tpe, loc) =>
        val e = visit(exp)
        Expression.Deref(e, tpe, loc)
      case Expression.Assign(exp1, exp2, tpe, loc) =>
        val e1 = visit(exp1)
        val e2 = visit(exp2)
        Expression.Assign(e1, e2, tpe, loc)
      case Expression.HandleWith(exp, bindings, tpe, loc) =>
        val e = visit(exp)
        val bs = bindings map {
          case HandlerBinding(sym, handler) => HandlerBinding(sym, visit(handler))
        }
        Expression.HandleWith(e, bs, tpe, loc)
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
      case Expression.HoleError(sym, tpe, eff, loc) => e
      case Expression.MatchError(tpe, loc) => e
      case Expression.SwitchError(tpe, loc) => e

      case Expression.ApplyCloTail(exp, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${e.getClass.getSimpleName}'.")
      case Expression.ApplyDefTail(sym, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${e.getClass.getSimpleName}'.")
      case Expression.ApplyEffTail(sym, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${e.getClass.getSimpleName}'.")
      case Expression.ApplySelfTail(sym, fparams, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${e.getClass.getSimpleName}'.")
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
