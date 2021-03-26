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
import ca.uwaterloo.flix.language.ast.{Ast, Symbol, Type}
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.{InternalCompilerException, Validation}

import scala.collection.mutable

object ClosureConv extends Phase[Root, Root] {

  /**
    * Performs closure conversion on the given AST `root`.
    */
  def run(root: Root)(implicit flix: Flix): Validation[Root, CompilationError] = flix.phase("ClosureConv") {

    // Definitions.
    val definitions = root.defs.map {
      case (sym, decl) => sym -> visitDef(decl)
    }

    // Properties.
    val properties = root.properties.map {
      property => visitProperty(property)
    }

    // Return the updated AST root.
    root.copy(defs = definitions, properties = properties).toSuccess
  }

  /**
    * Performs closure conversion on the given definition `def0`.
    */
  private def visitDef(def0: Def)(implicit flix: Flix): Def = {
    val convertedExp = visitExp(def0.exp)
    def0.copy(exp = convertedExp)
  }

  /**
    * Performs closure conversion on the given property `property0`.
    */
  private def visitProperty(property0: Property)(implicit flix: Flix): Property = {
    val convertedExp = visitExp(property0.exp)

    // Reassemble the property.
    property0.copy(exp = convertedExp)
  }

  /**
    * Performs closure conversion on the given expression `exp0`.
    */
  private def visitExp(exp0: Expression)(implicit flix: Flix): Expression = exp0 match {
    case Expression.Unit(_) => exp0

    case Expression.Null(_, _) => exp0

    case Expression.True(_) => exp0

    case Expression.False(_) => exp0

    case Expression.Char(lit, _) => exp0

    case Expression.Float32(lit, _) => exp0

    case Expression.Float64(lit, _) => exp0

    case Expression.Int8(lit, _) => exp0

    case Expression.Int16(lit, _) => exp0

    case Expression.Int32(lit, _) => exp0

    case Expression.Int64(lit, _) => exp0

    case Expression.BigInt(lit, _) => exp0

    case Expression.Str(lit, _) => exp0

    case Expression.Var(sym, tpe, loc) => exp0

    case Expression.Def(sym, tpe, loc) =>
      // The Def expression did not occur in an Apply expression.
      // We must create a closure, without free variables, of the definition symbol.
      Expression.Closure(sym, List.empty, tpe, loc)

    case Expression.Lambda(args, body, tpe, loc) =>
      // Retrieve the type of the function.
      val ts = tpe.typeArguments
      val (targs, tresult) = (ts.init, ts.last)

      // Convert lambdas to closures. This is the main part of the `convert` function.
      // Closure conversion happens as follows:

      // First, we collect the free variables in the lambda expression.
      // NB: We pass the lambda expression (instead of its body) to account for bound arguments.
      val fvs = freeVars(exp0).toList

      // We prepend the free variables to the arguments list. Thus all variables within the lambda body will be treated
      // uniformly. The implementation will supply values for the free variables, without any effort from the caller.
      // We introduce new symbols for each introduced parameter and replace their occurrence in the body.
      val subst = mutable.Map.empty[Symbol.VarSym, Symbol.VarSym]
      val newArgs = fvs.map {
        case (oldSym, ptype) =>
          val newSym = Symbol.freshVarSym(oldSym)
          subst += (oldSym -> newSym)
          FormalParam(newSym, Ast.Modifiers.Empty, ptype, loc)
      } ++ args

      // Update the lambda type.
      val argTpes = fvs.map(_._2) ++ targs
      val newTpe = Type.mkPureUncurriedArrow(argTpes, tresult)

      val newBody = visitExp(replace(body, subst.toMap))
      // We rewrite the lambda with its new arguments list and new body, with any nested lambdas also converted.
      val lambda = Expression.Lambda(newArgs, newBody, newTpe, loc)

      // At this point, `lambda` is the original lambda expression, but with all free variables converted to new
      // arguments, prepended to the original arguments list. Additionally, any lambdas within the body have also been
      // closure converted.

      // We return a MkClosure node, which contains `lambda` (rewritten to have extra arguments so there are no more
      // free variables) as well as the cached `freeVars`. The closure will actually be created at run time, where the
      // values for the free variables are bound and stored in the closure structure. When the closure is called, the
      // bound values are passed as arguments.
      // Note that MkClosure keeps the old lambda type.
      // In a later phase, we will lift the lambda to a top-level definition.
      //Expression.LambdaClosure(lambda, fvs.map(v => FreeVar(v._1, v._2)), tpe, loc)
      Expression.LambdaClosure(newArgs, fvs.map(v => FreeVar(v._1, v._2)), newBody, tpe, loc)

    case Expression.Apply(e, args, tpe, loc) =>
      // We're trying to call some expression `e`. If `e` is a Ref, then it's a top-level function, so we directly call
      // it with ApplyRef. We remove the Ref node and don't recurse on it to avoid creating a closure.
      // We do something similar if `e` is a Hook, where we transform Apply to ApplyHook.
      e match {
        case Expression.Def(sym, _, _) => Expression.ApplyDef(sym, args.map(visitExp), tpe, loc)
        case _ => Expression.ApplyClo(visitExp(e), args.map(visitExp), tpe, loc)
      }

    case Expression.Unary(sop, op, e, tpe, loc) =>
      Expression.Unary(sop, op, visitExp(e), tpe, loc)

    case Expression.Binary(sop, op, e1, e2, tpe, loc) =>
      Expression.Binary(sop, op, visitExp(e1), visitExp(e2), tpe, loc)

    case Expression.IfThenElse(e1, e2, e3, tpe, loc) =>
      Expression.IfThenElse(visitExp(e1), visitExp(e2), visitExp(e3), tpe, loc)

    case Expression.Branch(exp, branches, tpe, loc) =>
      val e = visitExp(exp)
      val bs = branches map {
        case (sym, br) => sym -> visitExp(br)
      }
      Expression.Branch(e, bs, tpe, loc)

    case Expression.JumpTo(sym, tpe, loc) =>
      Expression.JumpTo(sym, tpe, loc)

    case Expression.Let(sym, e1, e2, tpe, loc) =>
      Expression.Let(sym, visitExp(e1), visitExp(e2), tpe, loc)

    case Expression.Is(sym, tag, e, loc) =>
      Expression.Is(sym, tag, visitExp(e), loc)

    case Expression.Tag(enum, tag, e, tpe, loc) =>
      Expression.Tag(enum, tag, visitExp(e), tpe, loc)

    case Expression.Untag(sym, tag, e, tpe, loc) =>
      Expression.Untag(sym, tag, visitExp(e), tpe, loc)

    case Expression.Index(e, offset, tpe, loc) =>
      Expression.Index(visitExp(e), offset, tpe, loc)

    case Expression.Tuple(elms, tpe, loc) =>
      Expression.Tuple(elms.map(visitExp), tpe, loc)

    case Expression.RecordEmpty(tpe, loc) =>
      Expression.RecordEmpty(tpe, loc)

    case Expression.RecordSelect(exp, field, tpe, loc) =>
      val e = visitExp(exp)
      Expression.RecordSelect(e, field, tpe, loc)

    case Expression.RecordExtend(field, value, rest, tpe, loc) =>
      val v = visitExp(value)
      val r = visitExp(rest)
      Expression.RecordExtend(field, v, r, tpe, loc)

    case Expression.RecordRestrict(field, rest, tpe, loc) =>
      val r = visitExp(rest)
      Expression.RecordRestrict(field, r, tpe, loc)

    case Expression.ArrayLit(elms, tpe, loc) =>
      Expression.ArrayLit(elms.map(visitExp), tpe, loc)

    case Expression.ArrayNew(elm, len, tpe, loc) =>
      val e1 = visitExp(elm)
      val e2 = visitExp(len)
      Expression.ArrayNew(e1, e2, tpe, loc)

    case Expression.ArrayLoad(exp1, exp2, tpe, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      Expression.ArrayLoad(e1, e2, tpe, loc)

    case Expression.ArrayStore(exp1, exp2, exp3, tpe, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val e3 = visitExp(exp3)
      Expression.ArrayStore(e1, e2, e3, tpe, loc)

    case Expression.ArrayLength(exp, tpe, loc) =>
      val e = visitExp(exp)
      Expression.ArrayLength(e, tpe, loc)

    case Expression.ArraySlice(exp1, exp2, exp3, tpe, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val e3 = visitExp(exp3)
      Expression.ArraySlice(e1, e2, e3, tpe, loc)

    case Expression.Ref(exp, tpe, loc) =>
      val e = visitExp(exp)
      Expression.Ref(e, tpe, loc)

    case Expression.Deref(exp, tpe, loc) =>
      val e = visitExp(exp)
      Expression.Deref(e, tpe, loc)

    case Expression.Assign(exp1, exp2, tpe, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      Expression.Assign(e1, e2, tpe, loc)

    case Expression.Existential(params, e, loc) =>
      Expression.Existential(params, visitExp(e), loc)

    case Expression.Universal(params, e, loc) =>
      Expression.Universal(params, visitExp(e), loc)

    case Expression.Cast(exp, tpe, loc) =>
      val e = visitExp(exp)
      Expression.Cast(e, tpe, loc)

    case Expression.TryCatch(exp, rules, tpe, loc) =>
      val e = visitExp(exp)
      val rs = rules map {
        case CatchRule(sym, clazz, body) =>
          val b = visitExp(body)
          CatchRule(sym, clazz, b)
      }
      Expression.TryCatch(e, rs, tpe, loc)

    case Expression.InvokeConstructor(constructor, args, tpe, loc) =>
      val as = args map visitExp
      Expression.InvokeConstructor(constructor, as, tpe, loc)

    case Expression.InvokeMethod(method, exp, args, tpe, loc) =>
      val e = visitExp(exp)
      val as = args.map(visitExp)
      Expression.InvokeMethod(method, e, as, tpe, loc)

    case Expression.InvokeStaticMethod(method, args, tpe, loc) =>
      val as = args.map(visitExp)
      Expression.InvokeStaticMethod(method, as, tpe, loc)

    case Expression.GetField(field, exp, tpe, loc) =>
      val e = visitExp(exp)
      Expression.GetField(field, e, tpe, loc)

    case Expression.PutField(field, exp1, exp2, tpe, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      Expression.PutField(field, e1, e2, tpe, loc)

    case Expression.GetStaticField(field, tpe, loc) =>
      Expression.GetStaticField(field, tpe, loc)

    case Expression.PutStaticField(field, exp, tpe, loc) =>
      val e = visitExp(exp)
      Expression.PutStaticField(field, e, tpe, loc)

    case Expression.NewChannel(exp, tpe, loc) =>
      val e = visitExp(exp)
      Expression.NewChannel(e, tpe, loc)

    case Expression.GetChannel(exp, tpe, loc) =>
      val e = visitExp(exp)
      Expression.GetChannel(e, tpe, loc)

    case Expression.PutChannel(exp1, exp2, tpe, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      Expression.PutChannel(e1, e2, tpe, loc)

    case Expression.SelectChannel(rules, default, tpe, loc) =>
      val rs = rules map {
        case SelectChannelRule(sym, chan, exp) =>
          val c = visitExp(chan)
          val e = visitExp(exp)
          SelectChannelRule(sym, c, e)
      }

      val d = default.map(visitExp(_))

      Expression.SelectChannel(rs, d, tpe, loc)

    case Expression.Spawn(exp, tpe, loc) =>
      val e = visitExp(exp)
      Expression.Spawn(e, tpe, loc)

    case Expression.Lazy(exp, tpe, loc) =>
      val e = visitExp(exp)
      Expression.Lazy(e, tpe, loc)

    case Expression.Force(exp, tpe, loc) =>
      val e = visitExp(exp)
      Expression.Force(e, tpe, loc)

    case Expression.HoleError(sym, tpe, loc) => exp0
    case Expression.MatchError(tpe, loc) => exp0

    case Expression.Closure(_, _, _, _) => throw InternalCompilerException(s"Unexpected expression.")
    case Expression.LambdaClosure(_, _, _, _, _) => throw InternalCompilerException(s"Unexpected expression.")
    case Expression.ApplyClo(_, _, _, _) => throw InternalCompilerException(s"Unexpected expression.")
    case Expression.ApplyDef(_, _, _, _) => throw InternalCompilerException(s"Unexpected expression.")
  }

  /**
    * Returns the free variables in the given expression `exp`.
    * Does a left-to-right traversal of the AST, collecting free variables in order, in a LinkedHashSet.
    */
  // TODO: Use immutable, but sorted data structure?
  private def freeVars(exp0: Expression): mutable.LinkedHashSet[(Symbol.VarSym, Type)] = exp0 match {
    case Expression.Unit(_) => mutable.LinkedHashSet.empty
    case Expression.Null(_, _) => mutable.LinkedHashSet.empty
    case Expression.True(_) => mutable.LinkedHashSet.empty
    case Expression.False(_) => mutable.LinkedHashSet.empty
    case Expression.Char(_, _) => mutable.LinkedHashSet.empty
    case Expression.Float32(_, _) => mutable.LinkedHashSet.empty
    case Expression.Float64(_, _) => mutable.LinkedHashSet.empty
    case Expression.Int8(_, _) => mutable.LinkedHashSet.empty
    case Expression.Int16(_, _) => mutable.LinkedHashSet.empty
    case Expression.Int32(_, _) => mutable.LinkedHashSet.empty
    case Expression.Int64(_, _) => mutable.LinkedHashSet.empty
    case Expression.BigInt(_, _) => mutable.LinkedHashSet.empty
    case Expression.Str(_, _) => mutable.LinkedHashSet.empty
    case Expression.Var(sym, tpe, loc) => mutable.LinkedHashSet((sym, tpe))
    case Expression.Def(sym, tpe, loc) => mutable.LinkedHashSet.empty
    case Expression.Lambda(args, body, tpe, loc) =>
      val bound = args.map(_.sym)
      freeVars(body).filterNot { v => bound.contains(v._1) }
    case Expression.Apply(exp, args, tpe, loc) =>
      freeVars(exp) ++ args.flatMap(freeVars)
    case Expression.Unary(sop, op, exp, tpe, loc) => freeVars(exp)
    case Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
      freeVars(exp1) ++ freeVars(exp2)
    case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
      freeVars(exp1) ++ freeVars(exp2) ++ freeVars(exp3)
    case Expression.Branch(exp, branches, tpe, loc) =>
      mutable.LinkedHashSet.empty ++ freeVars(exp) ++ (branches flatMap {
        case (sym, br) => freeVars(br)
      })
    case Expression.JumpTo(sym, tpe, loc) => mutable.LinkedHashSet.empty
    case Expression.Let(sym, exp1, exp2, tpe, loc) =>
      val bound = sym
      freeVars(exp1) ++ freeVars(exp2).filterNot { v => bound == v._1 }
    case Expression.Is(sym, tag, exp, loc) => freeVars(exp)
    case Expression.Untag(sym, tag, exp, tpe, loc) => freeVars(exp)
    case Expression.Tag(enum, tag, exp, tpe, loc) => freeVars(exp)
    case Expression.Index(base, offset, tpe, loc) => freeVars(base)
    case Expression.Tuple(elms, tpe, loc) => mutable.LinkedHashSet.empty ++ elms.flatMap(freeVars)
    case Expression.RecordEmpty(tpe, loc) => mutable.LinkedHashSet.empty
    case Expression.RecordSelect(exp, field, tpe, loc) => freeVars(exp)
    case Expression.RecordExtend(field, value, rest, tpe, loc) => freeVars(value) ++ freeVars(rest)
    case Expression.RecordRestrict(field, rest, tpe, loc) => freeVars(rest)
    case Expression.ArrayLit(elms, tpe, loc) => mutable.LinkedHashSet.empty ++ elms.flatMap(freeVars)
    case Expression.ArrayNew(elm, len, tpe, loc) => freeVars(elm) ++ freeVars(len)
    case Expression.ArrayLoad(base, index, tpe, loc) => freeVars(base) ++ freeVars(index)
    case Expression.ArrayStore(base, index, elm, tpe, loc) => freeVars(base) ++ freeVars(index) ++ freeVars(elm)
    case Expression.ArrayLength(base, tpe, loc) => freeVars(base)
    case Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) => freeVars(base) ++ freeVars(beginIndex) ++ freeVars(endIndex)
    case Expression.Ref(exp, tpe, loc) => freeVars(exp)
    case Expression.Deref(exp, tpe, loc) => freeVars(exp)
    case Expression.Assign(exp1, exp2, tpe, loc) => freeVars(exp1) ++ freeVars(exp2)
    case Expression.Existential(fparam, exp, loc) =>
      freeVars(exp).filterNot { v => v._1 == fparam.sym }
    case Expression.Universal(fparam, exp, loc) =>
      freeVars(exp).filterNot { v => v._1 == fparam.sym }

    case Expression.Cast(exp, tpe, loc) => freeVars(exp)

    case Expression.TryCatch(exp, rules, tpe, loc) => mutable.LinkedHashSet.empty ++ freeVars(exp) ++ rules.flatMap(r => freeVars(r.exp).filterNot(_._1 == r.sym))

    case Expression.InvokeConstructor(constructor, args, tpe, loc) => mutable.LinkedHashSet.empty ++ args.flatMap(freeVars)

    case Expression.InvokeMethod(method, exp, args, tpe, loc) => freeVars(exp) ++ args.flatMap(freeVars)

    case Expression.InvokeStaticMethod(method, args, tpe, loc) => mutable.LinkedHashSet.empty ++ args.flatMap(freeVars)

    case Expression.GetField(field, exp, tpe, loc) => freeVars(exp)

    case Expression.PutField(field, exp1, exp2, tpe, loc) => freeVars(exp1) ++ freeVars(exp2)

    case Expression.GetStaticField(field, tpe, loc) => mutable.LinkedHashSet.empty

    case Expression.PutStaticField(field, exp, tpe, loc) => freeVars(exp)

    case Expression.NewChannel(exp, tpe, loc) => freeVars(exp)

    case Expression.GetChannel(exp, tpe, loc) => freeVars(exp)

    case Expression.PutChannel(exp1, exp2, tpe, loc) => freeVars(exp1) ++ freeVars(exp2)

    case Expression.SelectChannel(rules, default, tpe, loc) =>
      val rs = mutable.LinkedHashSet.empty ++ rules.flatMap {
        case SelectChannelRule(sym, chan, exp) => (freeVars(chan) ++ freeVars(exp)).filter(p => p._1 != sym)
      }

      val d = default.map(freeVars).getOrElse(mutable.LinkedHashSet.empty)

      rs ++ d

    case Expression.Spawn(exp, tpe, loc) => freeVars(exp)

    case Expression.Lazy(exp, tpe, loc) => freeVars(exp)

    case Expression.Force(exp, tpe, loc) => freeVars(exp)

    case Expression.HoleError(sym, tpe, loc) => mutable.LinkedHashSet.empty
    case Expression.MatchError(tpe, loc) => mutable.LinkedHashSet.empty

    case Expression.LambdaClosure(_, _, _, _, _) => throw InternalCompilerException(s"Unexpected expression.")
    case Expression.Closure(_, _, _, _) => throw InternalCompilerException(s"Unexpected expression.")
    case Expression.ApplyClo(_, _, _, _) => throw InternalCompilerException(s"Unexpected expression.")
    case Expression.ApplyDef(_, _, _, _) => throw InternalCompilerException(s"Unexpected expression.")
  }

  /**
    * Applies the given substitution map `subst` to the given expression `e`.
    */
  private def replace(e0: Expression, subst: Map[Symbol.VarSym, Symbol.VarSym])(implicit flix: Flix): Expression = {

    def visitExp(e: Expression): Expression = e match {
      case Expression.Unit(_) => e

      case Expression.Null(_, _) => e

      case Expression.True(_) => e

      case Expression.False(_) => e

      case Expression.Char(_, _) => e

      case Expression.Float32(_, _) => e

      case Expression.Float64(_, _) => e

      case Expression.Int8(_, _) => e

      case Expression.Int16(_, _) => e

      case Expression.Int32(_, _) => e

      case Expression.Int64(_, _) => e

      case Expression.BigInt(_, _) => e

      case Expression.Str(_, _) => e

      case Expression.Var(sym, tpe, loc) => subst.get(sym) match {
        case None => Expression.Var(sym, tpe, loc)
        case Some(newSym) => Expression.Var(newSym, tpe, loc)
      }

      case Expression.Def(sym, tpe, loc) => e

      case Expression.Lambda(fparams, exp, tpe, loc) =>
        val fs = fparams.map(fparam => replace(fparam, subst))
        val e = visitExp(exp)
        Expression.Lambda(fs, e, tpe, loc)

      case Expression.Closure(ref, freeVars, tpe, loc) => e

      case Expression.LambdaClosure(fparams, freeVars, exp, tpe, loc) =>
        val e = visitExp(exp).asInstanceOf[Expression.Lambda]
        Expression.LambdaClosure(fparams, freeVars, exp, tpe, loc)

      case Expression.ApplyClo(exp, args, tpe, loc) =>
        val e = visitExp(exp)
        val as = args map visitExp
        Expression.ApplyClo(e, as, tpe, loc)

      case Expression.ApplyDef(sym, args, tpe, loc) =>
        val as = args map visitExp
        Expression.ApplyDef(sym, as, tpe, loc)

      case Expression.Apply(exp, args, tpe, loc) =>
        val e = visitExp(exp)
        val as = args map visitExp
        Expression.Apply(e, as, tpe, loc)

      case Expression.Unary(sop, op, exp, tpe, loc) =>
        val e = visitExp(exp)
        Expression.Unary(sop, op, e, tpe, loc)

      case Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        Expression.Binary(sop, op, e1, e2, tpe, loc)

      case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        val e3 = visitExp(exp3)
        Expression.IfThenElse(e1, e2, e3, tpe, loc)

      case Expression.Branch(exp, branches, tpe, loc) =>
        val e = visitExp(exp)
        val bs = branches map {
          case (sym, br) => sym -> visitExp(br)
        }
        Expression.Branch(e, bs, tpe, loc)

      case Expression.JumpTo(sym, tpe, loc) =>
        Expression.JumpTo(sym, tpe, loc)

      case Expression.Let(sym, exp1, exp2, tpe, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        subst.get(sym) match {
          case None => Expression.Let(sym, e1, e2, tpe, loc)
          case Some(newSym) => Expression.Let(newSym, e1, e2, tpe, loc)
        }

      case Expression.Is(sym, tag, exp, loc) =>
        val e = visitExp(exp)
        Expression.Is(sym, tag, e, loc)

      case Expression.Untag(sym, tag, exp, tpe, loc) =>
        val e = visitExp(exp)
        Expression.Untag(sym, tag, e, tpe, loc)

      case Expression.Tag(enum, tag, exp, tpe, loc) =>
        val e = visitExp(exp)
        Expression.Tag(enum, tag, e, tpe, loc)

      case Expression.Index(exp, offset, tpe, loc) =>
        val e = visitExp(exp)
        Expression.Index(e, offset, tpe, loc)

      case Expression.Tuple(elms, tpe, loc) =>
        val es = elms map visitExp
        Expression.Tuple(es, tpe, loc)

      case Expression.RecordEmpty(tpe, loc) =>
        Expression.RecordEmpty(tpe, loc)

      case Expression.RecordSelect(base, field, tpe, loc) =>
        val b = visitExp(base)
        Expression.RecordSelect(b, field, tpe, loc)

      case Expression.RecordExtend(field, value, rest, tpe, loc) =>
        val v = visitExp(value)
        val r = visitExp(rest)
        Expression.RecordExtend(field, v, r, tpe, loc)

      case Expression.RecordRestrict(field, rest, tpe, loc) =>
        val r = visitExp(rest)
        Expression.RecordRestrict(field, r, tpe, loc)

      case Expression.ArrayLit(elms, tpe, loc) =>
        val es = elms map visitExp
        Expression.ArrayLit(es, tpe, loc)

      case Expression.ArrayNew(elm, len, tpe, loc) =>
        val e = visitExp(elm)
        val ln = visitExp(len)
        Expression.ArrayNew(e, ln, tpe, loc)

      case Expression.ArrayLoad(base, index, tpe, loc) =>
        val b = visitExp(base)
        val i = visitExp(index)
        Expression.ArrayLoad(b, i, tpe, loc)

      case Expression.ArrayStore(base, index, elm, tpe, loc) =>
        val b = visitExp(base)
        val i = visitExp(index)
        val e = visitExp(elm)
        Expression.ArrayStore(b, i, e, tpe, loc)

      case Expression.ArrayLength(base, tpe, loc) =>
        val b = visitExp(base)
        Expression.ArrayLength(b, tpe, loc)

      case Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) =>
        val b = visitExp(base)
        val i1 = visitExp(beginIndex)
        val i2 = visitExp(endIndex)
        Expression.ArraySlice(b, i1, i2, tpe, loc)

      case Expression.Ref(exp, tpe, loc) =>
        val e = visitExp(exp)
        Expression.Ref(e, tpe, loc)

      case Expression.Deref(exp, tpe, loc) =>
        val e = visitExp(exp)
        Expression.Deref(e, tpe, loc)

      case Expression.Assign(exp1, exp2, tpe, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        Expression.Assign(e1, e2, tpe, loc)

      case Expression.Existential(fparam, exp, loc) =>
        val fs = replace(fparam, subst)
        val e = visitExp(exp)
        Expression.Existential(fs, e, loc)

      case Expression.Universal(fparam, exp, loc) =>
        val fs = replace(fparam, subst)
        val e = visitExp(exp)
        Expression.Universal(fs, e, loc)

      case Expression.Cast(exp, tpe, loc) =>
        val e = visitExp(exp)
        Expression.Cast(e, tpe, loc)

      case Expression.TryCatch(exp, rules, tpe, loc) =>
        val e = visitExp(exp)
        val rs = rules map {
          case CatchRule(sym, clazz, body) =>
            val b = visitExp(body)
            CatchRule(sym, clazz, b)
        }
        Expression.TryCatch(e, rs, tpe, loc)

      case Expression.InvokeConstructor(constructor, args, tpe, loc) =>
        val as = args.map(visitExp)
        Expression.InvokeConstructor(constructor, as, tpe, loc)

      case Expression.InvokeMethod(method, exp, args, tpe, loc) =>
        val e = visitExp(exp)
        val as = args.map(visitExp)
        Expression.InvokeMethod(method, e, as, tpe, loc)

      case Expression.InvokeStaticMethod(method, args, tpe, loc) =>
        val as = args.map(visitExp)
        Expression.InvokeStaticMethod(method, as, tpe, loc)

      case Expression.GetField(field, exp, tpe, loc) =>
        val e = visitExp(exp)
        Expression.GetField(field, e, tpe, loc)

      case Expression.PutField(field, exp1, exp2, tpe, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        Expression.PutField(field, e1, e2, tpe, loc)

      case Expression.GetStaticField(field, tpe, loc) =>
        e

      case Expression.PutStaticField(field, exp, tpe, loc) =>
        val e = visitExp(exp)
        Expression.PutStaticField(field, e, tpe, loc)

      case Expression.NewChannel(exp, tpe, loc) =>
        val e = visitExp(exp)
        Expression.NewChannel(e, tpe, loc)

      case Expression.GetChannel(exp, tpe, loc) =>
        val e = visitExp(exp)
        Expression.GetChannel(e, tpe, loc)

      case Expression.PutChannel(exp1, exp2, eff, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        Expression.PutChannel(e1, e2, eff, loc)

      case Expression.SelectChannel(rules, default, tpe, loc) =>
        val rs = rules map {
          case SelectChannelRule(sym, chan, exp) =>
            val c = visitExp(chan)
            val e = visitExp(exp)
            SelectChannelRule(sym, c, e)
        }

        val d = default.map(visitExp)

        Expression.SelectChannel(rs, d, tpe, loc)

      case Expression.Spawn(exp, tpe, loc) =>
        val e = visitExp(exp)
        Expression.Spawn(e, tpe, loc)

      case Expression.Lazy(exp, tpe, loc) =>
        val e = visitExp(exp)
        Expression.Lazy(e, tpe, loc)

      case Expression.Force(exp, tpe, loc) =>
        val e = visitExp(exp)
        Expression.Force(e, tpe, loc)

      case Expression.HoleError(sym, tpe, loc) => e

      case Expression.MatchError(tpe, loc) => e
    }

    visitExp(e0)
  }

  /**
    * Applies the given substitution map `subst` to the given formal parameters `fs`.
    */
  private def replace(fparam: FormalParam, subst: Map[Symbol.VarSym, Symbol.VarSym]): FormalParam = fparam match {
    case FormalParam(sym, mod, tpe, loc) =>
      subst.get(sym) match {
        case None => FormalParam(sym, mod, tpe, loc)
        case Some(newSym) => FormalParam(newSym, mod, tpe, loc)
      }
  }

}
