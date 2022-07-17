/*
 * Copyright 2015-2016, 2022 Ming-Ho Yee, Magnus Madsen
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
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.SimplifiedAst._
import ca.uwaterloo.flix.language.ast.{Ast, Symbol, Type}
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.{InternalCompilerException, Validation}

import scala.collection.mutable

object ClosureConv {

  /**
    * Performs closure conversion on the given AST `root`.
    */
  def run(root: Root)(implicit flix: Flix): Validation[Root, CompilationMessage] = flix.phase("ClosureConv") {
    val newDefs = root.defs.map {
      case (sym, decl) => sym -> visitDef(decl)
    }

    root.copy(defs = newDefs).toSuccess
  }

  /**
    * Performs closure conversion on the given definition `def0`.
    */
  private def visitDef(def0: Def)(implicit flix: Flix): Def = {
    def0.copy(exp = visitExp(def0.exp))
  }

  /**
    * Performs closure conversion on the given expression `exp0`.
    */
  private def visitExp(exp0: Expression)(implicit flix: Flix): Expression = exp0 match {
    case Expression.Unit(_) => exp0

    case Expression.Null(_, _) => exp0

    case Expression.True(_) => exp0

    case Expression.False(_) => exp0

    case Expression.Char(_, _) => exp0

    case Expression.Float32(_, _) => exp0

    case Expression.Float64(_, _) => exp0

    case Expression.Int8(_, _) => exp0

    case Expression.Int16(_, _) => exp0

    case Expression.Int32(_, _) => exp0

    case Expression.Int64(_, _) => exp0

    case Expression.BigInt(_, _) => exp0

    case Expression.Str(_, _) => exp0

    case Expression.Var(_, _, _) => exp0

    case Expression.Def(sym, tpe, loc) =>
      //
      // Special Case: A def expression occurs outside of an `Apply` expression.
      //

      //
      // We must create a closure that references the definition symbol.
      //
      // The closure has no free variables since it is a reference to a top-level function.
      //
      // This case happens if the programmers writes e.g.:
      //
      // let m = List.map; ...
      //
      Expression.Closure(sym, List.empty, tpe, loc)

    case Expression.Lambda(fparams, exp, tpe, loc) =>
      //
      // Main case: Convert a lambda expression to a lambda closure.
      //

      // Step 1: Compute the free variables in the lambda expression.
      // Note: We must remove the formal parameters (which are obviously bound in the body).
      val boundSyms = fparams.map(_.sym)
      val fvs = freeVars(exp).toList.filter(kv => !boundSyms.contains(kv._1))

      // Step 2: We prepend the free variables to the formal parameter list.
      // Thus all variables within the lambda body will be treated uniformly.
      // The implementation will supply values for the free variables, without any effort from the caller.
      // We introduce new symbols for each introduced parameter and replace their occurrence in the body.
      val subst = mutable.Map.empty[Symbol.VarSym, Symbol.VarSym]
      val extFormalParams = fvs.map {
        case (oldSym, ptpe) =>
          val newSym = Symbol.freshVarSym(oldSym)
          subst += (oldSym -> newSym)
          FormalParam(newSym, Ast.Modifiers.Empty, ptpe, loc)
      } ++ fparams

      // Step 3: Replace every old symbol by its new symbol in the body expression.
      val newBody = visitExp(replace(exp, subst.toMap))

      // At this point all free variables have been converted to new arguments, prepended to the original
      // arguments list. Additionally, any lambdas within the body have also been closure converted.

      // The closure will actually be created at run time, where the values for the free variables are bound
      // and stored in the closure structure. When the closure is called, the bound values are passed as arguments.
      // In a later phase, we will lift the lambda to a top-level definition.
      Expression.LambdaClosure(extFormalParams, fvs.map(v => FreeVar(v._1, v._2)), newBody, tpe, loc)

    case Expression.Apply(exp, exps, tpe, purity, loc) => exp match {
      case Expression.Def(sym, _, _) =>
        //
        // Special Case: Direct call to a known function symbol.
        //
        val es = exps.map(visitExp)
        Expression.ApplyDef(sym, es, tpe, purity, loc)
      case _ =>
        //
        // General Case: Call to closure.
        //
        val e = visitExp(exp)
        val es = exps.map(visitExp)
        Expression.ApplyClo(e, es, tpe, purity, loc)
    }

    case Expression.Unary(sop, op, exp, tpe, purity, loc) =>
      val e = visitExp(exp)
      Expression.Unary(sop, op, e, tpe, purity, loc)

    case Expression.Binary(sop, op, exp1, exp2, tpe, purity, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      Expression.Binary(sop, op, e1, e2, tpe, purity, loc)

    case Expression.IfThenElse(exp1, exp2, exp3, tpe, purity, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val e3 = visitExp(exp3)
      Expression.IfThenElse(e1, e2, e3, tpe, purity, loc)

    case Expression.Branch(exp, branches, tpe, purity, loc) =>
      val e = visitExp(exp)
      val bs = branches map {
        case (sym, br) => sym -> visitExp(br)
      }
      Expression.Branch(e, bs, tpe, purity, loc)

    case Expression.JumpTo(sym, tpe, purity, loc) =>
      Expression.JumpTo(sym, tpe, purity, loc)

    case Expression.Let(sym, e1, e2, tpe, purity, loc) =>
      Expression.Let(sym, visitExp(e1), visitExp(e2), tpe, purity, loc)

    case Expression.LetRec(sym, e1, e2, tpe, purity, loc) =>
      Expression.LetRec(sym, visitExp(e1), visitExp(e2), tpe, purity, loc)

    case Expression.Is(sym, tag, e, purity, loc) =>
      Expression.Is(sym, tag, visitExp(e), purity, loc)

    case Expression.Tag(enum, tag, e, tpe, purity, loc) =>
      Expression.Tag(enum, tag, visitExp(e), tpe, purity, loc)

    case Expression.Untag(sym, tag, e, tpe, purity, loc) =>
      Expression.Untag(sym, tag, visitExp(e), tpe, purity, loc)

    case Expression.Index(e, offset, tpe, purity, loc) =>
      Expression.Index(visitExp(e), offset, tpe, purity, loc)

    case Expression.Tuple(elms, tpe, purity, loc) =>
      Expression.Tuple(elms.map(visitExp), tpe, purity, loc)

    case Expression.RecordEmpty(tpe, loc) =>
      Expression.RecordEmpty(tpe, loc)

    case Expression.RecordSelect(exp, field, tpe, purity, loc) =>
      val e = visitExp(exp)
      Expression.RecordSelect(e, field, tpe, purity, loc)

    case Expression.RecordExtend(field, value, rest, tpe, purity, loc) =>
      val v = visitExp(value)
      val r = visitExp(rest)
      Expression.RecordExtend(field, v, r, tpe, purity, loc)

    case Expression.RecordRestrict(field, rest, tpe, purity, loc) =>
      val r = visitExp(rest)
      Expression.RecordRestrict(field, r, tpe, purity, loc)

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

    case Expression.ArrayLength(exp, tpe, _, loc) =>
      val b = visitExp(exp)
      val purity = b.purity
      Expression.ArrayLength(b, tpe, purity, loc)

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

    case Expression.Cast(exp, tpe, purity, loc) =>
      val e = visitExp(exp)
      Expression.Cast(e, tpe, purity, loc)

    case Expression.TryCatch(exp, rules, tpe, purity, loc) =>
      val e = visitExp(exp)
      val rs = rules map {
        case CatchRule(sym, clazz, body) =>
          val b = visitExp(body)
          CatchRule(sym, clazz, b)
      }
      Expression.TryCatch(e, rs, tpe, purity, loc)

    case Expression.InvokeConstructor(constructor, args, tpe, purity, loc) =>
      val as = args map visitExp
      Expression.InvokeConstructor(constructor, as, tpe, purity, loc)

    case Expression.InvokeMethod(method, exp, args, tpe, purity, loc) =>
      val e = visitExp(exp)
      val as = args.map(visitExp)
      Expression.InvokeMethod(method, e, as, tpe, purity, loc)

    case Expression.InvokeStaticMethod(method, args, tpe, purity, loc) =>
      val as = args.map(visitExp)
      Expression.InvokeStaticMethod(method, as, tpe, purity, loc)

    case Expression.GetField(field, exp, tpe, purity, loc) =>
      val e = visitExp(exp)
      Expression.GetField(field, e, tpe, purity, loc)

    case Expression.PutField(field, exp1, exp2, tpe, purity, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      Expression.PutField(field, e1, e2, tpe, purity, loc)

    case Expression.GetStaticField(field, tpe, purity, loc) =>
      Expression.GetStaticField(field, tpe, purity, loc)

    case Expression.PutStaticField(field, exp, tpe, purity, loc) =>
      val e = visitExp(exp)
      Expression.PutStaticField(field, e, tpe, purity, loc)

    case Expression.NewObject(clazz, tpe, purity, methods0, loc) =>
      val methods = methods0 map {
        case JvmMethod(ident, fparams, exp, retTpe, purity, loc) =>
          JvmMethod(ident, fparams, visitExp(exp), retTpe, purity, loc)
      }
      Expression.NewObject(clazz, tpe, purity, methods, loc)

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

    case Expression.HoleError(_, _, _) => exp0

    case Expression.MatchError(_, _) => exp0

    case Expression.Closure(_, _, _, _) => throw InternalCompilerException(s"Unexpected expression: '$exp0'.")

    case Expression.LambdaClosure(_, _, _, _, _) => throw InternalCompilerException(s"Unexpected expression: '$exp0'.")

    case Expression.ApplyClo(_, _, _, _, _) => throw InternalCompilerException(s"Unexpected expression: '$exp0'.")

    case Expression.ApplyDef(_, _, _, _, _) => throw InternalCompilerException(s"Unexpected expression: '$exp0'.")
  }

  /**
    * Returns the free variables in the given expression `exp`.
    * Does a left-to-right traversal of the AST, collecting free variables in order, in a LinkedHashSet.
    */
  // TODO: Refactor to return a normal list of SimplifiedAst.FreeVar.
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

    case Expression.Var(sym, tpe, _) => mutable.LinkedHashSet((sym, tpe))

    case Expression.Def(_, _, _) => mutable.LinkedHashSet.empty

    case Expression.Lambda(args, body, _, _) =>
      val bound = args.map(_.sym)
      freeVars(body).filterNot { v => bound.contains(v._1) }

    case Expression.Apply(exp, args, _, _, _) =>
      freeVars(exp) ++ args.flatMap(freeVars)
    case Expression.Unary(_, _, exp, _, _, _) => freeVars(exp)
    case Expression.Binary(_, _, exp1, exp2, _, _, _) =>
      freeVars(exp1) ++ freeVars(exp2)
    case Expression.IfThenElse(exp1, exp2, exp3, _, _, _) =>
      freeVars(exp1) ++ freeVars(exp2) ++ freeVars(exp3)
    case Expression.Branch(exp, branches, _, _, _) =>
      mutable.LinkedHashSet.empty ++ freeVars(exp) ++ (branches flatMap {
        case (_, br) => freeVars(br)
      })
    case Expression.JumpTo(_, _, _, _) => mutable.LinkedHashSet.empty

    case Expression.Let(sym, exp1, exp2, _, _, _) =>
      val bound = sym
      freeVars(exp1) ++ freeVars(exp2).filterNot { v => bound == v._1 }

    case Expression.LetRec(sym, exp1, exp2, _, _, _) =>
      val bound = sym
      (freeVars(exp1) ++ freeVars(exp2)).filterNot { v => bound == v._1 }

    case Expression.Is(_, _, exp, _, _) => freeVars(exp)
    case Expression.Untag(_, _, exp, _, _, _) => freeVars(exp)
    case Expression.Tag(_, _, exp, _, _, _) => freeVars(exp)
    case Expression.Index(base, _, _, _, _) => freeVars(base)
    case Expression.Tuple(elms, _, _, _) => mutable.LinkedHashSet.empty ++ elms.flatMap(freeVars)
    case Expression.RecordEmpty(_, _) => mutable.LinkedHashSet.empty
    case Expression.RecordSelect(exp, _, _, _, _) => freeVars(exp)
    case Expression.RecordExtend(_, value, rest, _, _, _) => freeVars(value) ++ freeVars(rest)
    case Expression.RecordRestrict(_, rest, _, _, _) => freeVars(rest)
    case Expression.ArrayLit(elms, _, _) => mutable.LinkedHashSet.empty ++ elms.flatMap(freeVars)
    case Expression.ArrayNew(elm, len, _, _) => freeVars(elm) ++ freeVars(len)
    case Expression.ArrayLoad(base, index, _, _) => freeVars(base) ++ freeVars(index)
    case Expression.ArrayStore(base, index, elm, _, _) => freeVars(base) ++ freeVars(index) ++ freeVars(elm)
    case Expression.ArrayLength(base, _, _, _) => freeVars(base)
    case Expression.ArraySlice(base, beginIndex, endIndex, _, _) => freeVars(base) ++ freeVars(beginIndex) ++ freeVars(endIndex)
    case Expression.Ref(exp, _, _) => freeVars(exp)
    case Expression.Deref(exp, _, _) => freeVars(exp)
    case Expression.Assign(exp1, exp2, _, _) => freeVars(exp1) ++ freeVars(exp2)

    case Expression.Cast(exp, _, _, _) => freeVars(exp)

    case Expression.TryCatch(exp, rules, _, _, _) => mutable.LinkedHashSet.empty ++ freeVars(exp) ++ rules.flatMap(r => freeVars(r.exp).filterNot(_._1 == r.sym))

    case Expression.InvokeConstructor(_, args, _, _, _) => mutable.LinkedHashSet.empty ++ args.flatMap(freeVars)

    case Expression.InvokeMethod(_, exp, args, _, _, _) => freeVars(exp) ++ args.flatMap(freeVars)

    case Expression.InvokeStaticMethod(_, args, _, _, _) => mutable.LinkedHashSet.empty ++ args.flatMap(freeVars)

    case Expression.GetField(_, exp, _, _, _) => freeVars(exp)

    case Expression.PutField(_, exp1, exp2, _, _, _) => freeVars(exp1) ++ freeVars(exp2)

    case Expression.GetStaticField(_, _, _, _) => mutable.LinkedHashSet.empty

    case Expression.PutStaticField(_, exp, _, _, _) => freeVars(exp)

    case Expression.NewObject(_, _, _, methods, _) =>
      mutable.LinkedHashSet.empty ++ methods.flatMap {
        case JvmMethod(_, fparams, exp, _, _, _) =>
          val bound = fparams.map(_.sym)
          freeVars(exp).filterNot { v => bound.contains(v._1) }
      }

    case Expression.NewChannel(exp, _, _) => freeVars(exp)

    case Expression.GetChannel(exp, _, _) => freeVars(exp)

    case Expression.PutChannel(exp1, exp2, _, _) => freeVars(exp1) ++ freeVars(exp2)

    case Expression.SelectChannel(rules, default, _, _) =>
      val rs = mutable.LinkedHashSet.empty ++ rules.flatMap {
        case SelectChannelRule(sym, chan, exp) => (freeVars(chan) ++ freeVars(exp)).filter(p => p._1 != sym)
      }

      val d = default.map(freeVars).getOrElse(mutable.LinkedHashSet.empty)

      rs ++ d

    case Expression.Spawn(exp, _, _) => freeVars(exp)

    case Expression.Lazy(exp, _, _) => freeVars(exp)

    case Expression.Force(exp, _, _) => freeVars(exp)

    case Expression.HoleError(_, _, _) => mutable.LinkedHashSet.empty
    case Expression.MatchError(_, _) => mutable.LinkedHashSet.empty

    case Expression.LambdaClosure(_, _, _, _, _) => throw InternalCompilerException(s"Unexpected expression.")
    case Expression.Closure(_, _, _, _) => throw InternalCompilerException(s"Unexpected expression.")
    case Expression.ApplyClo(_, _, _, _, _) => throw InternalCompilerException(s"Unexpected expression.")
    case Expression.ApplyDef(_, _, _, _, _) => throw InternalCompilerException(s"Unexpected expression.")
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

      case Expression.Def(_, _, _) => e

      case Expression.Lambda(fparams, exp, tpe, loc) =>
        val fs = fparams.map(fparam => replace(fparam, subst))
        val e = visitExp(exp)
        Expression.Lambda(fs, e, tpe, loc)

      case Expression.Closure(_, _, _, _) => e

      case Expression.LambdaClosure(fparams, freeVars, exp, tpe, loc) =>
        val e = visitExp(exp).asInstanceOf[Expression.Lambda]
        Expression.LambdaClosure(fparams, freeVars, e, tpe, loc)

      case Expression.ApplyClo(exp, args, tpe, purity, loc) =>
        val e = visitExp(exp)
        val as = args map visitExp
        Expression.ApplyClo(e, as, tpe, purity, loc)

      case Expression.ApplyDef(sym, args, tpe, purity, loc) =>
        val as = args map visitExp
        Expression.ApplyDef(sym, as, tpe, purity, loc)

      case Expression.Apply(exp, args, tpe, purity, loc) =>
        val e = visitExp(exp)
        val as = args map visitExp
        Expression.Apply(e, as, tpe, purity, loc)

      case Expression.Unary(sop, op, exp, tpe, purity, loc) =>
        val e = visitExp(exp)
        Expression.Unary(sop, op, e, tpe, purity, loc)

      case Expression.Binary(sop, op, exp1, exp2, tpe, purity, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        Expression.Binary(sop, op, e1, e2, tpe, purity, loc)

      case Expression.IfThenElse(exp1, exp2, exp3, tpe, purity, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        val e3 = visitExp(exp3)
        Expression.IfThenElse(e1, e2, e3, tpe, purity, loc)

      case Expression.Branch(exp, branches, tpe, purity, loc) =>
        val e = visitExp(exp)
        val bs = branches map {
          case (sym, br) => sym -> visitExp(br)
        }
        Expression.Branch(e, bs, tpe, purity, loc)

      case Expression.JumpTo(sym, tpe, purity, loc) =>
        Expression.JumpTo(sym, tpe, purity, loc)

      case Expression.Let(sym, exp1, exp2, tpe, purity, loc) =>
        val newSym = subst.getOrElse(sym, sym)
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        Expression.Let(newSym, e1, e2, tpe, purity, loc)

      case Expression.LetRec(sym, exp1, exp2, tpe, purity, loc) =>
        val newSym = subst.getOrElse(sym, sym)
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        Expression.LetRec(newSym, e1, e2, tpe, purity, loc)

      case Expression.Is(sym, tag, exp, purity, loc) =>
        val e = visitExp(exp)
        Expression.Is(sym, tag, e, purity, loc)

      case Expression.Untag(sym, tag, exp, tpe, purity, loc) =>
        val e = visitExp(exp)
        Expression.Untag(sym, tag, e, tpe, purity, loc)

      case Expression.Tag(enum, tag, exp, tpe, purity, loc) =>
        val e = visitExp(exp)
        Expression.Tag(enum, tag, e, tpe, purity, loc)

      case Expression.Index(exp, offset, tpe, purity, loc) =>
        val e = visitExp(exp)
        Expression.Index(e, offset, tpe, purity, loc)

      case Expression.Tuple(elms, tpe, purity, loc) =>
        val es = elms map visitExp
        Expression.Tuple(es, tpe, purity, loc)

      case Expression.RecordEmpty(tpe, loc) =>
        Expression.RecordEmpty(tpe, loc)

      case Expression.RecordSelect(base, field, tpe, purity, loc) =>
        val b = visitExp(base)
        Expression.RecordSelect(b, field, tpe, purity, loc)

      case Expression.RecordExtend(field, value, rest, tpe, purity, loc) =>
        val v = visitExp(value)
        val r = visitExp(rest)
        Expression.RecordExtend(field, v, r, tpe, purity, loc)

      case Expression.RecordRestrict(field, rest, tpe, purity, loc) =>
        val r = visitExp(rest)
        Expression.RecordRestrict(field, r, tpe, purity, loc)

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

      case Expression.ArrayLength(base, tpe, _, loc) =>
        val b = visitExp(base)
        val purity = b.purity
        Expression.ArrayLength(b, tpe, purity, loc)

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

      case Expression.Cast(exp, tpe, purity, loc) =>
        val e = visitExp(exp)
        Expression.Cast(e, tpe, purity, loc)

      case Expression.TryCatch(exp, rules, tpe, purity, loc) =>
        val e = visitExp(exp)
        val rs = rules map {
          case CatchRule(sym, clazz, body) =>
            val b = visitExp(body)
            CatchRule(sym, clazz, b)
        }
        Expression.TryCatch(e, rs, tpe, purity, loc)

      case Expression.InvokeConstructor(constructor, args, tpe, purity, loc) =>
        val as = args.map(visitExp)
        Expression.InvokeConstructor(constructor, as, tpe, purity, loc)

      case Expression.InvokeMethod(method, exp, args, tpe, purity, loc) =>
        val e = visitExp(exp)
        val as = args.map(visitExp)
        Expression.InvokeMethod(method, e, as, tpe, purity, loc)

      case Expression.InvokeStaticMethod(method, args, tpe, purity, loc) =>
        val as = args.map(visitExp)
        Expression.InvokeStaticMethod(method, as, tpe, purity, loc)

      case Expression.GetField(field, exp, tpe, purity, loc) =>
        val e = visitExp(exp)
        Expression.GetField(field, e, tpe, purity, loc)

      case Expression.PutField(field, exp1, exp2, tpe, purity, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        Expression.PutField(field, e1, e2, tpe, purity, loc)

      case Expression.GetStaticField(_, _, _, _) =>
        e

      case Expression.PutStaticField(field, exp, tpe, purity, loc) =>
        val e = visitExp(exp)
        Expression.PutStaticField(field, e, tpe, purity, loc)

      case Expression.NewObject(clazz, tpe, purity, methods0, loc) =>
        val methods = methods0.map(replace(_, subst))
        Expression.NewObject(clazz, tpe, purity, methods, loc)

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

      case Expression.HoleError(_, _, _) => e

      case Expression.MatchError(_, _) => e
    }

    visitExp(e0)
  }

  /**
    * Applies the given substitution map `subst` to the given formal parameters `fs`.
    */
  // TODO: Move into the above replace function and rename to visitFormalParam
  private def replace(fparam: FormalParam, subst: Map[Symbol.VarSym, Symbol.VarSym]): FormalParam = fparam match {
    case FormalParam(sym, mod, tpe, loc) =>
      subst.get(sym) match {
        case None => FormalParam(sym, mod, tpe, loc)
        case Some(newSym) => FormalParam(newSym, mod, tpe, loc)
      }
  }

  /**
    * Applies the given substitution map `subst` to the given JvmMethod `method`.
    */
  // TODO: Move into the above replace function and rename to visitJvmMethod.
  private def replace(method: JvmMethod, subst: Map[Symbol.VarSym, Symbol.VarSym])(implicit flix: Flix): JvmMethod = method match {
    case JvmMethod(ident, fparams0, exp, retTpe, purity, loc) =>
      val fparams = fparams0.map(replace(_, subst))
      JvmMethod(ident, fparams, replace(exp, subst), retTpe, purity, loc)
  }

}
