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
import ca.uwaterloo.flix.language.ast.{Ast, SourceLocation, Symbol, Type}
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.{InternalCompilerException, Validation}

import scala.collection.immutable.SortedSet
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
    case Expression.Cst(_, _, _) => exp0

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
      Expression.Closure(sym, tpe, loc)

    case Expression.Lambda(fparams, exp, tpe, loc) =>
      //
      // Main case: Convert a lambda expression to a lambda closure.
      //
      mkLambdaClosure(fparams, exp, tpe, loc)

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

    case Expression.Region(tpe, loc) =>
      Expression.Region(tpe, loc)

    case Expression.Scope(sym, e, tpe, purity, loc) =>
      Expression.Scope(sym, visitExp(e), tpe, purity, loc)

    case Expression.Is(sym, e, purity, loc) =>
      Expression.Is(sym, visitExp(e), purity, loc)

    case Expression.Tag(enum, e, tpe, purity, loc) =>
      Expression.Tag(enum, visitExp(e), tpe, purity, loc)

    case Expression.Untag(sym, e, tpe, purity, loc) =>
      Expression.Untag(sym, visitExp(e), tpe, purity, loc)

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

    case Expression.NewObject(name, clazz, tpe, purity, methods0, loc) =>
      val methods = methods0 map {
        case JvmMethod(ident, fparams, exp, retTpe, purity, loc) =>
          val cloType = Type.mkImpureUncurriedArrow(fparams.map(_.tpe), retTpe, loc)
          val clo = mkLambdaClosure(fparams, exp, cloType, loc)
          JvmMethod(ident, fparams, clo, retTpe, purity, loc)
      }
      Expression.NewObject(name, clazz, tpe, purity, methods, loc)

    case Expression.Spawn(exp1, exp2, tpe, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      Expression.Spawn(e1, e2, tpe, loc)

    case Expression.Lazy(exp, tpe, loc) =>
      val e = visitExp(exp)
      Expression.Lazy(e, tpe, loc)

    case Expression.Force(exp, tpe, loc) =>
      val e = visitExp(exp)
      Expression.Force(e, tpe, loc)

    case Expression.HoleError(_, _, _) => exp0

    case Expression.MatchError(_, _) => exp0

    case Expression.Closure(_, _, loc) => throw InternalCompilerException(s"Unexpected expression: '$exp0'.", loc)

    case Expression.LambdaClosure(_, _, _, _, loc) => throw InternalCompilerException(s"Unexpected expression: '$exp0'.", loc)

    case Expression.ApplyClo(_, _, _, _, loc) => throw InternalCompilerException(s"Unexpected expression: '$exp0'.", loc)

    case Expression.ApplyDef(_, _, _, _, loc) => throw InternalCompilerException(s"Unexpected expression: '$exp0'.", loc)
  }

  /**
    * Returns a LambdaClosure under the given formal parameters fparams for the body expression exp where the overall lambda has type tpe.
    */
  private def mkLambdaClosure(fparams: List[FormalParam], exp: Expression, tpe: Type, loc: SourceLocation)(implicit flix: Flix): Expression.LambdaClosure = {
    // Step 1: Compute the free variables in the lambda expression.
    //         (Remove the variables bound by the lambda itself).
    val fvs = filterBoundParams(freeVars(exp), fparams).toList

    // Step 2: Convert the free variables into a new parameter list and substitution.
    val (extraParams, subst) = getFormalParamsAndSubst(fvs, loc)
    val newParams = extraParams ++ fparams

    // Step 3: Replace every old symbol by its new symbol in the body of the lambda.
    val newBody = visitExp(applySubst(exp, subst))

    // Step 4: Put everything back together.
    Expression.LambdaClosure(newParams, fvs, newBody, tpe, loc)
  }

  /**
    * Returns a pair of a formal parameter list and a substitution
    */
  private def getFormalParamsAndSubst(fvs: List[FreeVar], loc: SourceLocation)(implicit flix: Flix): (List[FormalParam], Map[Symbol.VarSym, Symbol.VarSym]) = {
    val subst = mutable.Map.empty[Symbol.VarSym, Symbol.VarSym]
    val fparams = fvs.map {
      case FreeVar(oldSym, ptpe) =>
        val newSym = Symbol.freshVarSym(oldSym)
        subst += (oldSym -> newSym)
        FormalParam(newSym, Ast.Modifiers.Empty, ptpe, loc)
    }
    (fparams, subst.toMap)
  }

  /**
    * Returns all free variables in the given expression `exp0`.
    *
    * Note: The result:
    *   - (A) must be a set to avoid duplicates, and
    *   - (B) must be sorted to ensure deterministic compilation.
    */
  private def freeVars(exp0: Expression): SortedSet[FreeVar] = exp0 match {
    case Expression.Cst(_, _, _) => SortedSet.empty

    case Expression.Var(sym, tpe, _) => SortedSet(FreeVar(sym, tpe))

    case Expression.Def(_, _, _) => SortedSet.empty

    case Expression.Lambda(args, body, _, _) =>
      filterBoundParams(freeVars(body), args)

    case Expression.Apply(exp, args, _, _, _) =>
      freeVars(exp) ++ freeVarsExps(args)

    case Expression.Unary(_, _, exp, _, _, _) => freeVars(exp)

    case Expression.Binary(_, _, exp1, exp2, _, _, _) =>
      freeVars(exp1) ++ freeVars(exp2)

    case Expression.IfThenElse(exp1, exp2, exp3, _, _, _) =>
      freeVars(exp1) ++ freeVars(exp2) ++ freeVars(exp3)

    case Expression.Branch(exp, branches, _, _, _) =>
      freeVars(exp) ++ (branches flatMap {
        case (_, br) => freeVars(br)
      })

    case Expression.JumpTo(_, _, _, _) => SortedSet.empty

    case Expression.Let(sym, exp1, exp2, _, _, _) =>
      filterBoundVar(freeVars(exp1) ++ freeVars(exp2), sym)

    case Expression.LetRec(sym, exp1, exp2, _, _, _) =>
      filterBoundVar(freeVars(exp1) ++ freeVars(exp2), sym)

    case Expression.Region(tpe, loc) =>
      SortedSet.empty

    case Expression.Scope(sym, exp, _, _, _) => filterBoundVar(freeVars(exp), sym)

    case Expression.Is(_, exp, _, _) => freeVars(exp)

    case Expression.Untag(_, exp, _, _, _) => freeVars(exp)

    case Expression.Tag(_, exp, _, _, _) => freeVars(exp)

    case Expression.Index(base, _, _, _, _) => freeVars(base)

    case Expression.Tuple(exps, _, _, _) => freeVarsExps(exps)

    case Expression.RecordEmpty(_, _) => SortedSet.empty

    case Expression.RecordSelect(exp, _, _, _, _) => freeVars(exp)

    case Expression.RecordExtend(_, value, rest, _, _, _) => freeVars(value) ++ freeVars(rest)

    case Expression.RecordRestrict(_, rest, _, _, _) => freeVars(rest)

    case Expression.ArrayLit(exps, _, _) => freeVarsExps(exps)

    case Expression.ArrayNew(elm, len, _, _) => freeVars(elm) ++ freeVars(len)

    case Expression.ArrayLoad(base, index, _, _) => freeVars(base) ++ freeVars(index)

    case Expression.ArrayStore(base, index, elm, _, _) => freeVars(base) ++ freeVars(index) ++ freeVars(elm)

    case Expression.ArrayLength(base, _, _, _) => freeVars(base)

    case Expression.ArraySlice(base, beginIndex, endIndex, _, _) => freeVars(base) ++ freeVars(beginIndex) ++ freeVars(endIndex)

    case Expression.Ref(exp, _, _) => freeVars(exp)

    case Expression.Deref(exp, _, _) => freeVars(exp)

    case Expression.Assign(exp1, exp2, _, _) => freeVars(exp1) ++ freeVars(exp2)

    case Expression.Cast(exp, _, _, _) => freeVars(exp)

    case Expression.TryCatch(exp, rules, _, _, _) => rules.foldLeft(freeVars(exp)) {
      case (acc, CatchRule(sym, _, exp)) =>
        acc ++ filterBoundVar(freeVars(exp), sym)
    }

    case Expression.InvokeConstructor(_, args, _, _, _) => freeVarsExps(args)

    case Expression.InvokeMethod(_, exp, args, _, _, _) => freeVars(exp) ++ freeVarsExps(args)

    case Expression.InvokeStaticMethod(_, args, _, _, _) => freeVarsExps(args)

    case Expression.GetField(_, exp, _, _, _) => freeVars(exp)

    case Expression.PutField(_, exp1, exp2, _, _, _) => freeVars(exp1) ++ freeVars(exp2)

    case Expression.GetStaticField(_, _, _, _) => SortedSet.empty

    case Expression.PutStaticField(_, exp, _, _, _) => freeVars(exp)

    case Expression.NewObject(_, _, _, _, methods, _) =>
      methods.foldLeft(SortedSet.empty[FreeVar]) {
        case (acc, JvmMethod(_, fparams, exp, _, _, _)) =>
          acc ++ filterBoundParams(freeVars(exp), fparams)
      }

    case Expression.Spawn(exp1, exp2, _, _) => freeVars(exp1) ++ freeVars(exp2)

    case Expression.Lazy(exp, _, _) => freeVars(exp)

    case Expression.Force(exp, _, _) => freeVars(exp)

    case Expression.HoleError(_, _, _) => SortedSet.empty

    case Expression.MatchError(_, _) => SortedSet.empty

    case Expression.LambdaClosure(_, _, _, _, loc) => throw InternalCompilerException(s"Unexpected expression: '$exp0'.", loc)

    case Expression.Closure(_, _, loc) => throw InternalCompilerException(s"Unexpected expression: '$exp0'.", loc)

    case Expression.ApplyClo(_, _, _, _, loc) => throw InternalCompilerException(s"Unexpected expression: '$exp0'.", loc)

    case Expression.ApplyDef(_, _, _, _, loc) => throw InternalCompilerException(s"Unexpected expression: '$exp0'.", loc)
  }

  /**
    * Returns the free variables in `exps0`.
    */
  private def freeVarsExps(exps0: List[Expression]): SortedSet[FreeVar] =
    exps0.foldLeft(SortedSet.empty[FreeVar]) {
      case (acc, exp) => acc ++ freeVars(exp)
    }

  /**
    * Returns `fvs` without the variable symbol `bound`.
    */
  private def filterBoundVar(fvs: SortedSet[FreeVar], bound: Symbol.VarSym): SortedSet[FreeVar] =
    fvs.filter {
      case FreeVar(sym, _) => sym != bound
    }

  /**
    * Returns `fvs` without all the variable symbols in the formal parameters `bound`.
    */
  private def filterBoundParams(fvs: SortedSet[FreeVar], bound: List[FormalParam]): SortedSet[FreeVar] =
    fvs.filter {
      case FreeVar(sym, _) => !bound.exists(fparam => sym == fparam.sym)
    }

  /**
    * Applies the given substitution map `subst` to the given expression `e`.
    */
  private def applySubst(e0: Expression, subst: Map[Symbol.VarSym, Symbol.VarSym])(implicit flix: Flix): Expression = {

    def visitExp(e: Expression): Expression = e match {
      case Expression.Cst(_, _, _) => e

      case Expression.Var(sym, tpe, loc) => subst.get(sym) match {
        case None => Expression.Var(sym, tpe, loc)
        case Some(newSym) => Expression.Var(newSym, tpe, loc)
      }

      case Expression.Def(_, _, _) => e

      case Expression.Lambda(fparams, exp, tpe, loc) =>
        val fs = fparams.map(fparam => visitFormalParam(fparam, subst))
        val e = visitExp(exp)
        Expression.Lambda(fs, e, tpe, loc)

      case Expression.Closure(_, _, _) => e

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

      case Expression.Region(tpe, loc) =>
        Expression.Region(tpe, loc)

      case Expression.Scope(sym, exp, tpe, purity, loc) =>
        val newSym = subst.getOrElse(sym, sym)
        val e = visitExp(exp)
        Expression.Scope(newSym, e, tpe, purity, loc)

      case Expression.Is(sym, exp, purity, loc) =>
        val e = visitExp(exp)
        Expression.Is(sym, e, purity, loc)

      case Expression.Untag(sym, exp, tpe, purity, loc) =>
        val e = visitExp(exp)
        Expression.Untag(sym, e, tpe, purity, loc)

      case Expression.Tag(enum, exp, tpe, purity, loc) =>
        val e = visitExp(exp)
        Expression.Tag(enum, e, tpe, purity, loc)

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

      case Expression.NewObject(name, clazz, tpe, purity, methods0, loc) =>
        val methods = methods0.map(visitJvmMethod(_, subst))
        Expression.NewObject(name, clazz, tpe, purity, methods, loc)

      case Expression.Spawn(exp1, exp2, tpe, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        Expression.Spawn(e1, e2, tpe, loc)

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
  private def visitFormalParam(fparam: FormalParam, subst: Map[Symbol.VarSym, Symbol.VarSym]): FormalParam = fparam match {
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
  private def visitJvmMethod(method: JvmMethod, subst: Map[Symbol.VarSym, Symbol.VarSym])(implicit flix: Flix): JvmMethod = method match {
    case JvmMethod(ident, fparams0, exp, retTpe, purity, loc) =>
      val fparams = fparams0.map(visitFormalParam(_, subst))
      JvmMethod(ident, fparams, applySubst(exp, subst), retTpe, purity, loc)
  }

}
