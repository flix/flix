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
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.util.{ParOps, Validation}
import ca.uwaterloo.flix.util.Validation._

/**
  * The Tree Shaking phase removes all unused function definitions.
  *
  * A function is considered reachable if it:
  *
  * (a) The main function is always reachable.
  * (b) A function marked with @benchmark or @test is reachable.
  * (c) Appears in a function which itself is reachable.
  *
  */
object EarlyTreeShaker {

  /**
    * Performs tree shaking on the given AST `root`.
    */
  def run(root: Root)(implicit flix: Flix): Validation[Root, CompilationMessage] = flix.phase("TreeShaker") {
    // Compute the symbols that are always reachable.
    val initReach = initReachable(root)

    // Compute the symbols that are transitively reachable.
    val allReachable = ParOps.parReachable(initReach, visitSym(_, root))

    // Filter the reachable definitions.
    val newDefs = root.defs.filter {
      case (sym, _) => allReachable.contains(sym)
    }

    // Reassemble the AST.
    root.copy(defs = newDefs).toSuccess
  }

  /**
    * Returns the symbols that are always reachable.
    */
  private def initReachable(root: Root): Set[Symbol.DefnSym] = {
    // A set used to collect the symbols of reachable functions.
    var reachable: Set[Symbol.DefnSym] = root.reachable

    //
    // (a) The main function is always reachable (if it exists).
    //
    reachable = reachable ++ root.entryPoint.toList

    //
    // (b) A function annotated with @benchmark or @test is always reachable.
    //
    /*
    for ((sym, defn) <- root.defs) {
      val isBenchmark = defn.ann.isBenchmark
      val isTest = defn.ann.isTest
      if (isBenchmark || isTest) {
        reachable = reachable + sym
      }
    }
  */
    reachable
  }

  /**
    * Returns the symbols reachable from the given symbol `sym`.
    */
  private def visitSym(sym: Symbol.DefnSym, root: Root): Set[Symbol.DefnSym] = root.defs.get(sym) match {
    case None => Set.empty
    case Some(defn) => visitExp(defn.exp)
  }

  /**
    * Returns the function symbols reachable from the given expression `e0`.
    */
  private def visitExp(e0: Expression): Set[Symbol.DefnSym] = e0 match {
    case Expression.Unit(_) =>
      Set.empty

    case Expression.Null(_, _) =>
      Set.empty

    case Expression.True(_) =>
      Set.empty

    case Expression.False(_) =>
      Set.empty

    case Expression.Char(_, _) =>
      Set.empty

    case Expression.Float32(_, _) =>
      Set.empty

    case Expression.Float64(_, _) =>
      Set.empty

    case Expression.Int8(_, _) =>
      Set.empty

    case Expression.Int16(_, _) =>
      Set.empty

    case Expression.Int32(_, _) =>
      Set.empty

    case Expression.Int64(_, _) =>
      Set.empty

    case Expression.BigInt(_, _) =>
      Set.empty

    case Expression.Str(_, _) =>
      Set.empty

    case Expression.Default(_, _) =>
      Set.empty

    case Expression.Wild(_, _) =>
      Set.empty

    case Expression.Var(_, _, _) =>
      Set.empty

    case Expression.Def(sym, _, _) =>
      Set(sym)

    case Expression.Sig(sym, _, _) =>
      Set(sym)

    case Expression.Hole(sym, _, _) =>
      Set(sym)

    case Expression.Lambda(fparam, exp, _, _) =>
      Set(fparam.sym) ++ visitExp(exp) // What to do with the symbols here?

    case Expression.Apply(exp, exps, tpe, pur, loc) => ???
    case Expression.Unary(sop, exp, tpe, pur, loc) => ???
    case Expression.Binary(sop, exp1, exp2, tpe, pur, loc) => ???
    case Expression.Let(sym, mod, exp1, exp2, tpe, pur, loc) => ???
    case Expression.LetRec(sym, mod, exp1, exp2, tpe, pur, loc) => ???
    case Expression.Region(tpe, loc) => ???
    case Expression.Scope(sym, regionVar, exp, tpe, pur, loc) => ???
    case Expression.IfThenElse(exp1, exp2, exp3, tpe, pur, loc) => ???
    case Expression.Stm(exp1, exp2, tpe, pur, loc) => ???
    case Expression.Discard(exp, pur, loc) => ???
    case Expression.Match(exp, rules, tpe, pur, loc) => ???
    case Expression.Choose(exps, rules, tpe, pur, loc) => ???
    case Expression.Tag(sym, tag, exp, tpe, pur, loc) => ???
    case Expression.Tuple(elms, tpe, pur, loc) => ???
    case Expression.RecordEmpty(tpe, loc) => ???
    case Expression.RecordSelect(exp, field, tpe, pur, loc) => ???
    case Expression.RecordExtend(field, value, rest, tpe, pur, loc) => ???
    case Expression.RecordRestrict(field, rest, tpe, pur, loc) => ???
    case Expression.ArrayLit(exps, exp, tpe, pur, loc) => ???
    case Expression.ArrayNew(exp1, exp2, exp3, tpe, pur, loc) => ???
    case Expression.ArrayLoad(base, index, tpe, pur, loc) => ???
    case Expression.ArrayLength(base, pur, loc) => ???
    case Expression.ArrayStore(base, index, elm, loc) => ???
    case Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) => ???
    case Expression.Ref(exp1, exp2, tpe, pur, loc) => ???
    case Expression.Deref(exp, tpe, pur, loc) => ???
    case Expression.Assign(exp1, exp2, tpe, pur, loc) => ???
    case Expression.Ascribe(exp, tpe, pur, loc) => ???
    case Expression.Cast(exp, declaredType, declaredEff, tpe, pur, loc) => ???
    case Expression.TryCatch(exp, rules, tpe, pur, loc) => ???
    case Expression.InvokeConstructor(constructor, args, tpe, pur, loc) => ???
    case Expression.InvokeMethod(method, exp, args, tpe, pur, loc) => ???
    case Expression.InvokeStaticMethod(method, args, tpe, pur, loc) => ???
    case Expression.GetField(field, exp, tpe, pur, loc) => ???
    case Expression.PutField(field, exp1, exp2, tpe, pur, loc) => ???
    case Expression.GetStaticField(field, tpe, pur, loc) => ???
    case Expression.PutStaticField(field, exp, tpe, pur, loc) => ???
    case Expression.NewObject(clazz, tpe, pur, loc) => ???
    case Expression.NewChannel(exp, tpe, pur, loc) => ???
    case Expression.GetChannel(exp, tpe, pur, loc) => ???
    case Expression.PutChannel(exp1, exp2, tpe, pur, loc) => ???
    case Expression.SelectChannel(rules, default, tpe, pur, loc) => ???
    case Expression.Spawn(exp, tpe, pur, loc) => ???
    case Expression.Lazy(exp, tpe, loc) => ???
    case Expression.Force(exp, tpe, pur, loc) => ???
    case Expression.FixpointConstraintSet(cs, stf, tpe, loc) => ???
    case Expression.FixpointLambda(pparams, exp, stf, tpe, pur, loc) => ???
    case Expression.FixpointMerge(exp1, exp2, stf, tpe, pur, loc) => ???
    case Expression.FixpointSolve(exp, stf, tpe, pur, loc) => ???
    case Expression.FixpointFilter(pred, exp, tpe, pur, loc) => ???
    case Expression.FixpointInject(exp, pred, tpe, pur, loc) => ???
    case Expression.FixpointProject(pred, exp, tpe, pur, loc) => ???
    case Expression.Reify(t, tpe, pur, loc) => ???
    case Expression.ReifyType(t, k, tpe, pur, loc) => ???
    case Expression.ReifyEff(sym, exp1, exp2, exp3, tpe, pur, loc) => ???
  }

  /**
    * Returns the function symbols reachable from `es`.
    */
  private def visitExps(es: List[Expression]): Set[Symbol.DefnSym] = es.map(visitExp).fold(Set())(_ ++ _)

}
