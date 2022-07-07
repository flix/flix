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
import ca.uwaterloo.flix.language.ast.Ast.Annotation
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.{Body, Head}
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast.{Symbol, TypedAst}
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.{ParOps, Validation}

import scala.annotation.tailrec

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
    for ((sym, defn) <- root.defs) {
      if (isBenchmark(defn.spec.ann) || isTest(defn.spec.ann)) {
        reachable = reachable + sym
      }
    }
    reachable
  }

  private def isBenchmark(l: List[TypedAst.Annotation]): Boolean = l.exists { a =>
    a.name match {
      case Annotation.Benchmark(_) => true
      case _ => false
    }
  }

  private def isTest(l: List[TypedAst.Annotation]): Boolean = l.exists { a =>
    a.name match {
      case Annotation.Test(_) => true
      case _ => false
    }
  }

  /**
    * Returns the symbols reachable from the given symbol `sym`.
    */
  private def visitSym(sym: Symbol.DefnSym, root: Root): Set[Symbol.DefnSym] = root.defs.get(sym) match {
    case None => Set.empty
    case Some(defn) => visitExp(defn.impl.exp)
  }

  private def visitMatchRules(rules: List[MatchRule]): Set[Symbol.DefnSym] = {
    @tailrec
    def loop(r: List[MatchRule], acc: Set[Symbol.DefnSym]): Set[Symbol.DefnSym] = r match {
      case Nil => acc
      case x :: xs => loop(xs, visitExp(x.exp) ++ visitExp(x.guard) ++ acc)
    }

    loop(rules, Set.empty)
  }

  private def visitChoiceRules(rules: List[ChoiceRule]): Set[Symbol.DefnSym] = {
    @tailrec
    def loop(r: List[ChoiceRule], acc: Set[Symbol.DefnSym]): Set[Symbol.DefnSym] = r match {
      case Nil => acc
      case x :: xs => loop(xs, visitExp(x.exp) ++ acc)
    }

    loop(rules, Set.empty)
  }

  def visitCatchRules(rules: List[CatchRule]): Set[Symbol.DefnSym] = {
    @tailrec
    def loop(r: List[CatchRule], acc: Set[Symbol.DefnSym]): Set[Symbol.DefnSym] = r match {
      case Nil => acc
      case x :: xs => loop(xs, visitExp(x.exp) ++ acc)
    }

    loop(rules, Set.empty)
  }

  def visitChannelRules(rules: List[SelectChannelRule]): Set[Symbol.DefnSym] = {
    @tailrec
    def loop(r: List[SelectChannelRule], acc: Set[Symbol.DefnSym]): Set[Symbol.DefnSym] = r match {
      case Nil => acc
      case x :: xs => loop(xs, visitExp(x.chan) ++ visitExp(x.exp) ++ acc)
    }

    loop(rules, Set.empty)
  }

  def visitConstraints(constraints: List[Constraint]): Set[Symbol.DefnSym] = {
    def visitHead(h: Head): Set[Symbol.DefnSym] = h match {
      case Head.Atom(_, _, terms, _, _) => visitExps(terms)
    }

    def visitBody(b: List[Body]): Set[Symbol.DefnSym] = {
      b.foldLeft(Set.empty: Set[Symbol.DefnSym]) {
        case (acc, Body.Guard(exp, _)) => visitExp(exp) ++ acc
        case (acc, Body.Loop(_, exp, _)) => visitExp(exp) ++ acc
        case (acc, _) => acc
      }
    }

    @tailrec
    def loop(cs: List[Constraint], acc: Set[Symbol.DefnSym]): Set[Symbol.DefnSym] = cs match {
      case Nil => acc
      case x :: xs => loop(xs, visitHead(x.head) ++ visitBody(x.body) ++ acc)
    }

    loop(constraints, Set.empty)
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
      Set.empty

    case Expression.Hole(sym, _, _) =>
      Set.empty

    case Expression.Lambda(fparam, exp, _, _) =>
      visitExp(exp)

    case Expression.Apply(exp, exps, _, _, _) =>
      visitExp(exp) ++ visitExps(exps)

    case Expression.Unary(_, exp, _, _, _) =>
      visitExp(exp)

    case Expression.Binary(_, exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.Let(sym, _, exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.LetRec(sym, _, exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.Region(_, _) =>
      Set.empty

    case Expression.Scope(sym, _, exp, _, _, _) =>
      visitExp(exp)

    case Expression.IfThenElse(exp1, exp2, exp3, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

    case Expression.Stm(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.Discard(exp, _, _) =>
      visitExp(exp)

    case Expression.Match(exp, rules, _, _, _) =>
      visitExp(exp) ++ visitMatchRules(rules)

    case Expression.Choose(exps, rules, _, _, _) =>
      visitExps(exps) ++ visitChoiceRules(rules)

    case Expression.Tag(sym, _, exp, _, _, _) =>
      visitExp(exp)

    case Expression.Tuple(elms, _, _, _) =>
      visitExps(elms)

    case Expression.RecordEmpty(_, _) =>
      Set.empty

    case Expression.RecordSelect(exp, _, _, _, _) =>
      visitExp(exp)

    case Expression.RecordExtend(_, value, rest, _, _, _) =>
      visitExp(value) ++ visitExp(rest)

    case Expression.RecordRestrict(_, rest, _, _, _) =>
      visitExp(rest)

    case Expression.ArrayLit(exps, exp, _, _, _) =>
      visitExps(exps) ++ visitExp(exp)

    case Expression.ArrayNew(exp1, exp2, exp3, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

    case Expression.ArrayLoad(base, index, _, _, _) =>
      visitExp(base) ++ visitExp(index)

    case Expression.ArrayLength(base, _, _) =>
      visitExp(base)
    case Expression.ArrayStore(base, index, elm, _) =>
      visitExp(base) ++ visitExp(index) ++ visitExp(elm)

    case Expression.ArraySlice(base, beginIndex, endIndex, _, _) =>
      visitExp(base) ++ visitExp(beginIndex) ++ visitExp(endIndex)

    case Expression.Ref(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.Deref(exp, _, _, _) =>
      visitExp(exp)

    case Expression.Assign(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.Ascribe(exp, _, _, _) =>
      visitExp(exp)

    case Expression.Cast(exp, _, _, _, _, _) =>
      visitExp(exp)

    case Expression.TryCatch(exp, rules, _, _, _) =>
      visitExp(exp) ++ visitCatchRules(rules)

    case Expression.InvokeConstructor(_, args, _, _, _) =>
      visitExps(args)

    case Expression.InvokeMethod(_, exp, args, _, _, _) =>
      visitExp(exp) ++ visitExps(args)

    case Expression.InvokeStaticMethod(_, args, _, _, _) =>
      visitExps(args)

    case Expression.GetField(_, exp, _, _, _) =>
      visitExp(exp)

    case Expression.PutField(_, exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.GetStaticField(_, _, _, _) =>
      Set.empty

    case Expression.PutStaticField(_, exp, _, _, _) =>
      visitExp(exp)

    case Expression.NewObject(_, _, _, _) =>
      Set.empty

    case Expression.NewChannel(exp, _, _, _) =>
      visitExp(exp)

    case Expression.GetChannel(exp, _, _, _) =>
      visitExp(exp)

    case Expression.PutChannel(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.SelectChannel(rules, default, _, _, _) =>
      visitChannelRules(rules) ++ default.map(visitExp).getOrElse(Set.empty)

    case Expression.Spawn(exp, _, _, _) =>
      visitExp(exp)

    case Expression.Lazy(exp, _, _) =>
      visitExp(exp)

    case Expression.Force(exp, _, _, _) =>
      visitExp(exp)

    case Expression.FixpointConstraintSet(cs, _, _, _) =>
      visitConstraints(cs)

    case Expression.FixpointLambda(_, exp, _, _, _, _) =>
      visitExp(exp)

    case Expression.FixpointMerge(exp1, exp2, _, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.FixpointSolve(exp, _, _, _, _) =>
      visitExp(exp)

    case Expression.FixpointFilter(_, exp, _, _, _) =>
      visitExp(exp)

    case Expression.FixpointInject(exp, _, _, _, _) =>
      visitExp(exp)

    case Expression.FixpointProject(_, exp, _, _, _) =>
      visitExp(exp)

    case Expression.Reify(_, _, _, _) =>
      Set.empty

    case Expression.ReifyType(_, _, _, _, _) =>
      Set.empty

    case Expression.ReifyEff(_, exp1, exp2, exp3, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

  }

  /**
    * Returns the function symbols reachable from `es`.
    */
  private def visitExps(es: List[Expression]): Set[Symbol.DefnSym] = es.map(visitExp).fold(Set())(_ ++ _)

}
