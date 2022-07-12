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
  def run(root: Root)(implicit flix: Flix): Validation[Root, CompilationMessage] = flix.phase("EarlyTreeShaker") {
    // Compute the symbols that are always reachable.
    val initReach = initReachable(root)

    // Compute the symbols that are transitively reachable.
    val allReachable = ParOps.parReachable(initReach, visitSym(_, root))

    // Filter the reachable definitions.
    val reachableDefs = root.defs.filter {
      case (sym, _) => allReachable.contains(ReachableSym.DefnSym(sym))
    }

    val reachableSigs = root.sigs.filter {
      case (sym, _) => allReachable.contains(ReachableSym.SigSym(sym))
    }

    val reachableInstances = root.instances.filter {
      case (sym, _) => allReachable.contains(ReachableSym.ClassSym(sym))
    }

    // Reassemble the AST.
    root.copy(defs = reachableDefs, sigs = reachableSigs, instances = reachableInstances).toSuccess
  }

  /**
    * Returns the symbols that are always reachable.
    */
  private def initReachable(root: Root): Set[ReachableSym] = {
    // A set used to collect the symbols of reachable functions.
    var reachable: Set[ReachableSym] = root.reachable.map(ReachableSym.DefnSym)

    //
    // (a) The main function is always reachable (if it exists).
    //
    reachable = reachable ++ root.entryPoint.toList.map(ReachableSym.DefnSym)

    //
    // (b) A function annotated with @benchmark or @test is always reachable.
    //
    for ((sym, defn) <- root.defs) {
      if (isBenchmark(defn) || isTest(defn)) {
        reachable = reachable + ReachableSym.DefnSym(sym)
      }
    }
    reachable
  }

  /**
    * Returns `true` if `defn` is annotated with `@benchmark`
    */
  private def isBenchmark(defn: TypedAst.Def): Boolean = defn.spec.ann.exists { a =>
    a.name match {
      case Annotation.Benchmark(_) => true
      case _ => false
    }
  }

  /**
    * Returns `true` if `defn` is annotated with `@test`
    */
  private def isTest(defn: TypedAst.Def): Boolean = defn.spec.ann.exists { a =>
    a.name match {
      case Annotation.Test(_) => true
      case _ => false
    }
  }

  /**
    * Returns the symbols reachable from the given symbol `sym`.
    */
  private def visitSym(sym: ReachableSym, root: Root): Set[ReachableSym] = sym match {
    case ReachableSym.DefnSym(defnSym) => root.defs.get(defnSym) match {
      case None => Set.empty
      case Some(defn) => visitExp(defn.impl.exp) ++ visitExps(defn.spec.ann.flatMap(_.args))
    }

    case ReachableSym.SigSym(sigSym) => root.sigs.get(sigSym) match {
      case None => Set.empty
      case Some(Sig(sigSym, spec, impl)) =>
        Set(ReachableSym.ClassSym(sigSym.clazz)) ++
          visitExps(spec.ann.flatMap(_.args)) ++
          impl.map(i => visitExp(i.exp)).getOrElse(Set.empty)
    }

    case ReachableSym.ClassSym(classSym) => root.instances.get(classSym) match {
      case None => Set.empty
      case Some(instances) =>
        instances.flatMap(_.defs.map(d => ReachableSym.DefnSym(d.sym))).toSet ++
          visitExps(instances.flatMap(_.ann.flatMap(_.args))) ++
          visitExps(instances.flatMap(_.defs.map(_.impl.exp))) ++
          visitExps(instances.flatMap(_.defs.flatMap(_.spec.ann.flatMap(_.args))))
    }
  }

  /**
    * Returns the symbols reachable from the given list of constraints `constraints`.
    */
  def visitConstraints(constraints: List[Constraint]): Set[ReachableSym] = {
    def visitHead(h: Head): Set[ReachableSym] = h match {
      case Head.Atom(_, _, terms, _, _) => visitExps(terms)
    }

    def visitBody(b: List[Body]): Set[ReachableSym] = {
      b.foldLeft(Set.empty: Set[ReachableSym]) {
        case (acc, Body.Guard(exp, _)) => visitExp(exp) ++ acc
        case (acc, Body.Loop(_, exp, _)) => visitExp(exp) ++ acc
        case (acc, _) => acc
      }
    }

    constraints.foldLeft(Set.empty: Set[ReachableSym])((acc, c) => visitHead(c.head) ++ visitBody(c.body) ++ acc)
  }

  /**
    * Returns the function symbols reachable from the given expression `e0`.
    */
  private def visitExp(e0: Expression): Set[ReachableSym] = e0 match {
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
      Set(ReachableSym.DefnSym(sym))

    case Expression.Sig(sym, _, _) =>
      Set(ReachableSym.SigSym(sym))

    case Expression.Hole(_, _, _) =>
      Set.empty

    case Expression.Lambda(_, exp, _, _) =>
      visitExp(exp)

    case Expression.Apply(exp, exps, _, _, _, _) =>
      visitExp(exp) ++ visitExps(exps)

    case Expression.Unary(_, exp, _, _, _, _) =>
      visitExp(exp)

    case Expression.Binary(_, exp1, exp2, _, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.Let(_, _, exp1, exp2, _, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.LetRec(_, _, exp1, exp2, _, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.Region(_, _) =>
      Set.empty

    case Expression.Scope(_, _, exp, _, _, _, _) =>
      visitExp(exp)

    case Expression.IfThenElse(exp1, exp2, exp3, _, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

    case Expression.Stm(exp1, exp2, _, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.Discard(exp, _, _, _) =>
      visitExp(exp)

    case Expression.Match(exp, rules, _, _, _, _) =>
      visitExp(exp) ++ visitExps(rules.map(_.exp)) ++ visitExps(rules.map(_.guard))

    case Expression.Choose(exps, rules, _, _, _, _) =>
      visitExps(exps) ++ visitExps(rules.map(_.exp))

    case Expression.Tag(_, _, exp, _, _, _, _) =>
      visitExp(exp)

    case Expression.Tuple(elms, _, _, _, _) =>
      visitExps(elms)

    case Expression.RecordEmpty(_, _) =>
      Set.empty

    case Expression.RecordSelect(exp, _, _, _, _, _) =>
      visitExp(exp)

    case Expression.RecordExtend(_, value, rest, _, _, _, _) =>
      visitExp(value) ++ visitExp(rest)

    case Expression.RecordRestrict(_, rest, _, _, _, _) =>
      visitExp(rest)

    case Expression.ArrayLit(exps, exp, _, _, _, _) =>
      visitExps(exps) ++ visitExp(exp)

    case Expression.ArrayNew(exp1, exp2, exp3, _, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

    case Expression.ArrayLoad(base, index, _, _, _, _) =>
      visitExp(base) ++ visitExp(index)

    case Expression.ArrayLength(base, _, _, _) =>
      visitExp(base)

    case Expression.ArrayStore(base, index, elm, _, _) =>
      visitExp(base) ++ visitExp(index) ++ visitExp(elm)

    case Expression.ArraySlice(base, beginIndex, endIndex, _, _, _) =>
      visitExp(base) ++ visitExp(beginIndex) ++ visitExp(endIndex)

    case Expression.Ref(exp1, exp2, _, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.Deref(exp, _, _, _, _) =>
      visitExp(exp)

    case Expression.Assign(exp1, exp2, _, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.Ascribe(exp, _, _, _, _) =>
      visitExp(exp)

    case Expression.Cast(exp, _, _, _, _, _, _, _) =>
      visitExp(exp)

    case Expression.TryCatch(exp, rules, _, _, _, _) =>
      visitExp(exp) ++ visitExps(rules.map(_.exp))

    case Expression.InvokeConstructor(_, args, _, _, _, _) =>
      visitExps(args)

    case Expression.InvokeMethod(_, exp, args, _, _, _, _) =>
      visitExp(exp) ++ visitExps(args)

    case Expression.InvokeStaticMethod(_, args, _, _, _, _) =>
      visitExps(args)

    case Expression.GetField(_, exp, _, _, _, _) =>
      visitExp(exp)

    case Expression.PutField(_, exp1, exp2, _, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.GetStaticField(_, _, _, _, _) =>
      Set.empty

    case Expression.PutStaticField(_, exp, _, _, _, _) =>
      visitExp(exp)

    case Expression.NewObject(_, _, _, _, _) =>
      Set.empty

    case Expression.NewChannel(exp, _, _, _, _) =>
      visitExp(exp)

    case Expression.GetChannel(exp, _, _, _, _) =>
      visitExp(exp)

    case Expression.PutChannel(exp1, exp2, _, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.SelectChannel(rules, default, _, _, _, _) =>
      visitExps(rules.map(_.chan)) ++ visitExps(rules.map(_.exp)) ++ default.map(visitExp).getOrElse(Set.empty)

    case Expression.Spawn(exp, _, _, _, _) =>
      visitExp(exp)

    case Expression.Lazy(exp, _, _) =>
      visitExp(exp)

    case Expression.Force(exp, _, _, _, _) =>
      visitExp(exp)

    case Expression.FixpointConstraintSet(cs, _, _, _) =>
      visitConstraints(cs)

    case Expression.FixpointLambda(_, exp, _, _, _, _, _) =>
      visitExp(exp)

    case Expression.FixpointMerge(exp1, exp2, _, _, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.FixpointSolve(exp, _, _, _, _, _) =>
      visitExp(exp)

    case Expression.FixpointFilter(_, exp, _, _, _, _) =>
      visitExp(exp)

    case Expression.FixpointInject(exp, _, _, _, _, _) =>
      visitExp(exp)

    case Expression.FixpointProject(_, exp, _, _, _, _) =>
      visitExp(exp)

    case Expression.Reify(_, _, _, _, _) =>
      Set.empty

    case Expression.ReifyType(_, _, _, _, _, _) =>
      Set.empty

    case Expression.ReifyEff(_, exp1, exp2, exp3, _, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

    case Expression.Do(sym, exps, _, _, _) =>
      visitExps(exps) // Add OpSym to reachable syms?

    case Expression.Resume(exp, _, _) =>
      visitExp(exp)

    case Expression.TryWith(exp, _, rules, _, _, _, _) =>
      visitExp(exp) ++ visitExps(rules.map(_.exp))

    case Expression.Without(exp, _, _, _, _, _) =>
      visitExp(exp)

  }

  /**
    * Returns the function symbols reachable from `es`.
    */
  private def visitExps(es: List[Expression]): Set[ReachableSym] = es.map(visitExp).fold(Set())(_ ++ _)


  /**
    * Represents a super type for reachable symbols in the AST.
    */
  sealed trait ReachableSym

  object ReachableSym {

    case class DefnSym(defnSym: Symbol.DefnSym) extends ReachableSym

    case class SigSym(sigSym: Symbol.SigSym) extends ReachableSym

    case class ClassSym(classSym: Symbol.ClassSym) extends ReachableSym

  }

}
