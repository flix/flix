/*
 * Copyright 2025 Jakob Schneider Villumsen
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
package ca.uwaterloo.flix.language.phase.optimizer

import ca.uwaterloo.flix.api.{CompilerConstants, Flix, FlixEvent}
import ca.uwaterloo.flix.language.ast.MonoAst
import ca.uwaterloo.flix.language.dbg.AstPrinter.DebugMonoAst

object Optimizer {

  /**
    * Returns an optimized version of the given AST `root`.
    */
  def run(root: MonoAst.Root)(implicit flix: Flix): MonoAst.Root = flix.phase("Optimizer") {
    // Snapshot pre-Optimizer body sizes so consumers (the compiler-top profiler)
    // can later compute the inliner-induced growth ratio per def.
    val preSizes = root.defs.iterator.map { case (sym, d) => sym -> nodeCount(d.exp) }.toMap

    var currentRoot = root
    var currentDelta = currentRoot.defs.keys.toSet
    for (_ <- 0 until CompilerConstants.MaxOptimizerRounds) {
      if (currentDelta.nonEmpty) {
        val afterOccurrenceAnalyzer = OccurrenceAnalyzer.run(currentRoot, currentDelta)
        val (newRoot, newDelta) = Inliner.run(afterOccurrenceAnalyzer)
        currentRoot = newRoot
        currentDelta = newDelta
      }
    }

    val postSizes = currentRoot.defs.iterator.map { case (sym, d) => sym -> nodeCount(d.exp) }.toMap
    flix.emitEvent(FlixEvent.OptimizerBodySizes(preSizes, postSizes))

    currentRoot
  }

  /**
    * Returns the number of [[MonoAst.Expr]] nodes in `e`, counting `e` itself and recursively
    * every nested expression reachable through its children — including those inside match /
    * try / handler rules and `NewObject` constructors / methods.
    *
    * Sized by AST nodes, not bytecode bytes. The unit is informal but stable across a single
    * compile, so the relative growth (`post / pre`) is meaningful even though the absolute
    * count isn't directly comparable to any external measure.
    */
  private def nodeCount(e: MonoAst.Expr): Int = e match {
    case MonoAst.Expr.Cst(_, _, _)                              => 1
    case MonoAst.Expr.Var(_, _, _)                              => 1
    case MonoAst.Expr.Lambda(_, exp, _, _)                      => 1 + nodeCount(exp)
    case MonoAst.Expr.ApplyAtomic(_, exps, _, _, _)             => 1 + sumNodes(exps)
    case MonoAst.Expr.ApplyClo(e1, e2, _, _, _)                 => 1 + nodeCount(e1) + nodeCount(e2)
    case MonoAst.Expr.ApplyDef(_, exps, _, _, _, _)             => 1 + sumNodes(exps)
    case MonoAst.Expr.ApplyLocalDef(_, exps, _, _, _)           => 1 + sumNodes(exps)
    case MonoAst.Expr.ApplyOp(_, exps, _, _, _)                 => 1 + sumNodes(exps)
    case MonoAst.Expr.Let(_, e1, e2, _, _, _, _)                => 1 + nodeCount(e1) + nodeCount(e2)
    case MonoAst.Expr.LocalDef(_, _, e1, e2, _, _, _, _)        => 1 + nodeCount(e1) + nodeCount(e2)
    case MonoAst.Expr.Region(_, _, exp, _, _, _)                => 1 + nodeCount(exp)
    case MonoAst.Expr.IfThenElse(e1, e2, e3, _, _, _)           => 1 + nodeCount(e1) + nodeCount(e2) + nodeCount(e3)
    case MonoAst.Expr.Stm(exps, exp, _, _, _)                   => 1 + sumNodes(exps) + nodeCount(exp)
    case MonoAst.Expr.Discard(exp, _, _)                        => 1 + nodeCount(exp)
    case MonoAst.Expr.Match(exp, rules, _, _, _)                =>
      1 + nodeCount(exp) + rules.iterator.map(r => r.guard.map(nodeCount).getOrElse(0) + nodeCount(r.exp)).sum
    case MonoAst.Expr.ExtMatch(exp, rules, _, _, _)             =>
      1 + nodeCount(exp) + rules.iterator.map(r => nodeCount(r.exp)).sum
    case MonoAst.Expr.VectorLit(exps, _, _, _)                  => 1 + sumNodes(exps)
    case MonoAst.Expr.VectorLoad(e1, e2, _, _, _)               => 1 + nodeCount(e1) + nodeCount(e2)
    case MonoAst.Expr.VectorLength(exp, _)                      => 1 + nodeCount(exp)
    case MonoAst.Expr.Cast(exp, _, _, _)                        => 1 + nodeCount(exp)
    case MonoAst.Expr.TryCatch(exp, rules, _, _, _)             =>
      1 + nodeCount(exp) + rules.iterator.map(r => nodeCount(r.exp)).sum
    case MonoAst.Expr.RunWith(exp, _, rules, _, _, _)           =>
      1 + nodeCount(exp) + rules.iterator.map(r => nodeCount(r.exp)).sum
    case MonoAst.Expr.NewObject(_, _, _, _, ctors, methods, _)  =>
      1 + ctors.iterator.map(c => nodeCount(c.exp)).sum + methods.iterator.map(m => nodeCount(m.exp)).sum
  }

  /** Sum of [[nodeCount]] across `exps` via an iterator to avoid intermediate List allocations. */
  private def sumNodes(exps: List[MonoAst.Expr]): Int = {
    var acc = 0
    val it = exps.iterator
    while (it.hasNext) acc += nodeCount(it.next())
    acc
  }

}
