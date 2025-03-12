/*
 * Copyright 2024 Anna Krogh, Patrick Lundvig, Christian Bonde
 * 2024 Jakob Schneider Villumsen
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
import ca.uwaterloo.flix.language.ast.MonoAst
import ca.uwaterloo.flix.language.dbg.AstPrinter.*
import ca.uwaterloo.flix.language.ast.Symbol

/**
  * Iterative runs of the optimizer pipeline: OccurrenceAnalyzer -> Inliner.
  */
object Optimizer1 {

  /**
    * Returns an optimized version of the given AST `root`.
    */
  def run(root: MonoAst.Root)(implicit flix: Flix): MonoAst.Root = flix.phase("Optimizer1") {
    if (flix.options.xnooptimizer1 || flix.options.inliner1Rounds < 1) {
      root
    } else {
      var result = OccurrenceAnalyzer1.run(root)
      var stats: Stats = null
      for (_ <- 2 to flix.options.inliner1Rounds) {
        val (afterInliner, stats1) = Inliner2.run(result)
        val afterOccurrenceAnalyzer = OccurrenceAnalyzer2.run(afterInliner)
        stats = if (stats == null) stats1 else stats ++ stats1
        result = afterOccurrenceAnalyzer
      }
      val (afterInliner, stats1) = Inliner1.run(result)
      stats = stats ++ stats1
      // println(stats)
      afterInliner
    }
  }

  case class Stats(inlinedDefs: Map[Symbol.DefnSym, Set[Symbol.DefnSym]],
                   inlinedVars: Map[Symbol.DefnSym, Set[Symbol.VarSym]],
                   betaReductions: Map[Symbol.DefnSym, Int],
                   eliminatedVars: Map[Symbol.DefnSym, Set[Symbol.VarSym]],
                   simplifiedIfThenElse: Map[Symbol.DefnSym, Int],
                   eliminatedStms: Map[Symbol.DefnSym, Int]) {
    def ++(that: Stats): Stats = {
      val inlinedDefs1 = Stats.merge(inlinedDefs, that.inlinedDefs)(_ ++ _)
      val inlinedVars1 = Stats.merge(inlinedVars, that.inlinedVars)(_ ++ _)
      val betaReductions1 = Stats.merge(betaReductions, that.betaReductions)(_ + _)
      val eliminatedVars1 = Stats.merge(eliminatedVars, that.eliminatedVars)(_ ++ _)
      val simplifiedIfThenElse1 = Stats.merge(simplifiedIfThenElse, that.simplifiedIfThenElse)(_ + _)
      val eliminatedStms1 = Stats.merge(eliminatedStms, that.eliminatedStms)(_ + _)
      Stats(inlinedDefs1, inlinedVars1, betaReductions1, eliminatedVars1, simplifiedIfThenElse1, eliminatedStms1)
    }

    override def toString: String = {
      s"""====== STATISTICS ======
         |
         |Inlined Defs: $inlinedDefs
         |
         |Inlined Vars: $inlinedVars
         |
         |Eliminated Vars: $eliminatedVars
         |
         |Beta Reductions: $betaReductions
         |
         |Eliminated Stms: $eliminatedStms
         |
         |Eliminated Ifs: $simplifiedIfThenElse
         |""".stripMargin
    }
  }

  object Stats {
    def merge[A, B](m1: Map[A, B], m2: Map[A, B])(combine: (B, B) => B): Map[A, B] = {
      val smallest = if (m1.size < m2.size) m1 else m2
      val biggest = if (m1.size >= m2.size) m1 else m2
      smallest.foldLeft(biggest) {
        case (acc, (k, v1)) => acc.get(k) match {
          case Some(v2) => acc + (k -> combine(v1, v2))
          case None => acc + (k -> v1)
        }
      }
    }

    def toMapSet[A, B](iterable: Iterable[(A, B)]): Map[A, Set[B]] = {
      iterable.foldLeft(Map.empty[A, Set[B]]) {
        case (m, (k, v)) => m.get(k) match {
          case Some(set) => m + (k -> (set + v))
          case None => m + (k -> Set(v))
        }
      }
    }

    def toCount[A](iterable: Iterable[(A, Int)]): Map[A, Int] = {
      iterable.foldLeft(Map.empty[A, Int]) {
        case (m, (k, i)) => m.get(k) match {
          case Some(j) => m + (k -> (i + j))
          case None => m + (k -> i)
        }
      }
    }
  }
}
