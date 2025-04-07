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
import ca.uwaterloo.flix.language.ast.{MonoAst, OccurrenceAst1, Symbol}
import ca.uwaterloo.flix.language.dbg.AstPrinter.*
import ca.uwaterloo.flix.util.collection.ListMap

/**
  * Iterative runs of the optimizer pipeline: OccurrenceAnalyzer -> Inliner.
  */
object Optimizer1 {

  /**
    * Returns an optimized version of the given AST `root`.
    */
  def run(root: MonoAst.Root)(implicit flix: Flix): MonoAst.Root = flix.phase("Optimizer1") {
    if (flix.options.xnooptimizer1) {
      root
    } else {
      var result = Converter.toOccurrenceAst(root)
      var stats: Stats = null
      for (_ <- 1 to flix.options.inliner1Rounds) {
        val afterOccurrenceAnalyzer = OccurrenceAnalyzer1.run(result)
        val (afterInliner, stats1) = Inliner1.run(afterOccurrenceAnalyzer)
        stats = if (stats == null) stats1 else stats ++ stats1
        result = afterInliner
      }
      // println(stats)
      Converter.toMonoAst(result)
    }
  }

  private object Converter {
    def toOccurrenceAst(root: MonoAst.Root): OccurrenceAst1.Root = ???

    def toMonoAst(root: OccurrenceAst1.Root): MonoAst.Root = ???
  }

  case class Stats(inlinedDefs: ListMap[Symbol.DefnSym, Symbol.DefnSym],
                   inlinedVars: ListMap[Symbol.DefnSym, Symbol.VarSym],
                   betaReductions: Map[Symbol.DefnSym, Int],
                   eliminatedVars: ListMap[Symbol.DefnSym, Symbol.VarSym],
                   simplifiedIfThenElse: Map[Symbol.DefnSym, Int],
                   eliminatedStms: Map[Symbol.DefnSym, Int]) {
    def ++(that: Stats): Stats = {
      val inlinedDefs1 = inlinedDefs ++ that.inlinedDefs
      val inlinedVars1 = inlinedVars ++ that.inlinedVars
      val betaReductions1 = Stats.merge(betaReductions, that.betaReductions)(_ + _)
      val eliminatedVars1 = eliminatedVars ++ that.eliminatedVars
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
