/*
 * Copyright 2026 Magnus Madsen
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
package ca.uwaterloo.flix.tools.compilertop

import ca.uwaterloo.flix.tools.compilertop.Formatting.lineCount
import ca.uwaterloo.flix.tools.compilertop.Model.*
import ca.uwaterloo.flix.tools.compilertop.Profiler.DefnStats

/**
  * Pure filter / sort / module-rollup logic for compiler-top stat snapshots.
  * Operates over `Vector[DefnStats]` and produces filtered / re-aggregated
  * views consumed by the renderer.
  */
object Aggregation {

  /** True if `phase` should be accounted for under the current `f`. */
  def matchesFilter(phase: String, f: PhaseFilter): Boolean = f match {
    case PhaseFilter.All      => true
    case PhaseFilter.Frontend => FrontendPhases.contains(phase)
    case PhaseFilter.Backend  => phase != "?" && !FrontendPhases.contains(phase)
  }

  /**
    * Projects each [[DefnStats]] through the active filter: keep only the
    * `byPhase` and `byPhaseCount` entries whose phase matches, and recompute
    * `totalNanos` and `callCount` as the sum over the kept phases. Module
    * aggregation re-rolls from these filtered defs, so module rows track
    * the filter automatically.
    *
    * `loc` isn't per-phase and passes through unchanged.
    */
  def applyFilter(snap: Vector[DefnStats], f: PhaseFilter): Vector[DefnStats] = {
    if (f == PhaseFilter.All) return snap
    snap.map { s =>
      val keptPhases = s.byPhase.filter { case (p, _) => matchesFilter(p, f) }
      val keptCounts = s.byPhaseCount.filter { case (p, _) => matchesFilter(p, f) }
      val keptAlloc  = s.byPhaseAlloc.filter { case (p, _) => matchesFilter(p, f) }
      val keptTotal = keptPhases.values.sum
      // Sum of per-phase counts cannot overflow Int in any realistic build.
      val keptCount = keptCounts.values.sum.toInt
      val keptAllocTotal = keptAlloc.values.sum
      s.copy(totalNanos = keptTotal, callCount = keptCount, byPhase = keptPhases, byPhaseCount = keptCounts, byPhaseAlloc = keptAlloc, allocBytes = keptAllocTotal)
    }
  }

  /** Sums `byPhaseCount` entries for the given phase set. */
  def sumPhaseCounts(byPhaseCount: Map[String, Long], phases: Set[String]): Long =
    byPhaseCount.iterator.collect { case (p, n) if phases.contains(p) => n }.sum

  /**
    * Returns the descending sort key for a def under the given sort mode.
    * `Hotness` (time / LOC) is 0 for synthetic defs with no real source
    * span (`locLines <= 0`) so they sink to the bottom rather than dominating.
    */
  def defSortKey(s: DefnStats, srt: Sort): Double = srt match {
    case Sort.Time    => s.totalNanos.toDouble
    case Sort.Hotness => Formatting.hotnessMsPerLine(s.totalNanos, lineCount(s.loc))
    case Sort.Mono  => sumPhaseCounts(s.byPhaseCount, MonoCountPhases).toDouble
    case Sort.Depth => if (s.maxDepth < 0) Double.NegativeInfinity else s.maxDepth.toDouble
    case Sort.Opt   => sumPhaseCounts(s.byPhaseCount, OptCountPhases).toDouble
    case Sort.Inl   => s.inlined.toDouble
    case Sort.Cls   => sumPhaseCounts(s.byPhaseCount, ClsCountPhases).toDouble
    case Sort.Size  => s.classBytes.toDouble
    case Sort.Alloc => s.allocBytes.toDouble
    case Sort.Cns   => s.cns.toDouble
    case Sort.Tvars => s.tvars.toDouble
    case Sort.Evars => s.evars.toDouble
  }

  /** Module-level analogue of [[defSortKey]], applied after summing across each module's defs. */
  def moduleSortKey(m: ModuleStats, srt: Sort): Double = srt match {
    case Sort.Time    => m.totalNanos.toDouble
    case Sort.Hotness => Formatting.hotnessMsPerLine(m.totalNanos, m.totalLines)
    case Sort.Mono  => sumPhaseCounts(m.byPhaseCount, MonoCountPhases).toDouble
    case Sort.Depth => if (m.maxDepth < 0) Double.NegativeInfinity else m.maxDepth.toDouble
    case Sort.Opt   => sumPhaseCounts(m.byPhaseCount, OptCountPhases).toDouble
    case Sort.Inl   => m.totalInlined.toDouble
    case Sort.Cls   => sumPhaseCounts(m.byPhaseCount, ClsCountPhases).toDouble
    case Sort.Size  => m.totalClassBytes.toDouble
    case Sort.Alloc => m.totalAllocBytes.toDouble
    case Sort.Cns   => m.totalCns.toDouble
    case Sort.Tvars => m.totalTvars.toDouble
    case Sort.Evars => m.totalEvars.toDouble
  }

  /** Groups `snap` by namespace, sums each metric, and returns rows sorted by `srt` descending. */
  def aggregateByModule(snap: Vector[DefnStats], srt: Sort): Vector[ModuleStats] = {
    val groups = snap.groupBy { s =>
      val ns = s.sym.namespace
      if (ns.isEmpty) "(root)" else ns.mkString(".")
    }
    groups.iterator.map { case (mod, defs) =>
      val totalNanos = defs.iterator.map(_.totalNanos).sum
      val totalCallCount = defs.iterator.map(_.callCount.toLong).sum
      val totalLines = defs.iterator.map(s => lineCount(s.loc)).sum
      val byPhase = defs.foldLeft(Map.empty[String, Long]) { (acc, d) =>
        d.byPhase.foldLeft(acc) { case (m, (phase, n)) =>
          m.updated(phase, m.getOrElse(phase, 0L) + n)
        }
      }
      val byPhaseCount = defs.foldLeft(Map.empty[String, Long]) { (acc, d) =>
        d.byPhaseCount.foldLeft(acc) { case (m, (phase, n)) =>
          m.updated(phase, m.getOrElse(phase, 0L) + n)
        }
      }
      val byPhaseAlloc = defs.foldLeft(Map.empty[String, Long]) { (acc, d) =>
        d.byPhaseAlloc.foldLeft(acc) { case (m, (phase, n)) =>
          m.updated(phase, m.getOrElse(phase, 0L) + n)
        }
      }
      val totalCns = defs.iterator.map(_.cns).sum
      val totalTvars = defs.iterator.map(_.tvars.toLong).sum
      val totalEvars = defs.iterator.map(_.evars.toLong).sum
      val totalInlined = defs.iterator.map(_.inlined).sum
      val totalClassBytes = defs.iterator.map(_.classBytes).sum
      val totalAllocBytes = defs.iterator.map(_.allocBytes).sum
      // `max` of all-(-1)s is `-1`, propagating the per-def "never observed"
      // sentinel up. Otherwise the module shows the deepest BFS wave any of
      // its defs sits at.
      val maxDepth = defs.iterator.map(_.maxDepth).maxOption.getOrElse(-1)
      ModuleStats(mod, totalNanos, totalCallCount, totalLines, byPhase, byPhaseCount, byPhaseAlloc, totalCns, totalTvars, totalEvars, totalInlined, totalClassBytes, totalAllocBytes, maxDepth)
    }.toVector.sortBy(m => -moduleSortKey(m, srt))
  }
}
