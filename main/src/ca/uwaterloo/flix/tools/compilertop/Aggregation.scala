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

import ca.uwaterloo.flix.api.PhaseTime
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
    * `totalNanos` as the sum over the kept phases. Module aggregation re-rolls
    * from these filtered defs, so module rows track the filter automatically.
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
      val keptAllocTotal = keptAlloc.values.sum
      s.copy(totalNanos = keptTotal, byPhase = keptPhases, byPhaseCount = keptCounts, byPhaseAlloc = keptAlloc, allocBytes = keptAllocTotal)
    }
  }

  /** Sums `byPhaseCount` entries for the given phase set. */
  def sumPhaseCounts(byPhaseCount: Map[String, Long], phases: Set[String]): Long =
    byPhaseCount.iterator.collect { case (p, n) if phases.contains(p) => n }.sum

  /**
    * Splits attributable phase wall time into accounted vs unaccounted for the
    * dashboard `observed` figure — see [[Model.Coverage]] for the semantics.
    *
    * For each phase we estimate how much of its wall slice the per-def table
    * actually attributes: the thread-summed per-def time spread across `par`
    * threads, i.e. `attributedWall ≈ threadSummed / par`, clamped to the phase
    * wall. This *optimistically assumes full parallelism* — accurate for a
    * saturated parallel phase (where `threadSummed ≈ par × wall`), but it
    * under-counts a sequential phase (which ran on one thread, yet is divided by
    * `par`), so such a phase reads as a partial blind spot even when fully
    * instrumented.
    *
    * Phase wall and per-def times are summed by name first, since a phase can
    * run more than once (e.g. `OccurrenceAnalyzer` / `Inliner` across optimizer
    * rounds). Phases in [[Model.NonAttributablePhases]] are dropped entirely
    * (they have no per-def unit). The unknown-phase bucket `"?"` never appears
    * as a real phase-timer entry, so it can't leak in.
    *
    * Scoped to the active filter `f` via [[matchesFilter]] — the same predicate
    * the def table uses — so the figure covers exactly the phases the visible
    * table covers (e.g. under `Frontend` only `FrontendPhases` count).
    */
  def computeCoverage(snapshot: Vector[DefnStats], phaseTimers: Vector[PhaseTime], f: PhaseFilter, par: Int): Coverage = {
    val threads = par.max(1)

    // Thread-summed per-def time attributed to each phase.
    val attributedByPhase = snapshot.foldLeft(Map.empty[String, Long]) {
      case (acc, s) => s.byPhase.foldLeft(acc) { case (m, (p, n)) => m.updated(p, m.getOrElse(p, 0L) + n) }
    }

    val wallByPhase = phaseTimers.foldLeft(Map.empty[String, Long]) {
      case (acc, pt) => acc.updated(pt.phase, acc.getOrElse(pt.phase, 0L) + pt.time)
    }

    var accountedNanos = 0L
    var unaccountedNanos = 0L
    wallByPhase.foreach {
      case (phase, _) if !matchesFilter(phase, f)              => // outside the active filter's phases
      case (phase, _) if NonAttributablePhases.contains(phase) => // excluded: no per-def unit
      case (phase, wall) =>
        // Optimistically assume full parallelism: thread-summed time / threads.
        val attributedWall = (attributedByPhase.getOrElse(phase, 0L) / threads).min(wall)
        accountedNanos += attributedWall
        unaccountedNanos += wall - attributedWall
    }
    Coverage(accountedNanos, unaccountedNanos)
  }

  /**
    * Builds one [[PhaseStats]] row per phase that ran, for the per-phase table.
    *
    * Phase wall time comes from `phaseTimers` (the authoritative list of phases,
    * summed by name since a phase can run more than once); the per-def time and
    * alloc come from the snapshot's `byPhase` / `byPhaseAlloc`. `attributedWall`
    * reuses [[computeCoverage]]'s optimistic-parallelism estimate
    * (`threadSummed / par`, clamped to wall), so the per-row `pctObserved`
    * matches the dashboard `observed` figure phase-for-phase.
    *
    * Unlike [[computeCoverage]], [[Model.NonAttributablePhases]] are NOT dropped:
    * the table deliberately shows phases like `Lexer` / `Parser2` with their real
    * `%wall` and `0%` observed. Rows are filtered by `f` (so `a`/`f`/`b` narrow the
    * phase list like the other tables) and sorted by wall time descending.
    */
  def aggregateByPhase(snapshot: Vector[DefnStats], phaseTimers: Vector[PhaseTime], f: PhaseFilter, par: Int): Vector[PhaseStats] = {
    val threads = par.max(1)

    val timeByPhase = snapshot.foldLeft(Map.empty[String, Long]) {
      case (acc, s) => s.byPhase.foldLeft(acc) { case (m, (p, n)) => m.updated(p, m.getOrElse(p, 0L) + n) }
    }
    val allocByPhase = snapshot.foldLeft(Map.empty[String, Long]) {
      case (acc, s) => s.byPhaseAlloc.foldLeft(acc) { case (m, (p, n)) => m.updated(p, m.getOrElse(p, 0L) + n) }
    }
    val wallByPhase = phaseTimers.foldLeft(Map.empty[String, Long]) {
      case (acc, pt) => acc.updated(pt.phase, acc.getOrElse(pt.phase, 0L) + pt.time)
    }

    wallByPhase.iterator.collect {
      case (phase, wall) if matchesFilter(phase, f) =>
        val threadSummed = timeByPhase.getOrElse(phase, 0L)
        val attributedWall = (threadSummed / threads).min(wall)
        PhaseStats(phase, wall, threadSummed, allocByPhase.getOrElse(phase, 0L), attributedWall)
    }.toVector.sortBy(p => -p.wallNanos)
  }

  /**
    * Returns the descending sort key for a def under the given sort mode.
    * `Hotness` (time / LOC) is 0 for synthetic defs with no real source
    * span (`locLines <= 0`) so they sink to the bottom rather than dominating.
    */
  def defSortKey(s: DefnStats, srt: Sort): Double = srt match {
    case Sort.Time    => s.totalNanos.toDouble
    case Sort.Hotness => Formatting.hotnessMsPerLine(s.totalNanos, lineCount(s.loc))
    case Sort.Mono  => sumPhaseCounts(s.byPhaseCount, MonoCountPhases).toDouble
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
      ModuleStats(mod, totalNanos, totalLines, byPhase, byPhaseCount, byPhaseAlloc, totalCns, totalTvars, totalEvars, totalInlined, totalClassBytes, totalAllocBytes)
    }.toVector.sortBy(m => -moduleSortKey(m, srt))
  }
}
