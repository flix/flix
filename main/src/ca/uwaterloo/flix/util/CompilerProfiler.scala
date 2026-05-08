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
package ca.uwaterloo.flix.util

import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol}

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.{AtomicInteger, AtomicLong, AtomicReference}
import scala.jdk.CollectionConverters.*

/**
  * Per-`DefnSym` statistics: total wall-clock time, number of times the
  * compiler visited the def, and a per-phase breakdown of where the time
  * went.
  */
final case class DefnStats(
  sym: Symbol.DefnSym,
  loc: SourceLocation,
  totalNanos: Long,
  callCount: Int,
  byPhase: Map[String, Long]
) {
  /** Returns the phase that consumed the most time, or None if empty. */
  def dominantPhase: Option[String] =
    if (byPhase.isEmpty) None else Some(byPhase.maxBy(_._2)._1)
}

/**
  * Tracks cumulative wall-clock time, call counts, and per-phase breakdowns
  * for each [[Symbol.DefnSym]]. Safe to call from multiple threads.
  *
  * `phaseProvider` is consulted at the start of every [[track]] call to
  * attribute the elapsed time to the currently running phase.
  *
  * == Roll-up of refreshed symbols ==
  *
  * A fresh [[Symbol.DefnSym]] minted from an existing sym via
  * [[Symbol.freshDefnSym]] is recorded under that existing sym, not under
  * its own identity. So every specialization produced by `Monomorpher` and
  * every closure/local-def lifted out by `LambdaLift` folds back into the
  * source sym it was derived from, and the user-facing table shows one row
  * per source-level definition rather than one row per fresh refresh.
  *
  * The folding is implemented inside this profiler (see [[sourceOf]]) and
  * relies on [[Symbol.freshDefnSym]] preserving the source sym's
  * `(namespace, text, loc)` triple — which is true today across all three
  * refresh sites (`Specialization`, `LambdaLift` closure lift, `LambdaLift`
  * local-def lift).
  *
  * == Instrumented phases ==
  *
  * The following phases call [[track]] and contribute per-`DefnSym` data:
  *
  *   - Frontend (10 sites):
  *     `Kinder` (visitDefSpecs + visitDefs), `Typer`, `EntryPoints`,
  *     `PredDeps`, `Stratifier`, `PatMatch`, `Redundancy`, `Safety`,
  *     `Terminator`, `Dependencies`.
  *   - Backend (16 sites):
  *     `Monomorpher` (initial + worklist; keyed by source generic sym),
  *     `LambdaDrop`, `OccurrenceAnalyzer`, `Inliner`, `Simplifier`,
  *     `ClosureConv`, `LambdaLift`, `EffectBinder`, `TailPos`, `Eraser`,
  *     `Reducer`, `JvmBackend` (closure + control-pure function +
  *     control-impure function cases in `GenFunAndClosureClasses`).
  *
  * == Uninstrumented phases ==
  *
  * These phases run but do not feed the timer:
  *
  *   - `Reader`, `Lexer`, `Parser2`, `Weeder2`, `Desugar`, `Namer` —
  *     pre-`DefnSym`; the symbol does not exist yet.
  *   - `Resolver` — per-def work is nested inside trait/instance walks
  *     rather than a clean `parMapValues` over `root.defs`.
  *   - `Deriver` — generates new instances from enum derivations; iterates
  *     over enums, not over existing defs.
  *   - `Instances` — per-instance-def work is interleaved with trait
  *     conformance checks; instrumentable but not a one-line wrap.
  *   - `TreeShaker1`, `TreeShaker2` — pure filter passes; no per-def work
  *     to time.
  *   - `Optimizer` — a wrapper that re-runs `OccurrenceAnalyzer` and
  *     `Inliner` in a fixed-point loop. All work is captured transitively
  *     through those instrumented inner phases.
  *   - `JvmBackend` — top-level orchestration (namespace classes, runtime
  *     types, etc.) is not per-def. The dominant per-def work — function
  *     and closure class generation in `GenFunAndClosureClasses` — *is*
  *     instrumented (see Backend list above).
  */
final class CompilerProfiler(phaseProvider: () => Option[String]) {

  private val stats = new ConcurrentHashMap[Symbol.DefnSym, CompilerProfiler.Counters]()

  /**
    * Runs `thunk` and records its elapsed wall-clock time against `sym`,
    * attributed to whatever phase is currently running. If no phase is
    * active (e.g. before the first phase has started), the time is
    * bucketed under `"?"` to match the unknown-phase label used by the
    * renderer.
    */
  def track[A](sym: Symbol.DefnSym, loc: SourceLocation)(thunk: => A): A = {
    val phase = phaseProvider().getOrElse("?")
    val t0 = System.nanoTime()
    try thunk
    finally {
      val elapsed = System.nanoTime() - t0
      val c = countersFor(sym)
      c.loc.compareAndSet(null, loc)
      c.totalNanos.addAndGet(elapsed)
      c.callCount.incrementAndGet()
      c.perPhaseNanos
        .computeIfAbsent(phase, _ => new AtomicLong(0L))
        .addAndGet(elapsed)
    }
  }

  /**
    * Returns an approximate snapshot. Iterators over the underlying maps are
    * weakly consistent, so individual entries may be slightly stale.
    */
  def snapshot(): Vector[DefnStats] = {
    val it = stats.entrySet().iterator().asScala
    it.map { e =>
      val c = e.getValue
      val byPhase = c.perPhaseNanos.entrySet().iterator().asScala
        .map(pe => (pe.getKey, pe.getValue.get())).toMap
      val loc = Option(c.loc.get()).getOrElse(e.getKey.loc)
      DefnStats(e.getKey, loc, c.totalNanos.get(), c.callCount.get(), byPhase)
    }.toVector
  }

  /** Returns the total number of `track` calls made so far across all defs. */
  def totalCallCount: Long = {
    var n = 0L
    val it = stats.values().iterator()
    while (it.hasNext) n += it.next().callCount.get()
    n
  }

  private def countersFor(sym: Symbol.DefnSym): CompilerProfiler.Counters =
    stats.computeIfAbsent(sourceOf(sym), _ => new CompilerProfiler.Counters)

  /**
    * Returns the source-equivalent sym for `sym`: same `(namespace, text,
    * loc)` triple but with `id = None`, i.e. the sym a user would have
    * written. A sym that is already a source sym (`id.isEmpty`) is returned
    * unchanged.
    *
    * Correct only as long as [[Symbol.freshDefnSym]] keeps the triple
    * identical to its argument; if a future change starts varying any of
    * those three, the roll-up here would silently cross-attribute time
    * between unrelated defs.
    */
  private def sourceOf(sym: Symbol.DefnSym): Symbol.DefnSym =
    if (sym.id.isEmpty) sym
    else new Symbol.DefnSym(None, sym.namespace, sym.text, sym.loc)
}

object CompilerProfiler {
  private final class Counters {
    val totalNanos = new AtomicLong(0L)
    val callCount = new AtomicInteger(0)
    val perPhaseNanos = new ConcurrentHashMap[String, AtomicLong]()
    /** Set on first `track` call; carries the def-body span so we can show LOC. */
    val loc = new AtomicReference[SourceLocation](null)
  }
}
