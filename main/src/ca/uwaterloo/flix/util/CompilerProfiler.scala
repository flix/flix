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
  * Per-`DefnSym` compile-time profiler.
  *
  * [[CompilerProfiler]] tracks cumulative wall-clock time, call counts, and a
  * per-phase breakdown for each [[Symbol.DefnSym]] the compiler visits. Its
  * [[CompilerProfiler.track]] entry point is called from inside instrumented
  * phases and is safe to invoke from multiple threads.
  *
  * A fresh sym minted from an existing one via [[Symbol.freshDefnSym]] is
  * folded back to its source sym (see [[CompilerProfiler.sourceOf]]) so the
  * user-facing table shows one row per source-level definition rather than
  * one row per refresh.
  *
  * Instrumentation coverage of the compiler pipeline:
  *
  * | Phase              | Instrumented | Reason                                                                                                        |
  * | ------------------ | ------------ | ------------------------------------------------------------------------------------------------------------- |
  * | Reader             | No           | Runs before any `DefnSym` exists, so there is no key to attribute time to.                                    |
  * | Lexer              | No           | -- same --                                                                                                    |
  * | Parser2            | No           | -- same --                                                                                                    |
  * | Weeder2            | No           | -- same --                                                                                                    |
  * | Desugar            | No           | -- same --                                                                                                    |
  * | Namer              | No           | -- same --                                                                                                    |
  * | Resolver           | No           | Per-def work is nested inside trait/instance walks rather than a clean `parMapValues` over `root.defs`.       |
  * | Deriver            | No           | Generates new instances from enum derivations; iterates over enums, not existing defs.                        |
  * | Instances          | No           | Per-instance-def work interleaves with trait conformance checks; instrumentable but not a one-line wrap.      |
  * | Kinder             | Yes          | `visitDefSpecs` + `visitDefs`.                                                                                |
  * | Typer              | Yes          |                                                                                                               |
  * | EntryPoints        | Yes          |                                                                                                               |
  * | PredDeps           | Yes          |                                                                                                               |
  * | Stratifier         | Yes          |                                                                                                               |
  * | PatMatch           | Yes          |                                                                                                               |
  * | Redundancy         | Yes          |                                                                                                               |
  * | Safety             | Yes          |                                                                                                               |
  * | Terminator         | Yes          |                                                                                                               |
  * | Dependencies       | Yes          |                                                                                                               |
  * | TreeShaker1        | No           | Pure filter pass; no per-def work to time.                                                                    |
  * | Monomorpher        | Yes          | Initial + worklist, keyed by the source generic sym.                                                          |
  * | LambdaDrop         | Yes          |                                                                                                               |
  * | Optimizer          | No           | Fixed-point wrapper around `OccurrenceAnalyzer` / `Inliner`; captured transitively through those inner phases.|
  * | OccurrenceAnalyzer | Yes          |                                                                                                               |
  * | Inliner            | Yes          |                                                                                                               |
  * | Simplifier         | Yes          |                                                                                                               |
  * | ClosureConv        | Yes          |                                                                                                               |
  * | LambdaLift         | Yes          |                                                                                                               |
  * | TreeShaker2        | No           | -- same as TreeShaker1 --                                                                                     |
  * | EffectBinder       | Yes          |                                                                                                               |
  * | TailPos            | Yes          |                                                                                                               |
  * | Eraser             | Yes          |                                                                                                               |
  * | Reducer            | Yes          |                                                                                                               |
  * | JvmBackend         | Partial      | Only the per-def cases in `GenFunAndClosureClasses` are instrumented; top-level orchestration is not.         |
  */
object CompilerProfiler {
  /**
    * A snapshot of per-`DefnSym` statistics produced by [[CompilerProfiler.snapshot]].
    *
    * @param sym        the source-level [[Symbol.DefnSym]] this row aggregates.
    * @param isActive   true if at least one `track` call is currently in flight for this sym.
    * @param callCount  number of `track` calls observed for this sym.
    * @param totalNanos total wall-clock time spent inside `track` calls for this sym, in nanoseconds.
    * @param byPhase    per-phase breakdown of `totalNanos`, keyed by phase name.
    * @param loc        the source location of the def's body, used to compute LOC.
    */
  final case class DefnStats(sym: Symbol.DefnSym, isActive: Boolean, callCount: Int, totalNanos: Long, byPhase: Map[String, Long], loc: SourceLocation) {
    /** Returns the phase that consumed the most time, or None if empty. */
    def dominantPhase: Option[String] =
      if (byPhase.isEmpty) None else Some(byPhase.maxBy(_._2)._1)
  }

  /**
    * Mutable per-`DefnSym` accumulators feeding [[DefnStats]].
    *
    * @param inProgress    number of `track` calls currently in flight for this source sym.
    * @param callCount     number of `track` calls observed for this sym.
    * @param totalNanos    total wall-clock time spent inside `track` calls for this sym, in nanoseconds.
    * @param perPhaseNanos per-phase nanosecond accumulator, keyed by phase name.
    * @param loc           set on first `track` call; carries the def-body span so we can show LOC.
    */
  private final case class Counters(inProgress: AtomicInteger, callCount: AtomicInteger, totalNanos: AtomicLong, perPhaseNanos: ConcurrentHashMap[String, AtomicLong], loc: AtomicReference[SourceLocation])
}

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
    val c = countersFor(sym)
    c.loc.compareAndSet(null, loc)
    c.inProgress.incrementAndGet()
    val t0 = System.nanoTime()
    try thunk
    finally {
      val elapsed = System.nanoTime() - t0
      c.totalNanos.addAndGet(elapsed)
      c.callCount.incrementAndGet()
      c.perPhaseNanos
        .computeIfAbsent(phase, _ => new AtomicLong(0L))
        .addAndGet(elapsed)
      c.inProgress.decrementAndGet()
    }
  }

  /**
    * Returns an approximate snapshot. Iterators over the underlying maps are
    * weakly consistent, so individual entries may be slightly stale.
    */
  def snapshot(): Vector[CompilerProfiler.DefnStats] = {
    val it = stats.entrySet().iterator().asScala
    it.map { e =>
      val c = e.getValue
      val byPhase = c.perPhaseNanos.entrySet().iterator().asScala
        .map(pe => (pe.getKey, pe.getValue.get())).toMap
      val loc = Option(c.loc.get()).getOrElse(e.getKey.loc)
      CompilerProfiler.DefnStats(e.getKey, isActive = c.inProgress.get() > 0,
        c.callCount.get(), c.totalNanos.get(), byPhase, loc)
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
    stats.computeIfAbsent(sourceOf(sym), _ => CompilerProfiler.Counters(
      inProgress = new AtomicInteger(0),
      callCount = new AtomicInteger(0),
      totalNanos = new AtomicLong(0L),
      perPhaseNanos = new ConcurrentHashMap[String, AtomicLong](),
      loc = new AtomicReference[SourceLocation](null)
    ))

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

