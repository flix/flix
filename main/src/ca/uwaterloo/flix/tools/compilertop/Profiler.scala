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

import ca.uwaterloo.flix.api.{FlixEvent, FlixListener}
import ca.uwaterloo.flix.language.ast.{Kind, SourceLocation, Symbol, Type}
import ca.uwaterloo.flix.language.phase.typer.TypeConstraint

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.{AtomicInteger, AtomicLong, AtomicReference}
import scala.jdk.CollectionConverters.*

/**
  * Per-`DefnSym` compile-time profiler.
  *
  * [[Profiler]] tracks cumulative wall-clock time, call counts, and a
  * per-phase breakdown for each [[Symbol.DefnSym]] the compiler visits. Its
  * [[Profiler.track]] entry point is called from inside instrumented
  * phases and is safe to invoke from multiple threads.
  *
  * A fresh sym minted from an existing one via [[Symbol.freshDefnSym]] is
  * folded back to its source sym (see [[Profiler.sourceOf]]) so the
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
  * | CodeGen            | Partial      | Only the per-def cases in `GenFunAndClosureClasses` are instrumented; top-level orchestration is not.         |
  */
object Profiler {
  /**
    * A snapshot of per-`DefnSym` statistics produced by [[Profiler.snapshot]].
    *
    * @param sym          the source-level [[Symbol.DefnSym]] this row aggregates.
    * @param isActive     true if at least one `track` call is currently in flight for this sym.
    * @param callCount    number of `track` calls observed for this sym.
    * @param totalNanos   total wall-clock time spent inside `track` calls for this sym, in nanoseconds.
    * @param byPhase      per-phase breakdown of `totalNanos`, keyed by phase name.
    * @param byPhaseCount per-phase breakdown of `callCount`, keyed by phase name.
    * @param cns          number of type constraints generated by Typer for this def's body, from the most recent typing pass.
    * @param tvars        number of distinct `Kind.Star` type variables present in the constraint system for this def, from the most recent typing pass.
    * @param evars        number of distinct `Kind.Eff` effect variables present in the constraint system for this def, from the most recent typing pass.
    * @param inlined      number of times this def was inlined at a call site by the inliner across all optimizer rounds.
    * @param loc          the source location of the def's body, used to compute LOC.
    */
  final case class DefnStats(sym: Symbol.DefnSym, isActive: Boolean, callCount: Int, totalNanos: Long, byPhase: Map[String, Long], byPhaseCount: Map[String, Long], cns: Long, tvars: Long, evars: Long, inlined: Long, loc: SourceLocation) {
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
    * @param perPhaseCount per-phase call-count accumulator, keyed by phase name.
    * @param constraints   type-constraint count from the most recent `FlixEvent.NewConstraintsDef`. Set (not accumulated) — a re-typing replaces the old value rather than doubling it.
    * @param tvars         distinct `Kind.Star` type variables from the most recent constraint system.
    * @param evars         distinct `Kind.Eff` effect variables from the most recent constraint system.
    * @param inlined       accumulator: number of times this def was inlined at a call site by the inliner.
    * @param loc           set on first `track` call; carries the def-body span so we can show LOC.
    */
  private final case class Counters(inProgress: AtomicInteger, callCount: AtomicInteger, totalNanos: AtomicLong, perPhaseNanos: ConcurrentHashMap[String, AtomicLong], perPhaseCount: ConcurrentHashMap[String, AtomicLong], constraints: AtomicLong, tvars: AtomicLong, evars: AtomicLong, inlined: AtomicLong, loc: AtomicReference[SourceLocation])

  /**
    * Composite map key for per-`DefnSym` accumulators.
    *
    * Why this exists: `Symbol.DefnSym.equals` (Symbol.scala) compares
    * `(id, namespace, text)` and intentionally ignores `loc`. That's fine for
    * most of the compiler — fresh syms produced by `freshDefnSym` (e.g. during
    * specialization) keep the source `(namespace, text, loc)` triple, and
    * after `sourceOf` strips `id` they collapse onto one source-sym. But
    * Namer also assigns fresh ids to *instance* defs to distinguish them
    * within a shared namespace (Namer.scala:797–802). Those instance methods
    * all share `(id=None after sourceOf, namespace, text)` and only differ
    * by `loc`, so they end up `equals` to each other and aggregate into a
    * single bucket — meaning every `Eq.T.eq` collapses onto one row.
    *
    * Using a case-class key whose own `equals` consults `loc` keeps each
    * source-line worth of work in its own bucket without touching
    * `DefnSym.equals` itself (which is used in many other places).
    */
  private final case class DefnSymWithLoc(sym: Symbol.DefnSym, loc: SourceLocation)

  /**
    * Returns `(tvars, evars)` — the number of distinct `Kind.Star` and
    * `Kind.Eff` variables present anywhere in the given constraint system.
    * The walk uses a `HashSet[Type.Var]` so a variable referenced from many
    * constraints counts once.
    *
    * Other kinds (`Kind.Bool`, `Kind.SchemaRow`, arrows, …) are ignored —
    * they're rarer per-def and not surfaced as columns today.
    */
  private def countVars(cs: List[TypeConstraint]): (Long, Long) = {
    val vars = scala.collection.mutable.HashSet.empty[Type.Var]
    def collect(c: TypeConstraint): Unit = c match {
      case TypeConstraint.Equality(t1, t2, _)         => vars ++= t1.typeVars; vars ++= t2.typeVars
      case TypeConstraint.Trait(_, t, _)              => vars ++= t.typeVars
      case TypeConstraint.Conflicted(t1, t2, _)       => vars ++= t1.typeVars; vars ++= t2.typeVars
      case TypeConstraint.Purification(_, e1, e2, _, nested) =>
        vars ++= e1.typeVars; vars ++= e2.typeVars
        nested.foreach(collect)
      case TypeConstraint.EffConflicted(_)            => ()
    }
    cs.foreach(collect)
    var tv = 0L
    var ev = 0L
    vars.foreach { v =>
      v.kind match {
        case Kind.Star => tv += 1L
        case Kind.Eff  => ev += 1L
        case _         => ()
      }
    }
    (tv, ev)
  }
}

final class Profiler(phaseProvider: () => Option[String]) extends FlixListener {

  /** Per-source-`DefnSym` accumulators, keyed by [[Profiler.DefnSymWithLoc]] so that defs at distinct locations don't aggregate. */
  private val stats = new ConcurrentHashMap[Profiler.DefnSymWithLoc, Profiler.Counters]()

  /**
    * Subscribes the profiler to compiler events emitted via `Flix.emitEvent`.
    * Today only [[FlixEvent.NewConstraintsDef]] is consumed — Typer fires it
    * once per def with the post-generation constraint list, which gives us
    * a clean per-def `cns` reading without instrumenting any inner loops.
    */
  override def notify(e: FlixEvent): Unit = e match {
    case FlixEvent.NewConstraintsDef(sym, tconstrs) =>
      val c = countersFor(sym)
      // `set` (not accumulate): a re-typing of the same def replaces the old
      // reading rather than doubling it. The same body should produce the same
      // constraints / variables, and an incremental rebuild that re-types the
      // def shouldn't make the column appear to grow.
      c.constraints.set(tconstrs.size.toLong)
      val (tv, ev) = Profiler.countVars(tconstrs)
      c.tvars.set(tv)
      c.evars.set(ev)
    case FlixEvent.InlinedDef(sym) =>
      countersFor(sym).inlined.incrementAndGet()
    case _ => ()
  }

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
      c.perPhaseCount
        .computeIfAbsent(phase, _ => new AtomicLong(0L))
        .incrementAndGet()
      c.inProgress.decrementAndGet()
    }
  }

  /**
    * Returns an approximate snapshot. Iterators over the underlying maps are
    * weakly consistent, so individual entries may be slightly stale.
    */
  def snapshot(): Vector[Profiler.DefnStats] = {
    val it = stats.entrySet().iterator().asScala
    it.map { e =>
      val key = e.getKey
      val c = e.getValue
      val byPhase = c.perPhaseNanos.entrySet().iterator().asScala
        .map(pe => (pe.getKey, pe.getValue.get())).toMap
      val byPhaseCount = c.perPhaseCount.entrySet().iterator().asScala
        .map(pe => (pe.getKey, pe.getValue.get())).toMap
      val loc = Option(c.loc.get()).getOrElse(key.loc)
      Profiler.DefnStats(key.sym, isActive = c.inProgress.get() > 0,
        c.callCount.get(), c.totalNanos.get(), byPhase, byPhaseCount,
        c.constraints.get(), c.tvars.get(), c.evars.get(), c.inlined.get(), loc)
    }.toVector
  }

  /**
    * Returns the [[Counters]] entry for `sym`'s source sym, creating a fresh
    * zero-initialized entry on first use. Atomic in the underlying map.
    */
  private def countersFor(sym: Symbol.DefnSym): Profiler.Counters =
    stats.computeIfAbsent(Profiler.DefnSymWithLoc(sourceOf(sym), sym.loc), _ => Profiler.Counters(
      inProgress = new AtomicInteger(0),
      callCount = new AtomicInteger(0),
      totalNanos = new AtomicLong(0L),
      perPhaseNanos = new ConcurrentHashMap[String, AtomicLong](),
      perPhaseCount = new ConcurrentHashMap[String, AtomicLong](),
      constraints = new AtomicLong(0L),
      tvars = new AtomicLong(0L),
      evars = new AtomicLong(0L),
      inlined = new AtomicLong(0L),
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

