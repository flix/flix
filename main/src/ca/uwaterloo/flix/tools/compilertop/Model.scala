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

/**
  * Pure data types and classification constants shared across the
  * compiler-top TUI: the filter / sort modes, the per-module row, the
  * column-layout descriptor, and the phase-name sets that drive each.
  */
object Model {

  /**
    * Which top-level screen the renderer should draw. Switched by `Tab`
    * (flip table), `?` (help), and `Esc` (back to defs) in the input thread.
    * The dashboard and stats lines render the same in every view; only the
    * body below them changes.
    *
    *   - [[View.Defs]]: the per-def table (default), full terminal height.
    *   - [[View.Modules]]: the per-module aggregate table, full terminal
    *     height. `Tab` cycles into this from [[View.Defs]].
    *   - [[View.Phases]]: the per-phase aggregate table (phase name, time,
    *     alloc, par, %cpu, %wall, blind, %observed). `Tab` cycles into this
    *     from [[View.Modules]].
    *   - [[View.Help]]: a static legend explaining each column and
    *     keystroke, so the user doesn't have to guess what e.g. `tv` or
    *     `rnd` mean.
    */
  sealed trait View {
    /**
      * The next table view in the `Tab` cycle:
      * [[View.Defs]] → [[View.Modules]] → [[View.Phases]] → [[View.Defs]].
      * From [[View.Help]] there is no "next table", so it maps to itself;
      * the input loop special-cases leaving help separately.
      */
    final def toggledTable: View = this match {
      case View.Defs    => View.Modules
      case View.Modules => View.Phases
      case View.Phases  => View.Defs
      case View.Help    => View.Help
    }
  }
  object View {
    case object Defs    extends View
    case object Modules extends View
    case object Phases  extends View
    case object Help    extends View
  }

  /**
    * Restricts which phases' time the dashboard accounts for. Toggled
    * interactively via the input thread (`f` / `b` / `a`). The split tracks
    * `Flix.check` (frontend) vs the phases that follow in `Flix.compile`'s
    * `codeGen` (backend) — see [[FrontendPhases]].
    *
    * `label` is the legend word; `key` is the keystroke that selects this
    * filter (by convention the first character of `label`). The renderer
    * derives the legend from these fields and the input loop dispatches via
    * [[PhaseFilter.fromKey]] — neither encodes the mapping inline.
    */
  sealed trait PhaseFilter {
    def label: String
    final def key: Char = label.head
  }
  object PhaseFilter {
    case object All      extends PhaseFilter { val label = "all" }
    case object Frontend extends PhaseFilter { val label = "frontend" }
    case object Backend  extends PhaseFilter { val label = "backend" }

    /** All filter values in legend display order. */
    val all: List[PhaseFilter] = List(All, Frontend, Backend)

    /** The filter whose `key` matches `c` (case-insensitive), or `None`. */
    def fromKey(c: Char): Option[PhaseFilter] = all.find(_.key == c.toLower)
  }

  /**
    * Phase-name strings that count as "frontend" — i.e. phases that run
    * inside `Flix.check`. Strings must match the literals each phase passes
    * to `flix.phase("…")` / `flix.phaseNew("…")`. Verified against the
    * `run` methods under `main/src/ca/uwaterloo/flix/language/phase`.
    *
    * Phases not in this set (and not the unknown-phase fallback `"?"`) are
    * treated as backend.
    */
  val FrontendPhases: Set[String] = Set(
    "Reader", "Lexer", "Parser2", "Weeder2", "Desugar", "Namer", "Resolver",
    "Kinder", "Deriver", "Typer", "EntryPoints", "Instances", "PredDeps",
    "Stratifier", "PatMatch", "Redundancy", "Safety", "Terminator", "Dependencies",
  )

  /**
    * Phases that inherently have no per-`DefnSym` unit to attribute time to, so
    * the dashboard coverage line excludes them from the accounted / unaccounted
    * split rather than reporting their wall time as a (false) closeable blind
    * spot. Three structural reasons, mirroring the coverage table in
    * [[Profiler]]:
    *
    *   - Pre-naming passes (`Reader`, `Lexer`, `Parser2`, `Weeder2`, `Desugar`,
    *     `Namer`) run before any `DefnSym` exists, so there is no key to attribute.
    *   - Pure filter passes (`TreeShaker1`, `TreeShaker2`) do no per-def work.
    *   - The `Optimizer` fixpoint wrapper does no work of its own; its time is
    *     captured transitively through the `OccurrenceAnalyzer` / `Inliner` phases
    *     it drives, so counting it here would be redundant.
    *
    * Phases that *could* be attributed but currently aren't (`Resolver`,
    * `Deriver`, `Instances`) are deliberately NOT in this set — they are real,
    * closeable blind spots and belong in the unaccounted tally.
    */
  val NonAttributablePhases: Set[String] = Set(
    "Reader", "Lexer", "Parser2", "Weeder2", "Desugar", "Namer",
    "TreeShaker1", "Optimizer", "TreeShaker2",
  )

  /**
    * Selects the descending sort key applied to the def and module tables.
    * Each option surfaces a different kind of suspect, so the same def can
    * top one ranking and not another.
    *
    * `key` is the keystroke that selects this sort. Stored lowercase; the
    * input loop normalizes via `c.toLower` before dispatching. The renderer
    * also reads `key` when deciding which character in a column header to
    * underline — adding a new sort needs only a single line here and one
    * `sortableColumn(...)` call in the renderer.
    */
  sealed trait Sort { def key: Char }
  object Sort {
    /** Slowest defs first. The default. */
    case object Time    extends Sort { val key = 't' }
    /** Hottest-per-line defs first — small defs that burn disproportionate time. */
    case object Hotness extends Sort { val key = 'h' }
    /** Most monomorphic instances first — surfaces generic-explosion hotspots. */
    case object Mono    extends Sort { val key = 'm' }
    /** Most times inlined at a call site first — surfaces small leaf defs that get duplicated everywhere. */
    case object Inl     extends Sort { val key = 'i' }
    /** Most class files emitted first — surfaces JVM fan-out from polymorphism / closures. */
    case object Cls     extends Sort { val key = 'c' }
    /** Largest total `.class` byte size first — surfaces defs whose bodies compile to bulky bytecode. */
    case object Size    extends Sort { val key = 's' }
    /** Most heap bytes allocated by the compiler while processing this def — surfaces compiler-side data-structure blow-up (e.g. unification trails, IR duplication) independent of wall time. */
    case object Alloc   extends Sort { val key = 'l' }
    /** Most type constraints first — surfaces type-checking-heavy defs. The key is `n` (not `c`, which is taken by [[Cls]]). */
    case object Cns     extends Sort { val key = 'n' }
    /** Most type variables first — surfaces broadly polymorphic defs. */
    case object Tvars   extends Sort { val key = 'v' }
    /** Most effect variables first — surfaces effect-polymorphic defs. */
    case object Evars   extends Sort { val key = 'e' }

    /** All sort values. */
    val all: List[Sort] = List(Time, Hotness, Mono, Inl, Cls, Size, Alloc, Cns, Tvars, Evars)

    /** The sort whose `key` matches `c` (case-insensitive), or `None`. */
    def fromKey(c: Char): Option[Sort] = all.find(_.key == c.toLower)
  }

  /** Phases whose `track` count maps to the `mono` column — number of monomorphic instances created. */
  val MonoCountPhases: Set[String] = Set("Monomorpher")
  /** Phases whose `track` count maps to the `cls` column — class files emitted post-specialization. */
  val ClsCountPhases: Set[String] = Set("CodeGen")

  /**
    * A row in the per-module aggregate table.
    *
    * @param module         dot-joined namespace (or `(root)` when empty).
    * @param totalNanos     summed wall-clock time across the module's defs.
    * @param totalLines     summed source-line counts across the module's defs.
    * @param byPhase        phase → summed nanoseconds across the module's defs.
    * @param byPhaseCount   phase → summed track-call counts across the module's defs.
    * @param byPhaseAlloc   phase → summed compiler-side allocation bytes across the module's defs. Lets the frontend/backend filter re-project module allocation the same way it re-projects module time.
    * @param totalCns       summed Typer constraint counts across the module's defs.
    * @param totalTvars     summed `Kind.Star` type-variable counts across the module's defs.
    * @param totalEvars     summed `Kind.Eff`  effect-variable counts across the module's defs.
    * @param totalInlined   summed inliner call-site inline counts across the module's defs.
    * @param totalClassBytes summed bytecode byte size of emitted `.class` files across the module's defs.
    * @param totalAllocBytes summed compiler-side heap allocation bytes across the module's defs.
    */
  final case class ModuleStats(module: String, totalNanos: Long, totalLines: Int, byPhase: Map[String, Long], byPhaseCount: Map[String, Long], byPhaseAlloc: Map[String, Long], totalCns: Long, totalTvars: Long, totalEvars: Long, totalInlined: Long, totalClassBytes: Long, totalAllocBytes: Long) {
    /** Returns the phase that consumed the most time in this module, or None if empty. */
    def dominantPhase: Option[String] =
      if (byPhase.isEmpty) None else Some(byPhase.maxBy(_._2)._1)
  }

  /**
    * A row in the per-phase aggregate table. One per phase that ran (driven by
    * `Flix.phaseTimers`, so non-attributable phases like `Lexer` / `Parser2`
    * appear too, with `0` observed). The renderer derives `%cpu` / `%wall`
    * against the per-frame `elapsed` / `parallelism`, the same late-binding the
    * def / module rows use for their percentages.
    *
    * @param phase               the phase name (as passed to `flix.phase(...)`).
    * @param wallNanos           real phase wall time, summed across repeats, from `Flix.phaseTimers`.
    * @param threadSummedNanos   thread-summed per-def time attributed to this phase (`Σ DefnStats.byPhase`).
    * @param allocBytes          compiler-side heap bytes attributed to this phase (`Σ DefnStats.byPhaseAlloc`).
    * @param attributedWallNanos estimated attributed wall slice: `(threadSummed / parallelism).min(wall)`, mirroring [[Coverage]].
    */
  final case class PhaseStats(phase: String, wallNanos: Long, threadSummedNanos: Long, allocBytes: Long, attributedWallNanos: Long) {
    /** Share of this phase's wall time the per-def table attributes (the per-phase `observed` figure). */
    def pctObserved: Double =
      if (wallNanos <= 0L) 0.0 else 100.0 * attributedWallNanos / wallNanos
  }

  /**
    * Phase-level wall-time reconciliation backing the dashboard `observed` figure.
    *
    * `Flix.phaseTimers` records each phase's wall time. For each phase we
    * estimate the slice the per-def table attributes by spreading its
    * thread-summed per-def time across the available threads:
    * `attributedWall ≈ threadSummed / parallelism`, clamped to the phase wall.
    * `accounted` sums those estimates; `unaccounted` is the remaining wall.
    *
    * The estimate *optimistically assumes full parallelism*. For a saturated
    * parallel phase (`threadSummed ≈ parallelism × wall`) it recovers the true
    * attributed wall, so a partially-instrumented parallel phase now reads as
    * partially covered rather than fully covered. The flip side: a sequential
    * phase ran on one thread yet is still divided by `parallelism`, so it is
    * under-counted and reads as a partial blind spot even when fully
    * instrumented — the unit mix (thread-summed ÷ threads vs single-threaded
    * wall) is only exact at the parallel extreme.
    *
    * Phases in [[NonAttributablePhases]] are excluded entirely: they have no
    * per-def unit by construction, so the denominator is "attributable phase
    * wall time".
    *
    * @param accountedNanos   summed estimated attributed wall across phases.
    * @param unaccountedNanos summed remaining (unattributed) wall across phases.
    */
  final case class Coverage(accountedNanos: Long, unaccountedNanos: Long)
}
