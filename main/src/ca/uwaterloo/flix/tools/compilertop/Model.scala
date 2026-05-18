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
    * Restricts which phases' time the dashboard accounts for. Toggled
    * interactively via the input thread (`f` / `b` / `a`, plus `u` to open
    * the custom-selection picker). The split tracks `Flix.check` (frontend)
    * vs the phases that follow in `Flix.compile`'s `codeGen` (backend) — see
    * [[FrontendPhases]].
    *
    * `label` is the legend word; `key` is the keystroke. By convention
    * `key = label.head` for All / Frontend / Backend; [[Custom]] uses `'u'`
    * because `'c'` is already taken by [[Sort.Cls]] (the input loop fans
    * every key into both ADTs, so a shared letter would change both).
    */
  sealed trait PhaseFilter {
    def label: String
    def key: Char
  }
  object PhaseFilter {
    case object All      extends PhaseFilter { val label = "all";      val key = 'a' }
    case object Frontend extends PhaseFilter { val label = "frontend"; val key = 'f' }
    case object Backend  extends PhaseFilter { val label = "backend";  val key = 'b' }

    /**
      * User-curated subset of phases. The renderer treats the underlined
      * keystroke as `u` (not the first letter of the label) — see [[key]].
      * `selected` is empty by default; the input loop replaces the whole
      * filter atomically when the picker is committed.
      */
    final case class Custom(selected: Set[String]) extends PhaseFilter {
      val label = "custom"
      val key = 'u'
    }

    /**
      * Filter constructors in legend display order. The [[Custom]] entry is
      * a sentinel used only for the legend's keystroke / label — the active
      * filter (with its real `selected` set) lives in
      * [[CompilerTop]]'s `filter` atomic.
      */
    val all: List[PhaseFilter] = List(All, Frontend, Backend, Custom(Set.empty))

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
    * Phase-name strings that count as "backend" — phases that run inside
    * `Flix.compile`'s `codeGen` pipeline. Strings must match the literals each
    * phase passes to `flix.phase("…")` / `flix.phaseNew("…")`. Verified against
    * the instrumentation table in [[Profiler]]'s scaladoc.
    *
    * Disjoint from [[FrontendPhases]]; together they enumerate the universe of
    * named phases the custom-filter picker can list. Phases not in either set
    * (and not the fallback `"?"`) are ignored by the picker — but kept in the
    * profiler snapshot — so a renamed phase shows up as a missing entry in
    * the picker rather than silently disappearing from the dashboard.
    */
  val BackendPhases: Set[String] = Set(
    "TreeShaker1", "Monomorpher", "LambdaDrop", "Optimizer", "OccurrenceAnalyzer",
    "Inliner", "Simplifier", "ClosureConv", "LambdaLift", "TreeShaker2",
    "EffectBinder", "TailPos", "Eraser", "Reducer", "CodeGen",
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
    /** Most optimizer fixed-point re-visits first — surfaces inliner / occurrence-analyzer thrashing. */
    case object Opt     extends Sort { val key = 'o' }
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
    val all: List[Sort] = List(Time, Hotness, Mono, Opt, Inl, Cls, Size, Alloc, Cns, Tvars, Evars)

    /** The sort whose `key` matches `c` (case-insensitive), or `None`. */
    def fromKey(c: Char): Option[Sort] = all.find(_.key == c.toLower)
  }

  /** Phases whose `track` count maps to the `mono` column — number of monomorphic instances created. */
  val MonoCountPhases: Set[String] = Set("Monomorpher")
  /** Phases whose `track` count maps to the `opt` column — optimizer fixed-point re-visits. */
  val OptCountPhases: Set[String] = Set("Optimizer", "LambdaDrop")
  /** Phases whose `track` count maps to the `cls` column — class files emitted post-specialization. */
  val ClsCountPhases: Set[String] = Set("CodeGen")

  /**
    * A row in the per-module aggregate table.
    *
    * @param module         dot-joined namespace (or `(root)` when empty).
    * @param totalNanos     summed wall-clock time across the module's defs.
    * @param totalCallCount summed `track` call counts across the module's defs.
    * @param totalLocLines  summed source-line counts across the module's defs.
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
  final case class ModuleStats(module: String, totalNanos: Long, totalCallCount: Long, totalLocLines: Int, byPhase: Map[String, Long], byPhaseCount: Map[String, Long], byPhaseAlloc: Map[String, Long], totalCns: Long, totalTvars: Long, totalEvars: Long, totalInlined: Long, totalClassBytes: Long, totalAllocBytes: Long) {
    /** Returns the phase that consumed the most time in this module, or None if empty. */
    def dominantPhase: Option[String] =
      if (byPhase.isEmpty) None else Some(byPhase.maxBy(_._2)._1)
  }

  /**
    * Which of [[FrontendPhases]] / [[BackendPhases]] the cursor is focused
    * on in the picker view. Left/right arrows toggle focus; up/down move
    * the cursor inside the focused list.
    */
  sealed trait PickerColumn
  object PickerColumn {
    case object Frontend extends PickerColumn
    case object Backend  extends PickerColumn
  }

  /**
    * Active screen for the TUI. CompilerTop swaps the [[View]] reference on
    * `u` (enter picker), Enter / Esc (leave picker), and the renderer
    * dispatches one frame layout vs the other from a single field on
    * [[Renderer.FrameState]].
    *
    * The picker keeps its working selection on the `View` itself rather than
    * mutating the active `PhaseFilter.Custom`. Esc therefore discards the
    * draft cleanly and the dashboard's column-set doesn't shift while the
    * user is curating.
    */
  sealed trait View
  object View {
    case object Main extends View

    /**
      * Picker screen state.
      *
      * @param column   which list the cursor is inside
      * @param frontendCursor 0-based row in the sorted Frontend list
      * @param backendCursor  0-based row in the sorted Backend list
      * @param selected the working set of phase names — committed to
      *                 [[PhaseFilter.Custom.selected]] on Enter, discarded on Esc
      */
    final case class Picker(column: PickerColumn, frontendCursor: Int, backendCursor: Int, selected: Set[String]) extends View
  }
}
