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
    * interactively via the input thread (`f` / `b` / `a`). The split tracks
    * `Flix.check` (frontend) vs the phases that follow in `Flix.compile`'s
    * `codeGen` (backend) — see [[FrontendPhases]].
    */
  sealed trait PhaseFilter
  object PhaseFilter {
    case object All extends PhaseFilter
    case object Frontend extends PhaseFilter
    case object Backend extends PhaseFilter
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
    * Selects the descending sort key applied to the def and module tables.
    * Each option surfaces a different kind of suspect, so the same def can
    * top one ranking and not another.
    */
  sealed trait Sort
  object Sort {
    /** Slowest defs first. The default. */
    case object Time extends Sort
    /** Hottest-per-line defs first — small defs that burn disproportionate time. */
    case object Hotness extends Sort
    /** Most monomorphic instances first — surfaces generic-explosion hotspots. */
    case object Mono extends Sort
    /** Most optimizer fixed-point re-visits first — surfaces inliner / occurrence-analyzer thrashing. */
    case object Opt extends Sort
    /** Most class files emitted first — surfaces JVM fan-out from polymorphism / closures. */
    case object Cls extends Sort
    /** Most type constraints first — surfaces type-checking-heavy defs. */
    case object Cns extends Sort
    /** Most type variables first — surfaces broadly polymorphic defs. */
    case object Tvars extends Sort
    /** Most effect variables first — surfaces effect-polymorphic defs. */
    case object Evars extends Sort
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
    * @param totalCns       summed Typer constraint counts across the module's defs.
    * @param totalTvars     summed `Kind.Star` type-variable counts across the module's defs.
    * @param totalEvars     summed `Kind.Eff`  effect-variable counts across the module's defs.
    */
  final case class ModuleStats(module: String, totalNanos: Long, totalCallCount: Long, totalLocLines: Int, byPhase: Map[String, Long], byPhaseCount: Map[String, Long], totalCns: Long, totalTvars: Long, totalEvars: Long) {
    /** Returns the phase that consumed the most time in this module, or None if empty. */
    def dominantPhase: Option[String] =
      if (byPhase.isEmpty) None else Some(byPhase.maxBy(_._2)._1)
  }
}
