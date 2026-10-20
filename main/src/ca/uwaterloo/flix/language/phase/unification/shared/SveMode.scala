/*
 * Copyright 2026 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.phase.unification.shared

/**
  * The granularity at which successive variable elimination (SVE) is applied to a system of
  * Boolean equations (see [[SveAlgorithm.sveAll]]).
  */
sealed trait SveMode

object SveMode {

  /** Solves the equations one at a time, applying each substitution to the remaining equations. */
  case object PerEquation extends SveMode

  /** Solves each connected component (equations that share a flexible variable) as a single equation. */
  case object PerComponent extends SveMode

  /** Solves the whole system as a single equation. Does not terminate on some programs. */
  case object PerSystem extends SveMode

}
