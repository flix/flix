/*
 * Copyright 2026 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.phase.typer

/**
  * How the declared effect of a def or signature bounds its inferred effect.
  */
sealed trait EffectBound

object EffectBound {

  /** The inferred effect must equal the declared effect. */
  case object Exact extends EffectBound

  /** The inferred effect may be a subeffect of the declared effect (sub-effecting). */
  case object Upper extends EffectBound

}
