/*
 * Copyright 2026 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.phase.typer

/**
  * How the effect of a function bounds the effect of its body: the declared effect of a def or
  * signature, or the effect in the arrow type of a lambda or local def.
  */
sealed trait EffectBound

object EffectBound {

  /** The effect of the body must equal the effect of the function. */
  case object Exact extends EffectBound

  /** The effect of the body may be a subeffect of the effect of the function (sub-effecting). */
  case object Upper extends EffectBound

}
