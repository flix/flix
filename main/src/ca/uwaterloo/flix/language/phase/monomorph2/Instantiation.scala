/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.language.phase.monomorph2

/**
  * A positional assignment of a [[MonoArg]] to each of a [[MonoVar]]'s type-parameter slots,
  * i.e. in a [[FlowConstraint]], `args(i)` is what flows into `dst`'s ith type-parameter.
  */
private[monomorph2] case class Instantiation(args: List[MonoArg])
