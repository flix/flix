/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.language.phase.monomorph2

/**
  * A component-wise flow constraint: `args` flows into the type-parameter slots of `dst`.
  */
private[monomorph2] case class FlowConstraint(args: Instantiation, dst: MonoVar)
