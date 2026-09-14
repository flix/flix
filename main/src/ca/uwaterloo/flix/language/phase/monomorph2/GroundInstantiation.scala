/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.language.phase.monomorph2

import ca.uwaterloo.flix.language.ast.Type

/**
  * [[Instantiation]] after solving: `args(i)` is the concrete [[Type]] for the ith
  * type-parameter slot, instead of a possibly-still-symbolic [[MonoArg]].
  */
private[monomorph2] case class GroundInstantiation(args: List[Type])
