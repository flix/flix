/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.language.phase.monomorph2

import ca.uwaterloo.flix.language.ast.Symbol

/**
  * The result of constraint solving: for each polymorphic def/enum/struct/restrictable-enum
  * symbol, the set of concrete instantiations it must be specialized at. A restrictable
  * enum's instantiation always starts with its case-set index (`Kind.CaseSet`).
  */
private[monomorph2] case class Solution(
  defs: Map[Symbol.DefnSym, List[GroundInstantiation]],
  enums: Map[Symbol.EnumSym, List[GroundInstantiation]],
  structs: Map[Symbol.StructSym, List[GroundInstantiation]],
  restrictableEnums: Map[Symbol.RestrictableEnumSym, List[GroundInstantiation]]
)
