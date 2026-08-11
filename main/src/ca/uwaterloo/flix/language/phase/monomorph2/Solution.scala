/*
 * Copyright 2026 Simon Lykke Andersen
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

package ca.uwaterloo.flix.language.phase.monomorph2

import ca.uwaterloo.flix.language.ast.{Symbol, Type}

/**
  * The result of constraint solving: for each polymorphic def/enum/struct/restrictable-enum
  * symbol, the set of concrete instantiations it must be specialized at. A restrictable
  * enum's instantiation always starts with its case-set index (`Kind.CaseSet`).
  */
case class Solution(
  defs: Map[Symbol.DefnSym, Set[List[Type]]],
  enums: Map[Symbol.EnumSym, Set[List[Type]]],
  structs: Map[Symbol.StructSym, Set[List[Type]]],
  restrictableEnums: Map[Symbol.RestrictableEnumSym, Set[List[Type]]]
)
