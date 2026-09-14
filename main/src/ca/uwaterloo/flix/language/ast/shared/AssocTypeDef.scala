/*
 * Copyright 2024 Holger Dal Mogensen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.ast.shared

import ca.uwaterloo.flix.language.ast.{Type, Symbol}

/**
  * Represents the definition of an associated type.
  * If this associated type is named `Assoc`, then
  * Assoc[arg] = ret.
  */
case class AssocTypeDef(tparams: List[Symbol.KindedTypeVarSym], arg: Type, ret: Type)
