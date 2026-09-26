/*
 * Copyright 2024 Holger Dal Mogensen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.ast.shared

import ca.uwaterloo.flix.language.ast.Symbol.KindedTypeVarSym
import ca.uwaterloo.flix.language.ast.Type

/**
  * Represents that an instance on type `tpe` has the type constraints `tconstrs`.
  */
case class Instance(tparams: List[KindedTypeVarSym], tpe: Type, tconstrs: List[TraitConstraint], econstrs: List[EqualityConstraint])
