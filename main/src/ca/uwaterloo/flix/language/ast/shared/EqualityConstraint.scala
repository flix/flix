/*
 * Copyright 2024 Holger Dal Mogensen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.ast.shared

import ca.uwaterloo.flix.language.ast.shared.SymUse.AssocTypeSymUse
import ca.uwaterloo.flix.language.ast.{SourceLocation, Type}

/**
  * Represents that `cst[tpe1]` and `tpe2` are equivalent types.
  */
case class EqualityConstraint(symUse: AssocTypeSymUse, tpe1: Type, tpe2: Type, loc: SourceLocation)
