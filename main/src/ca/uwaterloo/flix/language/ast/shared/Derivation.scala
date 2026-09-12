/*
 * Copyright 2024 Holger Dal Mogensen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.ast.shared

import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol}

/**
  * Represents a derivation on an enum (e.g. `enum E with Eq`).
  */
case class Derivation(sym: Symbol.TraitSym, loc: SourceLocation)
