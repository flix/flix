/*
 * Copyright 2024 Holger Dal Mogensen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.ast.shared

import ca.uwaterloo.flix.language.ast.SourceLocation

/**
  * Represents a list of derivations with a source location.
  *
  * The source location spans the entire `with X, Y, Z` clause.
  *
  * If there is no `with`-clause then the source location has zero
  * length and is positioned right after the enum type. For example,
  * if the enum is `enum Color {` then the source position would point
  * to the position right after `r` and have zero width.
  */
case class Derivations(traits: List[Derivation], loc: SourceLocation)
