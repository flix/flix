/*
 * Copyright 2024 Holger Dal Mogensen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.ast.shared

import ca.uwaterloo.flix.language.ast.{Symbol, TypeHead}

/**
  * Represents the super traits and instances available for a particular traits.
  */
case class TraitContext(superTraits: List[Symbol.TraitSym], instances: Map[TypeHead, Instance])
