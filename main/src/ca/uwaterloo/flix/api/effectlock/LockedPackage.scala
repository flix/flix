/*
 * Copyright 2026 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.api.effectlock

import ca.uwaterloo.flix.util.Sha256

/**
  * The signatures that one package is locked at.
  *
  * A signature is recorded by the hash of the scheme it was declared with, see [[HashType]], and
  * is named by the symbol it belongs to, e.g. `Museum.Clerk.sell`.
  *
  * The defs and the sigs of a package are kept apart, so that a def and a sig may share a name
  * without one of them standing for the other.
  *
  * @param defs the hash of each public def of the package.
  * @param sigs the hash of each public sig of the package.
  */
case class LockedPackage(defs: Map[String, Sha256], sigs: Map[String, Sha256])
