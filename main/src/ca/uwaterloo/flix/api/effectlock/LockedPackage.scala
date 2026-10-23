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
  * is named by the name of its declaration inside the package, e.g. `Museum.Clerk.sell`, see
  * [[EffectLock.nameOf]]. The package itself is not part of the name: the entry the signature is
  * recorded under names it.
  *
  * The defs and the sigs of a package are written in tables of their own for the reader, and
  * for nothing else: a def and a sig cannot share a name in one namespace, so a check compares
  * a locked name against whichever kind the package declares it as now, see
  * [[EffectLock.check]].
  *
  * @param defs the hash of each public def of the package.
  * @param sigs the hash of each public sig of the package.
  */
case class LockedPackage(defs: Map[String, Sha256], sigs: Map[String, Sha256])
