/*
 *  Copyright 2022 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.phase.unification.shared

/**
 * An exception thrown to indicate that boolean unification failed.
 */
case class BoolUnificationException() extends RuntimeException
