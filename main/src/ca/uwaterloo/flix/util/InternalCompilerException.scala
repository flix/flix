/*
 * Copyright 2015-2016 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.util

import ca.uwaterloo.flix.language.ast.SourceLocation

/**
  * An exception thrown to indicate an internal compiler error.
  *
  * This exception should never be thrown.
  *
  * @param message the error message.
  */
case class InternalCompilerException(message: String, loc: SourceLocation) extends RuntimeException(s"$message ($loc)")
