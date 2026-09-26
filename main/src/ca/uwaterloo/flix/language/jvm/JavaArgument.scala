/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.jvm

import java.lang.constant.ClassDesc

/** The erased Java type of a constructor or method argument. */
sealed trait JavaArgument

object JavaArgument {

  /** An argument with the erased type `desc`. */
  case class Typed(desc: ClassDesc) extends JavaArgument

  /** The Java `null` type, which is assignable to every reference type. */
  case object Null extends JavaArgument

}
