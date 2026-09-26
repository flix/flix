/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.ast.jvm

import java.lang.constant.ClassDesc

/** Identifies the declaration that owns a Java type variable. */
sealed trait JavaTypeVariableOwner

object JavaTypeVariableOwner {
  /** A class or interface declaration that owns a Java type variable. */
  case class Class(owner: ClassDesc) extends JavaTypeVariableOwner

  /** A method or constructor declaration that owns a Java type variable. */
  case class Method(owner: JavaMethodRef) extends JavaTypeVariableOwner

  /**
    * An owner that could not be recovered from the class-file metadata.
    *
    * This is a fallback for class-file parsers that expose a symbolic type variable without its declaring class or
    * method. It should not be used when a declaring source is available because same-named variables with unknown
    * owners compare equal.
    */
  case object Unknown extends JavaTypeVariableOwner
}
