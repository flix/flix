/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.ast.jvm

import java.lang.constant.ClassDesc

/** A Java type as it appears in a generic class-file signature. */
sealed trait JavaType {

  /** Returns the descriptor of this type after Java type erasure. */
  def erasure: ClassDesc

}

object JavaType {

  /** A generic array, such as `T[]`. */
  case class GenericArray(component: JavaType, erasure: ClassDesc) extends JavaType

  /** A primitive, void, array, or non-generic reference type. */
  case class NonGeneric(erasure: ClassDesc) extends JavaType

  /** A parameterized reference type, such as `List<String>`. */
  case class Parameterized(erasure: ClassDesc, arguments: List[JavaType]) extends JavaType

  /** A type-variable use, such as the `T` in `T id(T x)`. */
  case class Variable(variable: JavaTypeVariable, erasure: ClassDesc) extends JavaType

  /** A wildcard, including its class-file upper and lower bounds. */
  case class Wildcard(upperBounds: List[JavaType], lowerBounds: List[JavaType], erasure: ClassDesc) extends JavaType

}
