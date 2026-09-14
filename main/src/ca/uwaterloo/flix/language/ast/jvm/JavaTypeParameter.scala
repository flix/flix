/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.ast.jvm

/**
  * A declared Java type parameter and its upper bounds.
  *
  * Java type parameters can declare only upper bounds. Lower bounds occur on wildcard type arguments and are
  * represented by [[JavaType.Wildcard]].
  */
case class JavaTypeParameter(variable: JavaTypeVariable, upperBounds: List[JavaType])
