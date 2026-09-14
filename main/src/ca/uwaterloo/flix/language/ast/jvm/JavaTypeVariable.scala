/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.ast.jvm

/**
  * Identifies a Java type variable by its name and declaring class or method.
  *
  * Type-variable names are scoped to their declaration: unrelated declarations can each define `T`, and a method can
  * shadow a class variable named `T`. The owner is therefore part of the identity so these variables remain distinct.
  */
case class JavaTypeVariable(owner: JavaTypeVariableOwner, name: String)
