/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.ast.jvm

import java.lang.constant.{ClassDesc, MethodTypeDesc}

/**
  * A nominal, descriptor-based reference to a Java method or constructor.
  *
  * `isInterface` holds whether `owner` is an interface; the JVM distinguishes interface method references.
  */
case class JavaMethodRef(owner: ClassDesc, name: String, descriptor: MethodTypeDesc, isInterface: Boolean)
