/*
 * Copyright 2026 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.ast.shared

import ca.uwaterloo.flix.language.ast.jvm.JavaMethod

import java.lang.constant.{ClassDesc, MethodTypeDesc}

object JConstructor {

  /** Returns the [[JConstructor]] of the given class-file constructor metadata. */
  def of(constructor: JavaMethod): JConstructor =
    JConstructor(constructor.ref.owner, constructor.ref.descriptor)

}

/**
  * A nominal reference to a Java constructor of the class `owner` with the given method type
  * `descriptor` (whose return type is always `void`).
  *
  * Unlike [[java.lang.reflect.Constructor]], a [[JConstructor]] does not retain a loaded [[Class]].
  */
case class JConstructor(owner: ClassDesc, descriptor: MethodTypeDesc)
