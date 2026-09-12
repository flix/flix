/*
 * Copyright 2026 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.ast.shared

import ca.uwaterloo.flix.language.ast.jvm.JavaMethod

import java.lang.constant.{ClassDesc, MethodTypeDesc}

object JMethod {

  /** Returns the [[JMethod]] of the given class-file method metadata. */
  def of(method: JavaMethod): JMethod =
    JMethod(method.ref.owner, method.ref.name, method.ref.descriptor, method.ref.isInterface)

}

/**
  * A nominal reference to the Java method `name` declared by the class or interface `owner`
  * with the given method type `descriptor`.
  *
  * `isInterface` holds whether `owner` is an interface; the JVM needs the distinction to
  * emit method invocations.
  *
  * Unlike [[java.lang.reflect.Method]], a [[JMethod]] does not retain a loaded [[Class]].
  */
case class JMethod(owner: ClassDesc, name: String, descriptor: MethodTypeDesc, isInterface: Boolean)
