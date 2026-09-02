/*
 * Copyright 2026 Magnus Madsen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package ca.uwaterloo.flix.language.ast.shared

import ca.uwaterloo.flix.language.ast.jvm.JavaMethod

import java.lang.constant.{ClassDesc, MethodTypeDesc}

object JMethod {

  /** Returns the [[JMethod]] of the given class-file method metadata, whose owner is an interface iff `isInterface`. */
  def of(method: JavaMethod, isInterface: Boolean): JMethod =
    JMethod(method.ref.owner, method.ref.name, method.ref.descriptor, isInterface)

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
