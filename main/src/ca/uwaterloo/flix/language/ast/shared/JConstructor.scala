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

import ca.uwaterloo.flix.util.ClassDescs

import java.lang.constant.{ClassDesc, ConstantDescs, MethodTypeDesc}
import java.lang.reflect.Constructor

object JConstructor {

  /** Returns the [[JConstructor]] of the given reflective `constructor`. */
  def of(constructor: Constructor[?]): JConstructor = {
    val paramDescs = constructor.getParameterTypes.map(ClassDescs.of)
    val descriptor = MethodTypeDesc.of(ConstantDescs.CD_void, paramDescs: _*)
    JConstructor(ClassDescs.of(constructor.getDeclaringClass), descriptor)
  }

}

/**
  * A nominal reference to a Java constructor of the class `owner` with the given method type
  * `descriptor` (whose return type is always `void`).
  *
  * Unlike [[java.lang.reflect.Constructor]], a [[JConstructor]] does not retain a loaded [[Class]].
  */
case class JConstructor(owner: ClassDesc, descriptor: MethodTypeDesc)
