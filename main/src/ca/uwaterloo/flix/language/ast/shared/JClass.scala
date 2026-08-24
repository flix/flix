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

import java.lang.constant.ClassDesc

object JClass {

  /** Returns the [[JClass]] of the given loaded class `clazz`. */
  def of(clazz: Class[?]): JClass =
    JClass(ClassDescs.of(clazz), clazz.isInterface)

}

/**
  * A nominal reference to the Java class or interface `desc`.
  *
  * `isInterface` holds whether `desc` is an interface; the JVM needs the distinction to
  * decide between extending a superclass and implementing an interface.
  *
  * Unlike [[Class]], a [[JClass]] does not retain a loaded class.
  */
case class JClass(desc: ClassDesc, isInterface: Boolean)
