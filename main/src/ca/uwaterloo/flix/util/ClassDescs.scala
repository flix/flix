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
package ca.uwaterloo.flix.util

import ca.uwaterloo.flix.language.ast.SourceLocation

import java.lang.constant.ClassDesc

object ClassDescs {

  /**
    * Returns the [[ClassDesc]] of the given loaded class `clazz`.
    *
    * Throws an [[InternalCompilerException]] if `clazz` has no nominal descriptor (e.g. a hidden class).
    */
  def of(clazz: Class[?]): ClassDesc =
    clazz.describeConstable().orElseThrow(() =>
      InternalCompilerException(s"The class '${clazz.getName}' has no nominal descriptor.", SourceLocation.Unknown)
    )

  /**
    * Returns the JVM internal name of the class, interface, or array descriptor `desc`,
    * e.g. `java/lang/String` or `[Ljava/lang/String;`.
    */
  def internalNameOf(desc: ClassDesc): String = {
    if (desc.isArray) {
      // The internal name of an array type is its descriptor.
      desc.descriptorString()
    } else {
      // Strip the leading `L` and trailing `;` of the descriptor.
      val descriptor = desc.descriptorString()
      descriptor.substring(1, descriptor.length - 1)
    }
  }

  /**
    * Returns the class file name (e.g. `java/lang/String.class`) of the class or interface `desc`.
    */
  def classFileNameOf(desc: ClassDesc): String = {
    // Strip the leading `L` and trailing `;` of the descriptor.
    val descriptor = desc.descriptorString()
    descriptor.substring(1, descriptor.length - 1) + ".class"
  }

  /**
    * Returns the binary name (e.g. `java.lang.String`) of the class or interface `desc`.
    */
  def binaryNameOf(desc: ClassDesc): String = {
    // Strip the leading `L` and trailing `;` of the descriptor and replace `/` with `.`.
    val descriptor = desc.descriptorString()
    descriptor.substring(1, descriptor.length - 1).replace('/', '.')
  }

}
