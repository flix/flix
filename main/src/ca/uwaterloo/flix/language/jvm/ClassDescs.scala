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
package ca.uwaterloo.flix.language.jvm

import java.lang.constant.ClassDesc

object ClassDescs {

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
    * Returns the binary name of `desc` as [[Class.getName]] would return it,
    * e.g. `java.lang.String`, `int`, or `[Ljava.lang.String;`.
    */
  def binaryNameOf(desc: ClassDesc): String = {
    if (desc.isPrimitive) {
      desc.displayName()
    } else if (desc.isArray) {
      // The binary name of an array type is its descriptor with `/` replaced by `.`.
      desc.descriptorString().replace('/', '.')
    } else {
      // Strip the leading `L` and trailing `;` of the descriptor and replace `/` with `.`.
      val descriptor = desc.descriptorString()
      descriptor.substring(1, descriptor.length - 1).replace('/', '.')
    }
  }

  /**
    * Returns the canonical name of `desc` as [[Class.getCanonicalName]] would return it,
    * e.g. `java.util.Map.Entry` (for `java.util.Map$Entry`), `int`, or `java.lang.String[]`.
    *
    * Local and anonymous classes have no canonical name in Java. For those, this returns the
    * binary name with `$` replaced by `.`.
    */
  def canonicalNameOf(desc: ClassDesc): String = {
    if (desc.isArray) {
      canonicalNameOf(desc.componentType()) + "[]"
    } else {
      binaryNameOf(desc).replace('$', '.')
    }
  }

  /**
    * Returns the simple name of `desc` as [[Class.getSimpleName]] would return it,
    * e.g. `String`, `Entry` (for `java.util.Map$Entry`), `int`, `String[]`,
    * `Local` (for the local class `Outer$1Local`), or the empty string for an anonymous class.
    */
  def simpleNameOf(desc: ClassDesc): String = {
    if (desc.isArray) {
      simpleNameOf(desc.componentType()) + "[]"
    } else {
      // The display name is the unqualified name, e.g. `Map$Entry` for a nested class.
      val name = desc.displayName()
      val idx = name.lastIndexOf('$')
      if (idx < 0 || idx == name.length - 1) {
        name
      } else {
        // Drop the enclosing class and the digits that precede the name of a local class
        // (`Outer$1Local`) or make up the whole name of an anonymous class (`Outer$1`).
        name.substring(idx + 1).dropWhile(c => c >= '0' && c <= '9')
      }
    }
  }

}
