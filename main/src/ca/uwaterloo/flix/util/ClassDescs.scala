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
import java.lang.constant.ConstantDescs.*

object ClassDescs {

  /**
    * Loads the class represented by `desc` without initializing it.
    */
  def load(desc: ClassDesc, loader: ClassLoader): Class[?] = desc match {
    case CD_boolean => java.lang.Boolean.TYPE
    case CD_byte => java.lang.Byte.TYPE
    case CD_short => java.lang.Short.TYPE
    case CD_char => java.lang.Character.TYPE
    case CD_int => java.lang.Integer.TYPE
    case CD_long => java.lang.Long.TYPE
    case CD_float => java.lang.Float.TYPE
    case CD_double => java.lang.Double.TYPE
    case CD_void => java.lang.Void.TYPE
    case _ if desc.isArray => Class.forName(desc.descriptorString().replace('/', '.'), false, loader)
    case _ => loader.loadClass(binaryNameOf(desc))
  }

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
    * Returns the simple name of `desc` as [[Class.getSimpleName]] would return it,
    * e.g. `String`, `Entry` (for `java.util.Map$Entry`), `int`, or `String[]`.
    */
  def simpleNameOf(desc: ClassDesc): String = {
    if (desc.isArray) {
      simpleNameOf(desc.componentType()) + "[]"
    } else {
      // The display name is the unqualified name, e.g. `Map$Entry` for a nested class.
      val name = desc.displayName()
      val idx = name.lastIndexOf('$')
      if (idx >= 0 && idx < name.length - 1) name.substring(idx + 1) else name
    }
  }

}
