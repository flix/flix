/*
 * Copyright 2026 Flix Authors
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

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.util.Result
import ca.uwaterloo.flix.util.Result.Ok

import java.lang.constant.ClassDesc
import java.lang.constant.ConstantDescs.CD_Object

/**
  * Subtyping of Java types, read from the class-file metadata of the Java type provider.
  */
object JavaHierarchy {

  /**
    * Returns `Ok(true)` if the Java type `sub` is a subtype of the Java type `sup`, `Ok(false)` if it
    * is not, or `Err` if the nominal hierarchy metadata cannot be read.
    *
    * A primitive type is only a subtype of itself. Arrays follow the rules of Java: every array is a
    * subtype of `Object`, `Cloneable`, and `Serializable`, reference components are covariant, and
    * primitive components must be identical. Nominal reference types are delegated to the
    * [[JavaTypeProvider]] since array descriptors have no class-file metadata of their own.
    *
    *   - `String` is a subtype of `CharSequence`.
    *   - `String[]` is a subtype of `Object`, `Cloneable`, `Serializable`, and `Object[]`.
    *   - `int[]` is not a subtype of `long[]` because primitive array components must be identical.
    */
  def isSubtype(sub: ClassDesc, sup: ClassDesc)(implicit flix: Flix): Result[Boolean, JavaLookupError] = {
    if (sub == sup) {
      // Every type is a subtype of itself.
      Ok(true)
    } else if (sub.isPrimitive || sup.isPrimitive) {
      // A primitive type is only a subtype of itself.
      Ok(false)
    } else if (sub.isArray) {
      // Arrays have descriptor-defined supertypes and covariant reference components.
      if (sup == CD_Object || sup == JavaClasses.Cloneable || sup == JavaClasses.Serializable) {
        Ok(true)
      } else if (sup.isArray) {
        isSubtype(sub.componentType(), sup.componentType())
      } else {
        Ok(false)
      }
    } else if (sup.isArray) {
      // A non-array reference type is never a subtype of an array type.
      Ok(false)
    } else {
      // All remaining cases are nominal reference relationships read from class-file metadata.
      flix.javaTypeProvider.isSubtype(sub, sup)
    }
  }

}
