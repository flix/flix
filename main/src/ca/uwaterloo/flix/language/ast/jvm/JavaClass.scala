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
package ca.uwaterloo.flix.language.ast.jvm

import java.lang.constant.ClassDesc

/** Class-file metadata for a Java class or interface. */
case class JavaClass(
  desc: ClassDesc,
  modifiers: Int,
  isRuntimeVisibleAnnotation: Boolean,
  typeParameters: List[JavaTypeParameter],
  superClass: Option[JavaType],
  interfaces: List[JavaType],
  declaredConstructors: List[JavaMethod],
  declaredMethods: List[JavaMethod],
  declaredFields: List[JavaField]
) extends JavaMember {

  /** Returns whether this class-file type is an interface. */
  def isInterface: Boolean = JavaModifiers.has(modifiers, JavaModifiers.ACC_INTERFACE)

  /** Returns whether this class-file type is an annotation. */
  def isAnnotation: Boolean = JavaModifiers.has(modifiers, JavaModifiers.ACC_ANNOTATION)

  /** Returns whether this class declares a constructor without parameters that is not private. */
  def hasNonPrivateZeroArgConstructor: Boolean =
    declaredConstructors.exists(c => c.parameterTypes.isEmpty && !c.isPrivate)

}
