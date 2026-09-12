/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
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
