/*
 * Copyright 2026 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.ast.shared

import ca.uwaterloo.flix.language.ast.jvm.JavaField

import java.lang.constant.ClassDesc

object JField {

  /** Returns the [[JField]] of the given class-file field metadata. */
  def of(field: JavaField): JField =
    JField(field.ref.owner, field.ref.name)

}

/**
  * A nominal reference to the Java field `name` declared by the class `owner`.
  *
  * Unlike [[java.lang.reflect.Field]], a [[JField]] does not retain a loaded [[Class]].
  */
case class JField(owner: ClassDesc, name: String)
