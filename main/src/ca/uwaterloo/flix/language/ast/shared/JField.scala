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
import java.lang.reflect.Field

object JField {

  /** Returns the [[JField]] of the given reflective `field`. */
  def of(field: Field): JField =
    JField(ClassDescs.of(field.getDeclaringClass), field.getName)

}

/**
  * A nominal reference to the Java field `name` declared by the class `owner`.
  *
  * Unlike [[java.lang.reflect.Field]], a [[JField]] does not retain a loaded [[Class]].
  */
case class JField(owner: ClassDesc, name: String)
